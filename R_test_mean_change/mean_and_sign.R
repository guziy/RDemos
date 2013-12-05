
#clear workspace
rm(list = ls(all = TRUE))


#n_members (= 5) x n_years (= 30) x n_points ( = 547)
n_members = 5
n_years = 30
n_points = 547
n_samples = 1000
current_data <- array(0, dim = c(n_members, n_years, n_points))
future_data <- array(0, dim = c(n_members, n_years, n_points))

#read data from files
for (i in 1:n_members){
	#print(i)
	current_data[i,,] <- t(read.csv(file=sprintf("c_%d.csv", i - 1), sep=',', header=F))
	future_data[i,,] <- t(read.csv(file=sprintf("f_%d.csv", i - 1), sep=',', header=F))
}


library(stats)
merged_current_data = array(0, dim = c(n_members * n_years, n_points))
merged_future_data = array(0, dim = c(n_members * n_years, n_points))

start = 1
end = n_years
for (m in 1:n_members){
	merged_current_data[start:end,] = current_data[m,, ]
	merged_future_data[start:end,] = future_data[m,, ]
	start = start + n_years
	end = end + n_years
}

####Wilcoxon test
p_values = array(0, dim=c(n_points))

for (p in 1:n_points){
	x = wilcox.test( merged_current_data[, p],  merged_future_data[, p])
	p_values[p] = x$p.value	
}

significant = sum( p_values <= 0.05 )
print("Wilcoxon test:")
print(significant)

###Ttest
for (p in 1:n_points){
	x = t.test( merged_current_data[, p], y = merged_future_data[, p])
	p_values[p] = x$p.value	
}

significant = sum( p_values <= 0.05 )
print("t-test:")
print(significant)
##############################################################
### Bootstrap for merged (allowing exchange between members) #
##############################################################
n_ym = n_members * n_members
boot_merged_allow_current = array(0, dim=c(n_samples, n_points))
boot_merged_allow_future = array(0, dim=c(n_samples, n_points))
#current
for (s in 1:n_samples){
	idx = sample(1:n_ym, n_ym, replace = T)
	boot_merged_allow_current[s,] = apply( merged_current_data[idx, ], 2, mean )
}
 
#future
for (s in 1:n_samples){
	idx = sample(1:n_ym, n_ym, replace = T)
	boot_merged_allow_future[s,] = apply( merged_future_data[idx, ], 2, mean )
}

c_mean = apply( merged_current_data, 2, mean )
f_mean = apply( merged_future_data, 2, mean )
c_boot_std = apply( boot_merged_allow_current, 2, sd )
f_boot_std = apply( boot_merged_allow_future, 2, sd )

significant = sum( abs(f_mean - c_mean) >= 1.96 * (c_boot_std + f_boot_std) )
print("bootstrap allowing exchange between members:")
print(significant)




#### Merged bootstrap (do not allow the exchange between members)
boot_mean_current = array(0, dim = c(n_members, n_samples, n_points))
boot_mean_future = array(0, dim = c(n_members, n_samples, n_points))

#do bootstrapping
#current
for(member in 1:n_members){
	#print(member)
	for (s in 1:n_samples){
		idx = sample(1:n_years, n_years, replace = T)
		boot_mean_current[member, s,] = apply( current_data[member, idx, ], 2, mean )
	}
} 
#future
for(member in 1:n_members){
	#print(member)
	for (s in 1:n_samples){
		idx = sample(1:n_years, n_years, replace = T)
		boot_mean_future[member, s,] = apply( future_data[member, idx, ], 2, mean )
	}
} 


#merge dimensions of members and samples to take standard deviation
mean_current_merged = array(0, dim = c( n_samples * n_members, n_points))
mean_future_merged = array(0, dim = c( n_samples * n_members, n_points))
start = 1
end = n_samples
for (m in 1:n_members){
	mean_current_merged[start:end,] = boot_mean_current[m,, ]
	mean_future_merged[start:end,] = boot_mean_future[m,, ]
	start = start + n_samples
	end = end + n_samples
}


stds_current = apply( mean_current_merged, 2, sd)
stds_future = apply( mean_future_merged, 2, sd)

current_mean <- apply( current_data, 3 , mean )
future_mean <- apply( future_data, 3, mean )


#print(length( future_mean ))
change = future_mean - current_mean
x = abs(change) >= 1.96 * (stds_current + stds_future)
print("Bootstrap for merged ensemble (not allowing exchange between members):")
print(sum(x)) 
