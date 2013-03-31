##working through the tutorial at
##http://math.illinoisstate.edu/dhkim/rstuff/rtutor.html

x <- c(1,3,45,6,78)
y <- 2^x
print(y)

print( x^3 > y )

#bind columns
m1 <- cbind(x, y)
print(m1)
print(dim( m1 ))
print(t(m1))

print(m1[1,])
print(m1[2,])

sq_matrix <- m1[1:2,1:2]
print(sq_matrix)

#inverse of a matrix
print( solve(sq_matrix) )

#finding roots
y.fun<-function (x) {
  y<-(log(x))^2-x*exp(-x^3) 
}

root.fun<- function () {      
  x<-seq(0.2,2,0.01) 
  y<-y.fun(x) 
  #dev.new() 
  plot(x,y,type="l") 
  abline(h=0) 
  r1 <- uniroot(y.fun,lower=0.2,upper=1)$root 
  r2 <- uniroot(y.fun,lower=1,upper=2)$root 
  cat("Roots : ", round(r1,4), "  ", round(r2,4),"\n") 
}
root.fun()
