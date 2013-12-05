library(maptools)
library(ncdf)
library(sp)
library(ggplot2)
library(rgeos)
library(maps)
gpclibPermit()
Sys.setenv(LANG = "en")

path = "/Users/huziy/PythonProjects/basemap/examples/wm201_Arctic_JJA_1990-2008_moyenneDesMoyennes.nc"
nc = open.ncdf(path)
print(nc)

var_name = "preacc"
preacc = get.var.ncdf( nc, var_name)
lons = get.var.ncdf(nc, "lon")
lats = get.var.ncdf(nc, "lat")

#print(lons)
#filled.contour(preacc)

#coordinates(preacc)= ~lon+lat
#create dataframe object

lons_flat = lons
lats_flat = lats
preacc_flat = preacc

dim(lons_flat) = c(length(lons_flat),1)
dim(lats_flat) = c(length(lats_flat),1)
dim(preacc_flat) = c(length(lats_flat),1)

proj = "+o_proj=longlat"
df = data.frame(z=preacc_flat, lons = lons_flat, lats = lats_flat)
#coordinates(df) = c("lons", "lats")
#gridded(df) = TRUE

coords = cbind(lons_flat, lats_flat)
print(dim(coords))
test <- SpatialPointsDataFrame(coords=SpatialPoints(coords=coords), data=df, proj4string=proj)

map('usa')
plot(test, add=TRUE)

