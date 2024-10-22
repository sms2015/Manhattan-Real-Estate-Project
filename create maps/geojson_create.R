#helpsource: http://zevross.com/blog/2014/04/11/using-r-to-quickly-create-an-interactive-online-map-using-the-leafletr-package/

setwd("D:/CUNY Files/IS 608/final project/All Final Project Files/create maps")

library(leafletR)
library(rgdal) #for reading/writing geo files
library(rgeos) #for simplification
library(sp)
library(plyr)
require(RColorBrewer)

#read in the csv real estate data file ####
z_stats <- read.csv("z_stats_all.csv",header=T)
#change the zipcode column name to POSTAL for later join function
colnames(z_stats)[1] <- "POSTAL"

#open NYC shape files
downloaddirM <- "C:/Users/117284/Documents/IS 608 final project/All Final Project Files/create maps/shapefile"
#downloaddirM <- "D:/CUNY Files/IS 608/final project/All Final Project Files/create maps/shapefile"
filenameM <- "PostalBoundary"
datM<-readOGR(downloaddirM,filenameM) 

# ----- Create a subset of Manhattan zip codes
subdatM<-datM[datM@data$NAME == "New York",]

row.names(subdatM@data)

#combine csv data into shapefile datafile  ####
#join the real estate data to subdatM2@data,create full real estate data file that
#contains all data that will be shown on this map and then join by zipcode field - POSTAL
#subdatM2 may have to be exported to it's own file for this to be put online
subdatM2 <- subdatM
subdatM2@data
subdatM2@data <- join(subdatM@data,z_stats,by="POSTAL")

#set the row numbering for subdatM2 back to rows for subdatM
row.names(subdatM@data)
row.names(subdatM2@data)=row.names(subdatM@data)

#check that columns have been added to data
dim(subdatM@data)
dim(subdatM2@data)
subdatM2@data[1:5,]

summary(subdatM)
summary(subdatM2)

# ----- Transform to EPSG 4326 - WGS84 (required)
#subdatM<-spTransform(subdatM, CRS("+init=epsg:4326"))
#This isn't necessary for the zip code file, messes up the boundary lines

# ----- save the data slot
subdatM_data<-subdatM@data
subdatM2_data<-subdatM2@data

# ----- simplification yields a SpatialPolygons class
subdatM<-gSimplify(subdatM,tol=0.01, topologyPreserve=TRUE)
subdatM2<-gSimplify(subdatM2,tol=0.01, topologyPreserve=TRUE)

# ----- to write to geojson we need a SpatialPolygonsDataFrame
subdatM<-SpatialPolygonsDataFrame(subdatM, data=subdatM_data)
subdatM2<-SpatialPolygonsDataFrame(subdatM2, data=subdatM2_data)

write.csv(subdatM2,"subdatM2.csv")

summary(subdatM)
summary(subdatM2)

#write data to geojson
leafdatM<-paste(downloaddirM, "/", "Manhattan", ".geojson", sep="") 
summary(subdatM)
GeoJSONM <- toGeoJSON(data = subdatM, dest = downloaddirM, name = "Manhattan")

leafdatM2<-paste(downloaddirM, "/", "Manhattan2", ".geojson", sep="") 
summary(subdatM)
GeoJSONM2 <- toGeoJSON(data = subdatM2, dest = downloaddirM, name = "Manhattan2")
#writeOGR(datM, leafdatM, layer="", driver="GeoJSON")

# ----- Create the cuts
cutsM2<-round(quantile(subdatM2$P.2014, probs = seq(0, 1, 0.20), na.rm = T), 0)
cutsM2[1]<-0 # ----- for this example make first cut zero

# ----- Fields to include in the popup
colnames(subdatM2@data)

popupM2<-c("POSTAL","P.2014")

pal <- brewer.pal(5, "Reds")

styM2<-styleGrad(prop="P.2014", breaks=cutsM2, right=FALSE, style.par="col",
               style.val=pal, leg="2014 Median Price ($ in 000s)", lwd=1)


# ----- Create the map and load into browser
mapM<-leaflet(data=leafdatM2, dest=downloaddirM, style=styM2,
             title="Manhattan Real Estate Prices", base.map="osm",
             incl.data=TRUE,  popup=popupM2)

# ----- to look at the map you can use this code
browseURL(mapM)
