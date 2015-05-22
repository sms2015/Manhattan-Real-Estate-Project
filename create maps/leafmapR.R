#create a source file for the map

#can't use leaflet and leafletR have to choose, leafletR does not integrate with shiny
#see leaflet shiny


#the following libraries need to be added to shiny app for this source file to run
library(RColorBrewer)
library(leafletR)

#geojson file directory
leafdatM2<-"Manhattan2.geojson"

#import subdatM2 data file
subdatM2 <- read.csv("subdatM2.csv")

# ----- Create the cuts
cutsM2<-round(quantile(subdatM2$P.Condo.2014, probs = seq(0, 1, 0.20), na.rm = T), 0)
cutsM2[1]<-0 # ----- for this example make first cut zero

# ----- Fields to include in the popup
# I think will ultimately use the shiny inputs here, will need to edit for that


#colnames(subdatM2)

popupM2<-c("POSTAL","P.Condo.2014")

pal <- brewer.pal(5, "Reds")


a<-2014
styM2<-styleGrad(prop="P.Condo.2014", breaks=cutsM2, right=FALSE, style.par="col",
                 style.val=pal, leg=paste(a," Median Price ($ in 000s)"), lwd=1)


# ----- Create the map and load into browser
mapM<-leaflet(data=leafdatM2,style=styM2,
              title="Manhattan Real Estate Prices", base.map="osm",
              incl.data=TRUE,  popup=popupM2)

oldfname<-"Manhattan_Real_Estate_Prices/Manhattan_Real_Estate_Prices.html"
newfname<-paste0("html_maps","/","MedPriceAll2014",".html")
  
file.copy(oldfname, newfname,overwrite = T)

# ----- to look at the map you can use this code
browseURL(mapM)

#setwd("D:/CUNY Files/IS 608/App-3")
