#create html heat maps with leafletR

#setwd("D:/CUNY Files/IS 608/App-3")
setwd("C:/Users/117284/Documents/IS 608 final project/All Final Project Files/create maps")

#the following libraries need to be added to shiny app for this source file to run
library(RColorBrewer)
library(leafletR)

#geojson file directory
leafdat<-"Manhattan2.geojson"

#import subdatM2 data file
data <- read.csv("subdatM2.csv")
#colnames(data)

#only looking at price for the heatmap
#create a zipcode data file with price fields only
z_data <-read.csv("z_stats_all.csv")
col<-ncol(z_data)/2

rangep <- c(1:col)*2+1
rangep <- c(1,rangep)
cuts_data <- z_data[,rangep]

#has only price fields, created in Excel from z_data_all file which produced 
#the subdatM2 file

#compute cuts for all years of data - *2013 data removed
m<-ncol(cuts_data)
cuts<-round(quantile(cuts_data[,2:m], probs = seq(0, 1, 0.20), na.rm = T), 0)
cuts[1]<-0 # ----- for this example make first cut zero


#testing
#BType <- "All"
#Year <- 2014

#set color palate
pal <- brewer.pal(5, "Reds")

#create a function to generate maps
leafmap_gen <- function(Year,BType){

  #convert All to blank for column name
  if(BType=='All'){
    b <- "." } else {
    b <- paste0(".",BType,".")
  }
  
  P.BT.Year <- paste0("P",b,Year)
  
  # ----- Create the cuts
  #cuts<-round(quantile(data[,P.BT.Year], probs = seq(0, 1, 0.20), na.rm = T), 0)
  #cuts[1]<-0 # ----- for this example make first cut zero
  #cuts were calculated on all years of data
  
  popup<-c("POSTAL",P.BT.Year)
  #changing these popup labels seem to require going back and changing the 
  #original colum names and then recreate the z_data files and then the geojson
  
  #pal <- brewer.pal(5, "Reds")
  
  sty<-styleGrad(prop=P.BT.Year, breaks=cuts, right=FALSE, style.par="col",
                   style.val=pal, leg=paste(Year," Median Price ($ in 000s)"), lwd=1)
  
  # ----- Create the map and load into browser
  map<-leaflet(data=leafdat,style=sty,
                title="Manhattan Real Estate Prices", base.map="osm",
                incl.data=TRUE,  popup=popup)
  
  #copy file to html_maps directory
  oldfname<-"Manhattan_Real_Estate_Prices/Manhattan_Real_Estate_Prices.html"
  newfname<-paste0("html_maps","/",paste0("MedPrice",BType,Year),".html")
    
  file.copy(oldfname, newfname,overwrite = T)
  
  # ----- to look at the map you can use this code
  #browseURL(map)
}


#use a for loop to generate the column name and loop each building type and year
#to create a heat map for each one.

#years
years <- c(2003:2014)
nyears<-length(years)

#building type
btype<-c("All","Condo","Coop","Condop")
nbtype<-length(btype)

#test function
#Year=2004
#BType="All"
#leafmap_gen(Year,BType)

for (i in 1:nyears) {
  for (j in 1:nbtype){
    Year <- years[i]
    BType <- btype[j]
    #print(paste0(Year,BType))
    leafmap_gen(Year,BType)
  }
}

