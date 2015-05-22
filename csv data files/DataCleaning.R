#### set directory ####

setwd("C:/Users/117284/Documents/IS 608 final project/All Final Project Files/csv data files")

require(plyr)

#### exploratory analysis ####
# ---- read in 2014 manhattan data
NYRE2014 <- read.csv("manhattan_sales_2014.csv")
nrow(NYRE2014)
#change price format to remove $ and , and change it to numeric
NYRE2014$SALE.PRICE<-as.numeric(gsub("[[:punct:]]", "", as.matrix(NYRE2014$SALE.PRICE)))
#change all 0 prices to NA so they don't affect the median, these are not valid entries
is.na(NYRE2014$SALE.PRICE) <- !NYRE2014$SALE.PRICE

#######

#### clean 2014 data file, use as model for remaining files ####
# ---- extract rows with coop and condo sale categories
NYRE2014$CategoryNo <- substr(NYRE2014$BUILDING.CLASS.CATEGORY,1,2)
NYRE2014$count <- 1
chkcat <- data.frame(NYRE2014$BUILDING.CLASS.CATEGORY,NYRE2014$CategoryNo)
unique(chkcat)
aggregate(count ~ CategoryNo, data = NYRE2014, sum)

# ---check category 15  CONDOS - 2-10 UNIT RESIDENTIAL for number of units in each sale
Category <- as.data.frame(summary(NYRE2014$BUILDING.CLASS.CATEGORY))
cat15 <- NYRE2014[NYRE2014$CategoryNo == "15",]
summary(cat15)
#appears to be only one unit for all entries in this category

# --- check if any residential in 16  CONDOS - 2-10 UNIT WITH COMMERCIAL UNIT
cat16 <- NYRE2014[NYRE2014$CategoryNo == "16",]
summary(cat16)
#these are all commerical units

#include only categories: 9,10,12,13,15,17
#09  COOPS - WALKUP APARTMENTS 
#10  COOPS - ELEVATOR APARTMENTS 
#12  CONDOS - WALKUP APARTMENTS 
#13  CONDOS - ELEVATOR APARTMENTS 
#15  CONDOS - 2-10 UNIT RESIDENTIAL
#17  CONDO COOPS  

#create data set for 2014
NYRE2014$CategoryNo <- substr(NYRE2014$BUILDING.CLASS.CATEGORY,1,2)
NYRE2014$count <- 1
CN14<-NYRE2014$CategoryNo
Data2014 <- NYRE2014[CN14 == "09"|CN14=="10"|CN14=="12"|CN14=="13"|CN14=="15"|CN14=="17",]
Data2014<-data.frame(Data2014[c(1,2,3,11,17,20,21,22,23)])
colnames(Data2014)=c("BOROUGH","NEIGHBORHOOD","CATEGORY","ZIPCODE",
                     "YEAR_BUILT","SALE_PRICE","SALE_DATE","Category_No","Count")

#create a data frame for zipcodes/neighborhoods that has columns for total, coop/condo/condop, price/volume

#neighborhoods
#total residential sales price and volume
n_volume <- setNames(aggregate(Count ~ NEIGHBORHOOD, data = Data2014, sum),c("Neighborhood","V"))
n_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ NEIGHBORHOOD, data = Data2014, median),c("Neighborhood","P"))

#coop sales price and volume
#condo sales price and volume
#condop sales price and volume

#create coop, condo, and condop data frames
DataCN<-Data2014$Category_No
coop <- Data2014[DataCN == "09"|DataCN=="10",]
condo <- Data2014[DataCN == "12"|DataCN=="13"|DataCN=="15",]
condop <- Data2014[DataCN == "17",]

#compute coop, condo, and condop price and volume
n_cp_volume <- setNames(aggregate(Count ~ NEIGHBORHOOD, data = coop, sum),c("Neighborhood","V-Coop"))
n_cp_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ NEIGHBORHOOD, data = coop, median),c("Neighborhood","P-Coop"))

n_cn_volume <- setNames(aggregate(Count ~ NEIGHBORHOOD, data = condo, sum),c("Neighborhood","V-Condo"))
n_cn_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ NEIGHBORHOOD, data = condo, median),c("Neighborhood","P-Condo"))

n_cnp_volume <- setNames(aggregate(Count ~ NEIGHBORHOOD, data = condop, sum),c("Neighborhood","V-Condop"))
n_cnp_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ NEIGHBORHOOD, data = condop, median),c("Neighborhood","P-Condop"))


n_stats14 <- join_all(list(n_volume,n_price,n_cp_volume,n_cp_price,n_cn_volume,n_cn_price,n_cnp_volume,n_cnp_price),by="Neighborhood")


#zipcodes
#total residential sales price and volume
z_volume <- setNames(aggregate(Count ~ ZIPCODE, data = Data2014, sum),c("Zipcode","V"))
z_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ ZIPCODE, data = Data2014, median),c("Zipcode","P"))

z_cp_volume <- setNames(aggregate(Count ~ ZIPCODE, data = coop, sum),c("Zipcode","V-Coop"))
z_cp_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ ZIPCODE, data = coop, median),c("Zipcode","P-Coop"))

z_cn_volume <- setNames(aggregate(Count ~ ZIPCODE, data = condo, sum),c("Zipcode","V-Condo"))
z_cn_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ ZIPCODE, data = condo, median),c("Zipcode","P-Condo"))

z_cnp_volume <- setNames(aggregate(Count ~ ZIPCODE, data = condop, sum),c("Zipcode","V-Condop"))
z_cnp_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ ZIPCODE, data = condop, median),c("Zipcode","P-Condop"))

z_stats14 <- join_all(list(z_volume,z_price,z_cp_volume,z_cp_price,z_cn_volume,z_cn_price,z_cnp_volume,z_cnp_price),by="Zipcode")



#% change vs 2003 in volume and price (without condo/coop breakdown)  - need all years to compute this

# create a function to create processed data frames for all years ####
#create a vector of years
years<-c(2003:2014)

#create dataframes and subset the data to get just the relevant data
dfcreate <-function(x){
  #generate the file name to import
  fname<-paste0("manhattan_sales_",x,".csv")
  df <- read.csv(fname)
  #change price format to remove $ and , and change it to numeric
  df$SALE.PRICE<-as.numeric(gsub("[[:punct:]]", "", as.matrix(df$SALE.PRICE)))
  #change all 0 prices to NA so they don't affect the median, these are not valid sale prices
  is.na(df$SALE.PRICE) <- !df$SALE.PRICE
  #extract the category number as a string to a separate column
  df$CategoryNo <- substr(df$BUILDING.CLASS.CATEGORY,1,2)
  #add a column of 1's for counting (calculating sales volume)
  df$count <- 1
  #give the category number column a short alias so it's easier to use
  CN<-df$CategoryNo
  #extract only the rows with categories that are condo,coop, or condop residential sales
  #as determined above
  newdf <- df[CN == "09"|CN=="10"|CN=="12"|CN=="13"|CN=="15"|CN=="17",]
  newdf<-data.frame(newdf[c(1,2,3,11,17,20,21,22,23)])
  colnames(newdf)=c("BOROUGH","NEIGHBORHOOD","CATEGORY","ZIPCODE",
                       "YEAR_BUILT","SALE_PRICE","SALE_DATE","Category_No","Count")
  return(newdf) 
}

#create a processed data frames for each year, and a vector with their names
n=length(years)

for (i in 1:n) {
  assign( paste0("Data", years[i]), dfcreate(years[i]) )
}


#Create a function to aggregate data by price, volume and category by neighborhood and zipcode ####

aggregate_fct <- function(df,year) {
  
  #create coop, condo, and condop data frames
  DataCN<-df$Category_No
  coop <- df[DataCN == "09"|DataCN=="10",]
  condo <- df[DataCN == "12"|DataCN=="13"|DataCN=="15",]
  condop <- df[DataCN == "17",]
  
  #neighborhoods
  #total residential sales price and volume
  n_volume <- setNames(aggregate(Count ~ NEIGHBORHOOD, data = df, sum),c("Neighborhood",paste0("V-",year)))
  n_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ NEIGHBORHOOD, data = df, median),c("Neighborhood",paste("P-",year)))
  
  #compute coop, condo, and condop price and volume
  n_cp_volume <- setNames(aggregate(Count ~ NEIGHBORHOOD, data = coop, sum),c("Neighborhood",paste0("V-Coop-",year)))
  n_cp_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ NEIGHBORHOOD, data = coop, median),c("Neighborhood",paste0("P-Coop-",year)))
  
  n_cn_volume <- setNames(aggregate(Count ~ NEIGHBORHOOD, data = condo, sum),c("Neighborhood",paste0("V-Condo-",year)))
  n_cn_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ NEIGHBORHOOD, data = condo, median),c("Neighborhood",paste0("P-Condo-",year)))
  
  n_cnp_volume <- setNames(aggregate(Count ~ NEIGHBORHOOD, data = condop, sum),c("Neighborhood",paste0("V-Condop-",year)))
  n_cnp_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ NEIGHBORHOOD, data = condop, median),c("Neighborhood",paste0("P-Condop-",year)))
  
  n_stats <- join_all(list(n_volume,n_price,n_cp_volume,n_cp_price,n_cn_volume,
                           n_cn_price,n_cnp_volume,n_cnp_price),by="Neighborhood")
  
  #zipcodes
  #total residential sales price and volume
  z_volume <- setNames(aggregate(Count ~ ZIPCODE, data = df, sum),c("Zipcode",paste0("V-",year)))
  z_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ ZIPCODE, data = df, median),c("Zipcode",paste0("P-",year)))
  
  z_cp_volume <- setNames(aggregate(Count ~ ZIPCODE, data = coop, sum),c("Zipcode",paste0("V-Coop-",year)))
  z_cp_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ ZIPCODE, data = coop, median),c("Zipcode",paste0("P-Coop-",year)))
  
  z_cn_volume <- setNames(aggregate(Count ~ ZIPCODE, data = condo, sum),c("Zipcode",paste0("V-Condo-",year)))
  z_cn_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ ZIPCODE, data = condo, median),c("Zipcode",paste0("P-Condo-",year)))
  
  z_cnp_volume <- setNames(aggregate(Count ~ ZIPCODE, data = condop, sum),c("Zipcode",paste0("V-Condop-",year)))
  z_cnp_price <- setNames(aggregate(round(SALE_PRICE/1000,-1) ~ ZIPCODE, data = condop, median),c("Zipcode",paste0("P-Condop-",year)))
  
  z_stats <- join_all(list(z_volume,z_price,z_cp_volume,z_cp_price,z_cn_volume,
                           z_cn_price,z_cnp_volume,z_cnp_price),by="Zipcode")
 
  return(list(n_stats,z_stats))
}

#test
#agg2003<-aggregate_fct(Data2003,"2003")
#n_stats_03 <- data.frame(agg2003[1])
#z_stats_03 <- data.frame(agg2003[2])

#return a neighborhood and zipcode data frame for each year ####

for (i in 1:n) {
  print( paste0("Data", years[i]) )
  get(paste0("Data", years[i]))
  assign( paste0("agg", years[i]), aggregate_fct(get(paste0("Data", years[i])),years[i] ))
  assign( paste0("n_stats_", years[i]),data.frame(get(paste0("agg", years[i]))[1]) )
  assign( paste0("z_stats_", years[i]), data.frame(get(paste0("agg", years[i]))[2]) )
}


#combine all years into one neighborhood data frame and one zipcode data frame ####
n_names <-as.character() 
z_names <- as.character() 


for (i in 1:n) {
  n_names <- c(n_names,paste0("n_stats_", years[i]))
}


n_stats_all <- join_all(list(n_stats_2003,n_stats_2004,n_stats_2005,n_stats_2006,n_stats_2007, 
                        n_stats_2008,n_stats_2009,n_stats_2010,n_stats_2011,n_stats_2012,
                        n_stats_2013,n_stats_2014),by="Neighborhood")

z_stats_all <- join_all(list(z_stats_2003,z_stats_2004,z_stats_2005,z_stats_2006,z_stats_2007, 
                             z_stats_2008,z_stats_2009,z_stats_2010,z_stats_2011,z_stats_2012,
                             z_stats_2013,z_stats_2014),by="Zipcode")

#write files to csv to be used as primary data source #####

#n_stats_all file produces and extra "." after the P for P.2003, P.2004, etc,
#I'm not sure why or how to fix, but for now manually delete the extra period out
#of these column headings in the csv
write.csv(n_stats_all, file = "n_stats_all.csv",quote=FALSE,row.names=FALSE)

#z_stats_all file has a blank zipcode row at the top that should be deleted from the csv
write.csv(z_stats_all, file = "z_stats_all.csv",quote=FALSE,row.names=FALSE)
  


