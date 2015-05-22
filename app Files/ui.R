#ui

library(shiny)
library(ggvis)

#import neighborhood stats and zipcode stats csv files
n_data<-read.csv("n_stats_all.csv")  #neighborhood stats

#read in map related files
leafdatM2<-"Manhattan2.geojson"

shinyUI(fluidPage(
  titlePanel("Manhattan Residential Real Estate 2003 - 2014"),
    mainPanel(
      tabsetPanel(
        tabPanel("Main",
          img(src="Manhattan.png", height = 300, width = 532),
          
          h3("Introduction"),
          p("There is currently a significant focus on affordable housing in New York City, including the high cost of housing and housing cost increases. This app provides interactive charts and maps of residential real estate sales price trends for co-op, condo, and cond-op sales in Manhattan by neighborhood and zipcode for the years 2003 through 2014.  (A cond-op is a condo/co-op hybrid).  Note that data on interior square footage and number of bedrooms was not available in the underlying data set, so this level of detail is not available in this app.  Data for years before 2003 is also not publicly available in the data source."), 
          p("This app was created for a data visualization class for the CUNY M.S. in Data Analytics program.  These visualizations are based on publicly available data from the New York City Department of Finance, and shapefiles from the New York State GIS Clearinghouse.  The shapefiles were turned into geojson files using leafletR in RStudio."),
          p("Data URL: http://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page"),
          p("Shapefiles URL: https://gis.ny.gov/gisdata/inventories/details.cfm?DSID=934 "),
          p("The years 2003 through 2014 encompassed the housing boom, bust, and ongoing recovery.   The charts in this app explore changes and trends during these years."),
          
          h3("About"),
          p("This app consists of 5 tabs: Main, Scatter Plot, Bar Chart, Percent Change, and Heat Maps. Each tab contains a drop down to select a building type (Condo/Co-op/Condop/All) and a slider to select a year (2003 -2014). All prices are Median Prices for the selected Year and Building Category."),
          p("The Scatter Plot displays Price vs. Volume information by neighborhood. Hovering over a dot in the scatter plot will display the Neighborhood name."),
          p("The Bar Chart displays Median Sales Price by Neighborhood for the selected year and building type ordered by 2003 Median Prices.  2003 Median prices are displayed as a red dot for comparison."),
          p("The Percent Change displays percent change in Median Price between 2003 and the selected Year, ordered by percent change from highest to lowest."),
          p("The Heat Maps displays Median Price by zipcode. There are fixed price ranges for all years to make it easy to see the changes between years and building types.  Clicking on a color coded area of the map will provide information on the zip code and Median Price."),
          
                    
          h3("Observations"),          
          p("It is a known trend that condo prices in Manhattan are typically higher than co-op prices.  These charts strongly support this trend: it can be clearly seen for all years between 2003 and 2014, including the housing boom, bust, and recovery.  The trend holds true for both the dollar value increase and percent change.  As an example, in 2014, several neighborhoods show 200-300% increases for median condo sales prices vs. 2003, while the highest percent increase for co-ops is 200%.  The heat maps bear out this same trend."),
          p("Another observable trend is the housing market cycle. The market contraction in both price and volume for all neighborhoods can be clearly seen in the scatter plot for 2009 as compared to 2007 and 2008.  While 2014 prices have recovered and increased for most neighborhoods by 2014 vs. 2007, volume  still remains below 2007 levels for most neighborhoods which is likely driving some of the high prices.  In fact, neighborhoods with the highest median prices tend to have low volume, although the converse is not always true (areas with lower volume do not always have higher prices).  Finally, relative price levels between neighborhoods have shifted somewhat over the 11 year period. "),
          
          h3("Outliers"),
          p("Outliers removed from data by Neighborhood ($ in 000s):"),
          p("HARLEM-CENTRAL Median Price, Condop, 2010, $28,920 |     
            WASHINGTON HEIGHTS UPPER, Median Price, Condop, 2013,$11,000 |    
            HARLEM-EAST, Median Price, Condop, 2003, $8,750 |
            FASHION, Median Price, Condop, 2008, $8,640 |    
            SOUTHBRIDGE, Median Price, Coop, 2011,$8,605 |
            SOUTHBRIDGE, Median Price, Coop, 2006.$8,120  | 
            MIDTOWN CBD, Median Price, Condop, 2005		$5,500")
          
        ),
        
        tabPanel("Scatter Plot",                 
                 sidebarLayout(
                   sidebarPanel(
                     helpText("Select building type"),
                     selectInput("buildtype", 
                                 label = "Choose a variable to display",
                                 choices = list("Co-op", "Condo","Condop","All"),
                                 selected = "All"),
                     helpText("Select Year"),
                     sliderInput("integer",
                                 label = "Select Year:",
                                 min = 2003, max = 2014, value = 2014,sep = "",tick=F)
                   ),
                   ggvisOutput("plot"))
        ),
        
        tabPanel("Bar Chart",
                 sidebarLayout(
                   sidebarPanel(
                     helpText("Select building type"),
                     selectInput("buildtype2", 
                                 label = "Choose a variable to display",
                                 choices = list("Co-op", "Condo","Condop","All"),
                                 selected = "All"),
                     helpText("Select Year"),
                     sliderInput("integer2",
                                 label = "Select Year:",
                                 min = 2003, max = 2014, value = 2014,sep = "",tick=F)
                   ),         
                   plotOutput("plot2"))
        ),
        
        tabPanel("Percent Change",
                 sidebarLayout(
                   sidebarPanel(
                     helpText("Select building type"),
                     selectInput("buildtype3", 
                                 label = "Choose a variable to display",
                                 choices = list("Co-op", "Condo","Condop","All"),
                                 selected = "All"),
                     helpText("Select Year"),
                     sliderInput("integer3",
                                 label = "Select Year:",
                                 min = 2003, max = 2014, value = 2014,sep = "",tick=F)
                   ),         
                   plotOutput("plot3"))
        ),
        
        
        tabPanel("Heat Maps",
                 sidebarLayout(
                   sidebarPanel(
                     helpText("Select building type"),
                     selectInput("buildtype4", 
                                 label = "Choose a variable to display",
                                 choices = list("Co-op", "Condo","Condop","All"),
                                 selected = "All"),
                     helpText("Select Year"),
                     sliderInput("integer4",
                                 label = "Select Year:",
                                 min = 2003, max = 2014, value = 2014,sep = "",tick=F)
                   ),
                   
                   htmlOutput("Map1")
                   
                   )
                 
        ) 
      )
    )
))