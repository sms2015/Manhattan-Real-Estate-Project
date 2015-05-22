#server

library(shiny)
library(ggplot2)
library(ggvis)
library(scales) 

#read in processed data tables
n_data<-read.csv("n_stats_all.csv")  #neighborhood stats
n_data$id <- 1:nrow(n_data)  # Add an id column to use as the key for ggvis tooltip

options(shiny.error=browser)

shinyServer(function(input, output) {
  
  main_plot <- reactive({ 
    
    #get year and building type inputs
    yr_in <- input$integer
    
    build_in <- input$buildtype
    
    #testing
    #yr_in <- 2003
    #build_in <- "Condop"
    
    if (build_in == "Co-op"){
      build_in <- "Coop"
    }
    
    #create column name for price and volume
    if (build_in == "All"){
      p_col_name <- paste0("P.",yr_in)
      v_col_name <- paste0("V.",yr_in)   } else {
        p_col_name <- paste0("P.",build_in,".",yr_in)
        v_col_name <- paste0("V.",build_in,".",yr_in)
      }
    
    #get the data by neighborhood for the selected inputs
    nbhd_data <- setNames(data.frame(n_data[,1],n_data[,p_col_name],n_data[,v_col_name],n_data[,"id"]),
                          c("Neighborhood","Median_Price","Volume","id"))
    
    #trying to get rid of na's, test what this does
    nbhd_data <- nbhd_data[complete.cases(nbhd_data),]
    
      
    #create ggvis plot with tooltip
   
    all_values <- function(x) {
      if(is.null(x)) return(NULL)
      row <- nbhd_data[nbhd_data$id == x$id,"Neighborhood" ]
      paste0(names(row), format(row), collapse = "<br />")
    }
    
    #creat the ggvis plot
    nbhd_data %>% 
      ggvis(x=~Volume, y=~Median_Price, key:= ~id)%>% 
      layer_points(stroke := "steelblue", fill := "steelblue") %>%
      add_axis("x", title = "Volume (Units Sold)",title_offset = 40) %>%
      add_axis("y", title = "Median Price ( $ in 000's )",title_offset = 55) %>%
      scale_numeric("x", domain = c(0, 3000), nice = FALSE) %>%
      scale_numeric("y", domain = c(0, 3500), nice = FALSE)%>%
        
      add_tooltip(all_values, "hover")
  })
  
  outputPlot2 <- function(){
    #get year and building type inputs
    yr_in2 <- input$integer2
    
    build_in2 <- input$buildtype2
    
    #look up columns for price and volume using the ref_table
    
    #testing
    #yr_in2 <- 2010
    #build_in2 <- "Co-op"
    
    if (build_in2 == "Co-op"){
      build_in2 <- "Coop"
    }
    
    #create column name for price and 2003 price
    if (build_in2 == "All"){
      p_col_name2 <- paste0("P.",yr_in2)
      P03_col_name2 <- paste0("P.",2003)   } else {
        p_col_name2 <- paste0("P.",build_in2,".",yr_in2)
        P03_col_name2 <- paste0("P.",build_in2,".",2003)
      }
    
    #get the data by neighborhood for the selected inputs
    nbhd_data2 <- setNames(data.frame(n_data[,1],n_data[,p_col_name2],n_data[,P03_col_name2]),
                           c("Neighborhood","Median_Price","Median_Price_2003"))
    
    #order by Median Price for 2013
    nbhd_data2$Neighborhood <- reorder(nbhd_data2$Neighborhood, nbhd_data2$Median_Price_2003) 
    
    #create a bar chart
    bar<-ggplot(nbhd_data2, aes(x=Neighborhood,y=Median_Price)) +  
      geom_bar(stat="identity",color="darkblue",fill="steelblue") +
      geom_point(data=nbhd_data2, aes(x=Neighborhood,y=Median_Price_2003),color="darkred",fill="darkred")+
      xlab("Neighborhood") +  ylab("Median Price ( $ in 000s )") +
      coord_flip()+
      ggtitle("Manhattan Real Estate Sales by Neighborhood (Median Price)")+
      theme(panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))+
      scale_y_continuous(limits=c(0, 3500))
    
    print(bar)
  }
  
 
  outputPlot3 <- function(){
    #get year and building type inputs
    yr_in3 <- input$integer3
    
    build_in3 <- input$buildtype3
    
    #look up columns for price and volume using the ref_table
    
    #testing
    #yr_in3 <- 2010
    #build_in3 <- "Co-op"
    
    if (build_in3 == "Co-op"){
      build_in3 <- "Coop"
    }
    
    #create column name for price and 2003 price
    if (build_in3 == "All"){
      p_col_name3 <- paste0("P.",yr_in3)
      P03_col_name3 <- paste0("P.",2003)   } else {
        p_col_name3 <- paste0("P.",build_in3,".",yr_in3)
        P03_col_name3 <- paste0("P.",build_in3,".",2003)
      }
    
    #get the data by neighborhood for the selected inputs
    nbhd_data3 <- setNames(data.frame(n_data[,1],n_data[,p_col_name3],n_data[,P03_col_name3]),
                           c("Neighborhood","Median_Price","Median_Price_2003"))
    
    nbhd_data3$Pct_Change<-nbhd_data3$Median_Price/nbhd_data3$Median_Price_2003-1
    
    #order by Median Price
    nbhd_data3$Neighborhood <- reorder(nbhd_data3$Neighborhood, nbhd_data3$Pct_Change) 
    
    #create a bar chart
    pct_change<-ggplot(nbhd_data3, aes(x=Neighborhood,y=Median_Price)) +  
      #geom_bar(stat="identity",color="darkblue",fill="steelblue") +
      geom_point(data=nbhd_data3, aes(x=Neighborhood,y=Pct_Change),color="darkred",fill="darkred",size=2.5)+
      xlab("Neighborhood") +  ylab("% Change in Median Price") +
      coord_flip()+
      ggtitle("Manhattan Real Estate Sales by Neighborhood (% change in Median Price vs 2003)")+
      theme(panel.background = element_blank(), 
            panel.grid.major.y = element_line(colour="grey"),
            axis.ticks.x = element_line(colour="black"),
            axis.line = element_line(colour = "black"))+
      scale_y_continuous(limits=c(-.1, 3),labels=percent)
    
    print(pct_change)
  }
 
  
  
  #open the maps based in inputs in an iframe
    
    url <- reactive({
      yr_in4 <- input$integer4
    
      build_in4 <- input$buildtype4
      
      if (build_in4 == "Co-op"){
        build_in4 <- "Coop"
      }
      
      paste0("http://sms2015.github.io/MedPrice",build_in4,yr_in4,".html")
            
      })
    
  # send to output
  main_plot %>% bind_shiny("plot")
  #output$plot <- renderPlot(outputPlot(), height=500, width=600)
  output$plot2 <- renderPlot(outputPlot2(), height=500, width=600)
  output$plot3 <- renderPlot(outputPlot3(), height=500, width=800)
  output$Map1 <- renderUI({
    tags$iframe(src = url(), height=500, width=600)
    })
  
})