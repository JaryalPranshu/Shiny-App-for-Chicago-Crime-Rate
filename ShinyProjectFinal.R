
library(data.table)
library(reshape2)
library(ggplot2)
library(leaflet)
library(tidyverse)
library(DT)
library(shiny)
library(dplyr)
library(lubridate)


Crimes2018 <- read.csv("Crimes_-_2001_to_present.csv")

# Creating Dataframe
Crimes2018.df <- as.data.frame(Crimes2018)
Crimes2018.df$DATES <- mdy_hms(Crimes2018.df$Date)
Crimes2018.df$Month <- month(Crimes2018.df$DATES) 
Crimes2018.df$Hours <- hour(Crimes2018.df$DATES) 
Crimes2018.df$DateOnly <- date(Crimes2018.df$DATES)

#Pre-processing

## Checking for missing values(column wise)
sapply(Crimes2018, function(x) sum(is.na(x)))

# Tab 1: Frequency of crime by month and crime type. Create bar charts for different month crime type combinations.

Month <- unique(Crimes2018.df$Month)
Crimes <- unique(Crimes2018.df$Primary.Type)

Crimes.Month <- table(Crimes2018.df$Primary.Type,Crimes2018.df$Month)
Crimes.Month.df <- as.data.frame(Crimes.Month)



##Renaming the columns
setnames(Crimes.Month.df, "Var1", "Crime_type")
setnames(Crimes.Month.df, "Var2", "Month")

## Removes all rows that don't have location data

CrimesWithLoc.data <- subset(Crimes2018.df, !is.na(Crimes2018.df$Lat))
setnames(CrimesWithLoc.data, "Longitude", "longitude")
setnames(CrimesWithLoc.data, "Latitude", "latitude")

##### Tab 3: A heatmap using the type of crime and the hour of the day when the crime was committed.
hour <- unique(Crimes2018.df$hours)
Crimes <- unique(Crimes2018.df$Primary.Type)

Crimes.hour <- table(Crimes2018.df$Primary.Type,Crimes2018.df$Hours)
Crimes.hour.df <- as.data.frame(Crimes.hour)
##Renaming the columns
setnames(Crimes.hour.df, "Var1", "Crime_type")
setnames(Crimes.hour.df, "Var2", "Hour")

# Calculating Correlation

melted_Crimes.hour.df <- melt(Crimes.hour.df)
head(melted_Crimes.hour.df)


## Shiny App

ui <- fluidPage(
  tabsetPanel(
    tabPanel (title = "Frequency of crime by month and crime type",
              sidebarLayout(
                sidebarPanel(
                  selectInput("Crime", 
                              label = "Select Crime to see frequency by month",
                              choices = unique(Crimes2018.df$Primary.Type))),
                mainPanel(
                  plotOutput(outputId = "BarChart")))),
    
    tabPanel (title = "Location of crimes by date on a map",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("DayofYear",
                              label = "Pick date",
                              min = as.Date("2018-01-01","%Y-%m-%d"),
                              max = as.Date("2018-12-31","%Y-%m-%d"),
                              value = as.Date("2018-10-27"),
                              timeFormat = "%Y-%m-%d")
                ),
                mainPanel( 
                  leafletOutput(outputId = "map"),
                 "Zoom in/out of map to see detail location of Crime"))
              ),
    
    tabPanel (title = "Heatmap of crime and hour of the day",
              plotOutput(outputId = "HeatMap")))
  
)
server <- function(input, output) {
  Crime <- reactive({
    a <- subset(Crimes.Month.df, Crimes.Month.df$Crime_type == input$Crime)
    return(a)})
  
    output$BarChart <- renderPlot({
    
    p <- ggplot(data = Crime(), aes(x= Month , y = Freq)) +
      geom_bar(stat="identity")
    print(p)
    })
    
    output$HeatMap <- renderPlot({
    h <-   ggplot(data = melted_Crimes.hour.df, aes(x=Hour, y=Crime_type, fill=value)) + 
      geom_tile()
    print(h)
    })
  
 
    output$map <- renderLeaflet({
      
      CrimeDay <- reactive({
        CrimesWithLoc.data[CrimesWithLoc.data$DateOnly  == input$DayofYear,]
      })  
       m  <- leaflet(CrimeDay(), width = "100") %>%
        setView(lng = -87.623177 , lat = 41.881832, zoom = 10) %>%
        addTiles(group = "OSM (defaul)") %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addMarkers(lng =~ longitude, lat =~ latitude,
                   group = df,
                   clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                   labelOptions = labelOptions(noHide = F, drection = 'auto')) 
         
        
    print(m)
    })
}
shinyApp(ui = ui, server = server)

