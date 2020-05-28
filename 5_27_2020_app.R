#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(tigris)
library(leaflet)


options(tigris_use_cache = TRUE)

# Get zip code shapes for NYC'
nyc_zip <- zctas(cb = TRUE, starts_with = c("0", "10", "11"))



# Add a column that describes the year of each rat sighting
rawdata <- read.csv("Rat_Sightings.csv")

rawdata %>%
select("Created.Date") %>%
sapply(as.character) %>%
str_sub(start = 7, end= 10) -> year


ratdata <- cbind(rawdata, year)

ratdata %>%
  arrange(year) -> ratdata


# Create a data frame with a row for each zip code and a column for every year
# Each column will represent the change in proportion of rat sightings from 2010

# First, convert empty values and nonsense values to "N/A"
levels(ratdata$Incident.Zip)[1] <- "N/A"
ratdata$Incident.Zip[ratdata$Incident.Zip == "1123"] <- "N/A"
ratdata$Incident.Zip[ratdata$Incident.Zip == "100354"] <- "N/A"

# Create list of unique zip codes
ratdata %>%
  select("Incident.Zip") %>%
  unique() %>%
  sapply(as.character) -> unique_zips

# Create a list of unique years
ratdata %>%
  select("year") %>%
  unique() %>%
  sapply(as.character) -> unique_years


# Get zip code shapes for NYC'
nyc_zip <- zctas(cb = TRUE, starts_with = unique_zips[unique_zips != "N/A"])


# This function will calculate how many sightings correspond to each zip code per year


get_num_of_sightings_zip <-function(zipcode) {
  ratdata %>%
    filter(Incident.Zip == zipcode) %>%
    nrow() -> x_total

  
  x_years = data.frame(zip = as.character(zipcode),
                       total = x_total,
                       "x2010" = c(0),
                       "x2011" = c(0),
                       "x2012" = c(0),
                       "x2013" = c(0),
                       "x2014" = c(0),
                       "x2015" = c(0),
                       "x2016" = c(0),
                       "x2017" = c(0),
                       stringsAsFactors = FALSE
                       )
  for (i in 1:length(unique_years)) {
    ratdata %>% 
      filter(Incident.Zip == zipcode, year == unique_years[i]) %>%
      nrow() -> x_years[1,(i+2)]
  }
  return(x_years)
  
}




# Apply that function to every zip code
unique_zips %>%
  lapply(get_num_of_sightings_zip) %>%
  do.call(rbind,.) -> zip_data



# Join zip boundaries and rat sighting data
nyc_zip2 <- geo_join(nyc_zip, 
                      zip_data, 
                      by_sp = "GEOID10", 
                      by_df = "zip",
                      how = "left")


# Create a color palette
pal <- colorNumeric(
  palette = "Greens",
  domain = nyc_zip2@data$total
)


labels <- paste0(
  "Zip Code: ",
  nyc_zip2@data$GEOID10, ", ",
  "Number of Rat Sightings: ",
  nyc_zip2@data$total
)

# Plot zip codes on map
nyc_zip2 %>%
  leaflet %>% 
  setView(lng = -73.9500, lat = 40.7128, zoom = 10) %>%
  addProviderTiles("CartoDB") %>%
  
  addPolygons(fillColor = ~pal(total),
              weight = 0.5,
              opacity = 0.5,
              color = "black",
              fillOpacity = 0.7,
              highlight = highlightOptions(weight = 2,
                                           color = "red"),
                
              label = labels) %>% 
  addLegend(pal = pal,
            values = ~total,
            opacity = 0.7,
            title = htmltools::HTML("Number of <br>
                                    Rat Sightings"),
            position = "bottomright")






test_year = paste0("x",2010)


############################################################################ 


# Define UI for application that draws a map of NYC rat sightings
ui <- fluidPage(
   
   # Application title
   titlePanel("Rat Sightings in NYC"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput(inputId = "year",
                     label = "Year:",
                     min = 2010,
                     max = 2016,
                     value = 2012,
                     step = 1,
                     sep = "")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("nyc_map")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Detect which year was selected by user
  getSelectedYearIndex <- reactive({
    input$year - 2010 + 7
  })
  
  # Create a color palette
  pal <- reactive(
    colorNumeric(
    palette = "Greens",
    domain = nyc_zip2@data[,input$year - 2010 + 7]
  ))
  
  
  labels <- reactive(
    paste0(
    "Zip Code: ",
    nyc_zip2@data$GEOID10, ", ",
    "Number of Rat Sightings: ",
    nyc_zip2@data[,input$year - 2010 + 7]
  ))
  
   
  output$nyc_map <- renderLeaflet({
    selectedYearIndex <- getSelectedYearIndex() 
    
     nyc_zip2 %>%
       leaflet %>% 
       setView(lng = -73.9500, lat = 40.7128, zoom = 10) %>%
       addProviderTiles("CartoDB") %>%
       
       addPolygons(fillColor = ~pal(nyc_zip2@data[,selectedYearIndex]),
                   weight = 0.5,
                   opacity = 0.5,
                   color = "black",
                   fillOpacity = 0.7,
                   highlight = highlightOptions(weight = 2,
                                                color = "red"),
                   
                   label = labels) %>% 
       addLegend(pal = pal,
                 values = ~nyc_zip2@data[,selectedYearIndex],
                 opacity = 0.7,
                 title = htmltools::HTML("Number of <br>
                                    Rat Sightings"),
                 position = "bottomright")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

