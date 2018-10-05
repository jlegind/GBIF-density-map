require(leaflet)
library(rgdal)
library(dplyr)
library(rsconnect)


countries <- readOGR("countries.geojson", "OGRGeoJSON")
#This is the SpatialPolygonsDataFrame

countries@data <- data.frame(sapply(countries@data, as.character), stringsAsFactors = FALSE)
#Converts the data object to character then into a data frame without any factors

countries@data[countries$ADMIN=="France", "ISO_A3"] <- "FRA"
countries@data[countries$ADMIN=="Norway", "ISO_A3"] <- "NOR"
#Inexplicably these two country have no ISO code

dns <- read.csv("density_map.csv")
#Stats from HIVE and the World Bank

dns <- plyr::rename(dns, c("X_c1"= "iso3", "X_c11"="occurrences", "X_c12"="density_score"))
dns$iso3 <- sapply(dns$iso3, trimws)
dns$density_score <- sapply(dns$density_score, function(x) as.numeric(as.character(x)))
#function(x) needed to preserve decimal numbers from factors

res <- dplyr::right_join(dns, countries@data, by=c("iso3"="ISO_A3"))
#This is where the two frames are merged. Next is adding it back to spatial data frame
countries@data <- res

labels <- paste("Density score: ", countries$density_score)

pl <- colorQuantile(palette = "YlGnBu", domain = countries$density_score, n=9)
#pl <- colorBin(palette = "YlGnBu", domain = countries$density_score, 9, pretty = FALSE)
m <- leaflet(countries, options = leafletOptions(zoom = 2))
m <- addTiles(m)
m <- setView(m, lng=1.567, lat=42.09, zoom=3) 
addPolygons(m, stroke=FALSE, fillOpacity = 0.5, color=~pl(density_score), label=labels) %>%
  addLegend("bottomright", title="Density score<br> landarea/occurrence count", pal=pl, values=~density_score, opacity=0.8)

### Shiny ###
library(shiny)

ui <- fluidPage(
  titlePanel("Density map"),
  leafletOutput("densityMap", height=800)
)

server <- function(input, output){
  output$densityMap <- renderLeaflet({
    m <- leaflet(countries, options = leafletOptions(zoom = 2))
    m <- addTiles(m) 
    m <- setView(m, lng=1.567, lat=42.09, zoom=3) 
    addPolygons(m, stroke=FALSE, fillOpacity = 0.5, color=~pl(density_score), label=labels) %>%
      addLegend("bottomright", title="Density score<br> landarea/occurrence count", pal=pl, values=~density_score, opacity=0.8)
  })
}

shinyApp(ui, server)    

