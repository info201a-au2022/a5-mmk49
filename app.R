#Load libraries
library("tidyverse")
library("leaflet")
library("sp")
library("rgdal")
library("RColorBrewer")

#Read in data
read_CO2_data <- read.csv("owid-co2-data.csv", stringsAsFactors = FALSE)

world_spdf <- readOGR( 
  dsn= paste0("world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

get_emission_by_year <- function(emission, years) {
    yearly_emissions <- read_CO2_data %>% 
      filter(year == years) %>% 
      select(iso_code, all_of(emission)) %>% 
      group_by(iso_code) %>% 
      summarize_at(emission, sum, na.rm = TRUE) %>% 
      na.omit()
    
    return(yearly_emissions)
}
test <- get_emission_by_year("coal_co2",2021)

get_bin <- function(emission) {
  mybins <- c(0, Inf)
  
  if(emission == "cement_co2") {
    mybins <- c(0, 1, 5, 10, 50, Inf)
  } else if(emission == "coal_co2") {
    mybins <- c(0, 1, 10, 100, 500, Inf)
  } else if(emission == "gas_co2") {
    mybins <- c(0, 1, 10, 100, 500, Inf)
  } else if(emission == "oil_co2") {
    mybins <- c(0, 5, 50, 100, 500, Inf)
  } else if (emission == "co2") {
    mybins <- c(0, 10, 100, 500, 1000, Inf)
  }
  
  return(mybins)
}

map_emission_by_year <- function(emission, years) {
  yearly_emissions <- get_emission_by_year(emission, years)
  
  mybins <- get_bin(emission)
  
  colnames(yearly_emissions)[1] <- "ISO3"
  colnames(yearly_emissions)[2] <- "CO2"
  world_spdf_with_values <- sp::merge(world_spdf, yearly_emissions, by = "ISO3", all=F)
  
  mypalette <- colorBin( palette="Blues",
                         domain=world_spdf_with_values@data$CO2,
                         na.color="transparent", 
                         bins = mybins)
  
  mytext <- paste(
    "Country: ", world_spdf_with_values@data$NAME,"<br/>", 
    "Value: ", round(world_spdf_with_values@data$CO2),
    sep="") %>%
    lapply(htmltools::HTML)
  
  value_map <- leaflet(world_spdf_with_values) %>% 
    addTiles()  %>% 
    setView( lat=10, lng=0 , zoom=2) %>%
    addPolygons( 
      fillColor = ~mypalette(CO2),
      stroke=TRUE, 
      fillOpacity = 0.9, 
      color="Black", 
      weight=0.9,
      label = mytext,
      labelOptions = labelOptions( 
        style = list("font-weight" = "normal", padding = "3px 8px"), 
        textsize = "13px", 
        direction = "auto"
      )
    ) %>%
    addLegend( pal=mypalette, values=~CO2, opacity=0.9, title = "Value (Million Tonnes)", position = "bottomleft" )
  
  return(value_map)
}