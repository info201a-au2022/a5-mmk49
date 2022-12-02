library(shiny)
library(ggplot2)
library(DT)

source("app.R")

co2_df <- read_CO2_data

page_one <- tabPanel(
  "Introduction",
  titlePanel("Introduction"),
    
  mainPanel(
    h4("What can this application do?"),
    p("This application allows you to observe a choropleth map containing the latest CO2 
      emission values for cement, coal, oil, and gas for each country in the world in 
      millions of tonnes. In addition to the data being displayed in a choropleth map, a 
      correspending interactive table is availiable to look at the unrounded CO2 values"),
    p(),
    h4("An example with the US: "),
    htmlOutput("US_example")
  )
)
  
page_two <- tabPanel(
  "Data", 
  headerPanel("CO2 Viewer"),
  sidebarPanel(
  h2(strong("Map Options: ")),
  selectInput(inputId =  "select_co2", label = h3("Emission Type"), 
              choices = list("Total CO2" = "co2",
                             "Cement CO2" = "cement_co2",
                             "Coal CO2" = "coal_co2", 
                             "Gas CO2" = "gas_co2", 
                             "Oil CO2" = "oil_co2"),
              selected = 1),
  checkboxInput("map_checkbox", label = "Show Map", value = TRUE),
  h2(strong("Table Options:")),
  sliderInput("year_slider", "Year", min = min(co2_df$year), max = max(co2_df$year), value = 2021, sep = ""),
  checkboxInput("table_checkbox", label = "Show Table", value = TRUE)
  ),
  mainPanel(
    leafletOutput("co2_map"),
    h3(strong("Key Findings: ")),
    p("Exploring the different emission types for each country reveals numerous interesting 
      trends about the dataset. In the choropleth graphs it is easy to see which countries
      are contributing the most/least CO2 emission for every emission type. In the ", 
      strong("gas CO2"), " category, the United States is by far the greatest producer with ~1637
      million tonnes (mT) while the runner ups, Russia and China have ~875mT and ~774mT
      respectively. In the ", strong("coal CO2"), " category, China is by far the greatest 
      producer with ~7956mT while the runner ups, India and the USA have ~1802mT 
      and ~1002mT respectively, In the ", strong("cement CO2"), " category, China is by far the greatest 
      producer with ~853mT while the runner ups, India and Vietnam have ~149mT 
      and ~54mT respectively. In the ", strong("oil CO2"), " category, the USA and China are near each
      other with ~2234mT and ~1713mT worth of emissions produced India is in third place with
      ~622mT produced. Among all of these categories, the USA, China, and india are seen contributing
      to CO2 production the most. In terms of least CO2-producing countries it is harder to pick
      out because many countries produce a minimal amount or no CO2 in all the categories. The general
      trend is that eastcoast, mid, and south African countries, as well as Iceland, Greenland, 
      Svalbard and some South American countries produce minimal amounts of CO2 across all categories"),
    dataTableOutput("co2_data")
  )
)
  
ui <- fluidPage( 
  navbarPage (
    "CO2 Data Analysis",
    page_one,
    page_two
  )
) 
