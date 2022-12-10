library(shiny)
library(ggplot2)

source("app.R")

co2_df <- read_CO2_data

US_co2_df <- co2_df %>% 
  filter(country == "United States")

oil_2021 <- US_co2_df %>% 
  filter(year == 2021) %>% 
  pull(oil_co2) %>% 
  round()

oil_1870 <- US_co2_df %>% 
  filter(year == 1870) %>% 
  pull(oil_co2) %>% 
  round()
  
gas_2021 <- US_co2_df %>% 
  filter(year == 2021) %>% 
  pull(gas_co2) %>% 
  round()

gas_1870 <- US_co2_df %>% 
  filter(year == 1870) %>% 
  pull(gas_co2) %>% 
  round()

coal_2021 <- US_co2_df %>% 
  filter(year == 2021) %>% 
  pull(coal_co2) %>% 
  round()

coal_1870 <- US_co2_df %>% 
  filter(year == 1870) %>% 
  pull(coal_co2) %>% 
  round()

cement_2021 <- US_co2_df %>% 
  filter(year == 2021) %>% 
  pull(cement_co2) %>% 
  round()

cement_1870 <- US_co2_df %>% 
  filter(year == 1870) %>% 
  pull(cement_co2) %>% 
  round()

function(input, output) {
  
  output$co2_map <- renderLeaflet({
    if(input$map_checkbox == TRUE) {
    map_emission_by_year(input$select_co2, 2021)
    }
  })
  
  output$US_example <- renderText({
    US_text <- paste0("Thoughout United States history, in millions of tonnes, carbon emissions from oil have increased from ", oil_1870, " in 1870 ", "to ", strong(oil_2021), " in 2021. ", "Gas has increased from ", gas_1870, " to ", strong(gas_2021), ". Coal has increased from ", coal_1870, " to ", strong(coal_2021), ". Cement has increased from ", coal_1870, " to ", strong(coal_2021), ". In all forms of carbon production there has been an exponential increase in its production since 1870. This is one example that showcases how this application can be used to show how the CO2 production of certain countries has reached uncontrolable amounts and expose the countries who need to make changes to reduce global emissions.")
  })
  
  output$co2_data <-DT::renderDataTable(
    if(input$table_checkbox == TRUE) {
      datatable(
        get_emission_by_year(input$select_co2, input$year_slider),
        filter = 'top',
        colnames = c("ISO Code", input$select_co2)
      )
    }
  )
  
}