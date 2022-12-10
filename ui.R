library(shiny)
library(ggplot2)
library(DT)

source("app.R")

co2_df <- read_CO2_data

page_one <- tabPanel(
  "Introduction",
  titlePanel("Introduction"),
  headerPanel(img(src = "pollution.jpg")),
    
  mainPanel(
    h4("What can this application do?"),
    p("CO2 emissions are a global issue that drive climate change and therefore effects everybody
      on the planet. This application allows you to observe a choropleth map containing the latest CO2 
      emission values for cement, coal, oil, and gas for each country in the world in 
      millions of tonnes. In addition to the data being displayed in a choropleth map, a 
      correspending interactive table is available to look at the unrounded CO2 values and
      explore data from years other than the latest years. This allows for various research
      questions to be explored. The research question that this application will be focusing
      on is which countries are currently producing the most/least CO2 emissions in the
      categories of cement, coal, oil, and gas emissions. This application hopes to find which
      countries are producing the most CO2 emissions so that we can make steps to reduce cement,
      coal, oil, and gas emissions around the world"),
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
    h2(strong("CO2 Emission Map")),
    leafletOutput("co2_map"),
    h3(strong("Why this map is useful: ")),
    p("When I began deciding how I would display the data in an effective and interactive way I had
      many options to choose from. A map stood out to me the most. The data was for every country
      in the world so I felt like a map represented the data perfectly. A map is a great tool to see
      every countries CO2 data at once. On its own a map is just a table placed into geographic
      locations. I still needed to make the data more digestable. For this I employed a choropleth,
      a type of map that uses a color gradient to bin values. Using a choropleth was a perfect solution
      for the aim of my application. In a choropleth one can easily see high and low values because
      high values are the darkest and low values are white. This makes the map easily understandable
      and interactive."),
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
      ~622mT produced. Among all of these categories, the USA, China, and India are seen contributing
      to CO2 production the most. In terms of least CO2-producing countries it is harder to pick
      out because many countries produce a minimal amount or no CO2 in all the categories. The general
      trend is that eastcoast, mid, and south African countries, as well as Iceland, Greenland, 
      Svalbard and some South American countries produce minimal amounts of CO2 across all categories.
      Overall we can see the trend that polluter countries like the United States, China and India
      show up within the top contributers of CO2 emission production across all categories. This
      application thus suggests the countries with high CO2 emissions must take action to reduce emissions
      across all categories in order to combat the negative effects of high CO2 emissions."),
    h2(strong("CO2 Emission Table")),
    dataTableOutput("co2_data"),
    h3(strong("Why this table is useful: ")),
    p("The map above was created to show the latest CO2 emission data for countries across all categories
      in an easily digestable way. This was perfect for completing the goal of this application which
      was to compare oil, cement, coal, and gas emission for all countries and to see who were the
      highest and lowest producers in each catergory. This table fills in the areas where the map
      becomes unhelpful, that is, data for countries across all categories for any year (as opposed
      to only the latest figures). Although this chart is much less digestable, it allows for others
      using this app to explore their own research questions about oil, cement, coal, and gas emissions."),
    h3(strong("Key Findings: ")),
    p("This chart is largely unimportant in regards to my research in this application other than comparing
      previous years values to the latest data in my chart to see how CO2 emissions have grown but I 
      can imagine that the chart would be useful for many other research purposes such as comparing emissions
      between select countries or viewing the change of CO2 over time for a single country which can reveal
      its own respective key findings. Overall this chart allows users to explore their own inquiries about
      the data.")
  )
)
  
ui <- fluidPage( 
  navbarPage (
    "CO2 Data Analysis",
    page_one,
    page_two
  )
) 
