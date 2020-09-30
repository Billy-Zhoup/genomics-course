#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(maps)
library(mapdata)
library(wesanderson)

daily_report_0926 <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/09-26-2020.csv")) %>% 
    rename(Long = "Long_") %>% 
    group_by(Country_Region) %>% 
    summarise_at(c("Confirmed", "Deaths"), sum)

# load the global map data
global <- as_tibble(map_data("world")) %>% 
    mutate(region = str_replace_all(region, c("USA" = "US", "Czech Republic" = "Czechia",  
                                              "Ivory Coast" = "Cote d'Ivoire", "Democratic Republic of the Congo" = "Congo (Kinshasa)", 
                                              "Republic of Congo" = "Congo (Brazzaville)")))
# We need to join the us map data with our daily report to make one data frame/tibble
country_join <- left_join(global, daily_report_0926, by = c("region" = "Country_Region"))

Report_Type = c("Confirmed", "Deaths")

# Define UI for application 
ui <- fluidPage(
    
    # Application title
    titlePanel("Graphs for 20200926 COVID-19 Reporting data"),
    p("Data for this application are from the Johns Hopkins Center for Systems Science and Engineering",
      tags$a("GitHub Repository", href="https://github.com/CSSEGISandData")
    ),
    tags$br(),
    tags$hr(),  # Adds line to page
    
    sidebarLayout(
        sidebarPanel(
            # Select Reporting type
            selectInput("select_type", 
                        label = "Report Type", 
                        choices = Report_Type, selected = "Confirmed"),
        ),
        
        # Show a plots
        mainPanel(
            plotOutput("Plot1")
        )
    )
)

# Define server logic required to make the plot
server <- function(input, output) {
    
    output$Plot1 <- renderPlot({

        # plot world map
        ggplot(data = global, mapping = aes(x = long, y = lat, group = group)) + 
            coord_fixed(1.3) + 
            # Add data layer
            geom_polygon(data = country_join, aes_string(fill = input$select_type), color = NA) +
            
            scale_fill_gradientn(colours = 
                                     wes_palette("Zissou1", 100, type = "continuous"),
                                 trans = "log10") +
            ggtitle("JHU COVID-19 data for reporting type:", input$select_type)
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)