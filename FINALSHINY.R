#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("World Happiness Report"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("region",
                        "Select a Region:",
                        choices = c(happiness$`Regional indicator`),
                        selected = c("East Asia"), 
                        multiple = TRUE),
        
            sliderInput("gdp", 
                        "Minimum GDP per Capita:", 
                        min = min(happiness$`Logged GDP per capita`),
                        max = max(happiness$`Logged GDP per capita`),
                        value = median(happiness$`Logged GDP per capita`))
                        
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
           tabPanel("Graph #1", plotOutput("graph1")),
           tabPanel("Graph #2",plotOutput("graph2")),
           tabPanel("Graph #3", plotOutput("graph3")),
           tabPanel("Graph #4", plotOutput("graph4"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## filter the data
  filtered_data <- reactive({
    dplyr::filter(happiness, happiness$`Regional indicator` == input$region, happiness$`Logged GDP per capita` >= input$gdp)
  })
  

    output$graph1 <- renderPlot({
       ggplot(filtered_data(), aes(x = `Regional indicator`, y = `Ladder score`, `Regional indicator`)) +
        geom_col() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        guides(fill = FALSE) +
        labs(title = "Ladder Score",
             x = "Region",
             y = "Ladder Score")
    })
    
    output$graph2 <- renderPlot ({
      ggplot(filtered_data(), aes(x = `Logged GDP per capita`, y = `Ladder score`, color = `Regional indicator`)) + 
        geom_point(mapping = aes(size = 5, alpha = 0.5)) +
        guides(size = FALSE, alpha = FALSE) +
        theme_minimal() + 
        labs(title = "GDP Per Capita vs. Happiness Score",
             x = "GDP per Capita",
             y = "Happiness Score")
    })
    
    output$graph3 <- renderPlot({
      ggplot(filtered_data(), aes(x = `Freedom to make life choices`, y = `Perceptions of corruption`, color = `Regional indicator`)) +
        geom_point(mapping = aes(size = `Ladder score`, alpha = .5))+
        guides(size = FALSE, alpha = FALSE) +
        theme_minimal() +
        labs(title = "Freedom to Make Life Choices vs. Perception of Corruption",
             x = "Freedom to Make Life Choices",
             y= "Perception of Corruption")
    })
    
    output$graph4 <- renderPlot ({
      ggplot(filtered_data(), aes(x = `Healthy life expectancy`, y = `Ladder score`, color = `Regional indicator`))+
        geom_point(mapping = aes(size = `Social support`, alpha = 0.5)) +
      guides(size = FALSE, alpha = FALSE) +
        theme_bw() +
        labs(title = "Life Expectancy vs. Happiness Score",
             x = "Life Expectancy",
             y= "Happiness Score")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
