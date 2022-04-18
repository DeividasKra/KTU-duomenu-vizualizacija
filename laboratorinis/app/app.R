library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Verslo veikla"),
                    dashboardSidebar(selectizeInput(inputId = "imones_pavadinimas", label="Imones pavadinimas",
                                                    choices= NULL, selected= NULL)),
                    dashboardBody(tabsetPanel(
                      tabPanel("Vidutinio atlyginimo grafikas", plotOutput("plot1"))
                    )
                    )
)

server <- function(input, output, session){
  data <- read_csv("../data/lab_sodra.csv")
  data1 <- data %>%
    filter(ecoActCode == 682000) %>%
    mutate(month_value=as.integer(substr(month, 5 ,7)))
  
  updateSelectizeInput(session, "imones_pavadinimas", 
                       choices = data1$name, 
                       server = TRUE)
  
  output$table <- renderTable(
    data1 %>%
      filter(name == input$imones_pavadinimas), digits = 0
  )
  
  output$plot1 <- renderPlot(
    data1 %>%
      filter(name == input$imones_pavadinimas) %>%
      ggplot(aes(x = month_value, y = avgWage)) +
      scale_x_continuous("Menuo",breaks=1:12,limits=c(1,12)) +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      geom_point() +
      geom_line(colour = 'black') +
      labs(x = "Menuo", y = "Atlyginimas eurais")
  )
  
  
  
}
shinyApp(ui, server)