library(shiny)
library(tidyverse)
library(scales)
library(plotly)
library(ggplot2)
load(url("https://github.com/cem268/Final-Project-2/blob/main/info2.RData?raw=true"))

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
  
      titlePanel("Gross Amounts (in $)"),
      
      selectInput(inputId = "var",
                  label = "Variable",
                  choices = c("Daily", "Avg", "To_Date"),
                  selected = "Daily"),
      
      selectInput(inputId = "color_p" ,
                  label = "Choose Point Color:",
                  choices = c("Red", "Blue", "Black", "Green"),
                  selected = "Black"),
      
      selectInput(inputId = "color_l" ,
                  label = "Choose Line Color:",
                  choices = c("Red", "Blue", "Black", "Green"),
                  selected = "Black"),
     
      sliderInput(inputId = "range",
                  label = "Day Range of Interest:",
                  min = 0,
                  max = 234,
                  value = c(0,234))
    
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Jurassic World (2015)", plotOutput(outputId = "graph_1")),
        tabPanel("Finding Dory (2016)", plotOutput(outputId = "graph_2")),
        tabPanel("Star Wars: Episode VIII - The Last Jedi (2017)", plotOutput(outputId = "graph_3")),
        tabPanel("Black Panther (2018)", plotOutput(outputId = "graph_4")),
        tabPanel("Avengers: Endgame (2019)", plotOutput(outputId = "graph_5")),
        tabPanel("Bad Boys for Life (2020)", plotOutput(outputId = "graph_6"))
        
    )
  )
)
)

server <- function(input, output) {
  

  
  output$graph_1 <- renderPlot({
    d1<-subset(info, info$Movies=="Jurassic World")
    ggplot(data = d1, aes_string(x = d1$Day, y = input$var)) + geom_line(colour=input$color_l) + 
      geom_point(colour=input$color_p) + scale_y_continuous(labels = comma) + ylab(as.character(input$var)) + xlab("Day") +
      labs(title=input$plot_title) + xlim(as.numeric(input$range[1]), as.numeric(input$range[2])) 
  })
  
  output$graph_2 <- renderPlot({
    d2<-subset(info, info$Movies=="Finding Dory")
    ggplot(data = d2, aes_string(x = d2$Day, y = input$var)) + geom_line(colour=input$color_l) + 
      geom_point(colour=input$color_p) + scale_y_continuous(labels = comma) + ylab(as.character(input$var)) + xlab("Day") +
      labs(title=input$plot_title) + xlim(as.numeric(input$range[1]), as.numeric(input$range[2])) 
  })
  output$graph_3 <- renderPlot({
    d3<-subset(info, info$Movies=="Star Wars: Episode VIII - The Last Jedi")
    ggplot(data = d3, aes_string(x = d3$Day, y = input$var)) + geom_line(colour=input$color_l) + 
      geom_point(colour=input$color_p) + scale_y_continuous(labels = comma) + ylab(as.character(input$var)) + xlab("Day") +
      labs(title=input$plot_title) + xlim(as.numeric(input$range[1]), as.numeric(input$range[2])) 
  })
  
  output$graph_4 <- renderPlot({
    d4<-subset(info, info$Movies=="Black Panther")
    ggplot(data = d4, aes_string(x = d4$Day, y = input$var)) + geom_line(colour=input$color_l) + 
      geom_point(colour=input$color_p) + scale_y_continuous(labels = comma) + ylab(as.character(input$var)) + xlab("Day") +
      labs(title=input$plot_title) + xlim(as.numeric(input$range[1]), as.numeric(input$range[2])) 
  })
    
  output$graph_5 <- renderPlot({
    d5<-subset(info, info$Movies=="Avengers: Endgame")
    ggplot(data = d5, aes_string(x = d5$Day, y = input$var)) + geom_line(colour=input$color_l) + 
      geom_point(colour=input$color_p) + scale_y_continuous(labels = comma) + ylab(as.character(input$var)) + xlab("Day") +
      labs(title=input$plot_title) + xlim(as.numeric(input$range[1]), as.numeric(input$range[2])) 
  })
  
  output$graph_6 <- renderPlot({
    d6<-subset(info, info$Movies=="Bad Boys for Life")
    ggplot(data = d6, aes_string(x = d6$Day, y = input$var)) + geom_line(colour=input$color_l) + 
      geom_point(colour=input$color_p) + scale_y_continuous(labels = comma) + ylab(as.character(input$var)) + xlab("Day") +
      labs(title=input$plot_title) + xlim(as.numeric(input$range[1]), as.numeric(input$range[2])) 
  })
}

shinyApp(ui = ui, server = server)

