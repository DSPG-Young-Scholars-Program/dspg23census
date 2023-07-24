library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(htmlwidgets)
library(wordcloud2)
library(ggplot2)
library(MASS)

# Define UI for app that draws a histogram ----
ui <-  shinyUI(navbarPage(title="State Data Use and Needs",
               tabPanel("Overview",
                        box(title="Project Overview",
                        p("To provide the Census Bureau with information on the following."),
                        p("1. What data sources do state, U.S. territories, and District of Columbia, data centers use?"),
                        p("2. How do they use these data sources?"),
                        p("3. What are their future data needs?"))
                        ), 
               tabPanel("Findings",
                        br(),
                        sidebarLayout(sidebarPanel(
                          selectInput("dropdown2", "Which state are you interested in?",
                                        list("All states" = "all","Alaska"="AK", "Virginia"="VA", "California"= "CA"))),
                          mainPanel(textOutput("text2")
                          ))),
               tabPanel("Word Cloud",
                        br(),
                        sidebarLayout(sidebarPanel(
                          selectInput("dropdown3", "Which state's mission statement are you interested in?",
                                      list("Alaska"="AK", "Virginia"="VA", "California"= "CA"))),
                          mainPanel(textOutput("text3"),
                                    plotOutput("plot3")
                          ))),
               tabPanel("Team",
                        box(title="Meet Our Team", width = 6,
                          br(),
                          h5("DSPG, University of Virginia, Biocomplexity Institute, Social and Decision Analytics"),
                          p("- Marijke van der Geer, Fourth Year @ SDSU (Stats & DS)"),
                          p("- Jianing Cai, Fourth Year @ UVA (CS & Math)"),
                          br(),
                          h5("University of Virginia, Biocomplexity Institute, Social and Decision Analytics"),
                          p("- Vicki Lancaster, Principal Scientist"),
                          p("- Neil Kattampallil, Research Scientist"),
                          p("- Treena Goswami, Postdoc Researcher")))))
  
 

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$text2 <- renderText({
    if (input$dropdown2 == "all"){
      "You have selected all states to inspect"
    }
    else if (input$dropdown2 == "VA"){
      "You have selected Virginia to inspect"
    }
    else if (input$dropdown2 == "AK"){
      "You have selected Alaska to inspect"
    }
    else if (input$dropdown2 == "CA"){
      "You have selected California to inspect"
    }
  })
  
  output$text3 <- renderText({
    if (input$dropdown3 == "VA"){
      "Word Cloud on Virginia's SDC Mission Statement"
    }
    else if (input$dropdown3 == "AK"){
      "Word Cloud on Alaska's SDC Mission Statement"
    }
    else if (input$dropdown3 == "CA"){
      "Word Cloud on California's SDC Mission Statement"
    }
  })
  
}


shinyApp(ui = ui, server = server)
