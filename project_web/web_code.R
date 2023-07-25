library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(htmlwidgets)
library(wordcloud2)
library(ggplot2)
library(MASS)
library(dplyr)
library(data.table)
library(rsconnect)
library(forcats)
library(plotly)
library(DT)

econ_data <- read.csv("/Users/jianingcai/Downloads/Economy_compilation.csv")
econ_data$Sub.categories = tolower(data$Sub.categories)


# Define UI for app that draws a histogram ----
ui <-  fluidPage(
  theme = "themes.css",
  
  navbarPage(title= tags$a(href = "https://datascienceforthepublicgood.org", target = "_blank", # "_blank" opens the link in a new tab
                           tags$img(src = "DSPG_black-01.png", width = "120px", style="margin-top:-10px")
                           ),
             tabPanel("Overview",
                        box(title="Project Overview",
                        p("To provide the Census Bureau with information on the following."),
                        p("1. What data sources do state, U.S. territories, and District of Columbia, data centers use?"),
                        p("2. How do they use these data sources?"),
                        p("3. What are their future data needs?"))
                        ), 
             tabPanel("Topic Modeling"),
             
             tabPanel("Word Cloud",
                        br(),
                        sidebarLayout(sidebarPanel(
                          selectInput("dropdown3", "Which state's mission statement are you interested in?",
                                      list("All states" = "all", "Alaska"="AK", "Virginia"="VA", "California"= "CA"))),
                          mainPanel(textOutput("text3"),
                                    plotOutput("plot3")
                          ))),
             navbarMenu("Findings",
                        tabPanel("Demographics"),
                        tabPanel("Economy",
                                 br(),
                                 sidebarLayout(sidebarPanel(
                                   selectInput("dropdown2", "Which state are you interested in?",
                                               list("All states","Alaska", "Alabama", "California", "Colorado", "Connecticut")),
                                   strong("What categories are you interested in?"),
                                   checkboxInput("demo", "Demographics", TRUE),
                                   checkboxInput("econ", "Economy", TRUE),
                                   checkboxInput("house", "Housing", TRUE),
                                   checkboxInput("diver", "Diversity", TRUE)),
                                   mainPanel(#textOutput("text2"),
                                             plotOutput("plot2")
                                   ))),
                        tabPanel("Housing"),
                        tabPanel("Diversity"),
                        tabPanel("Health & Education")
             ),
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
  
  #output$text2 <- renderText({
  #    {paste("Distribution of Data Tools' Category in ", input$dropdown2)}
  #})
  
  output$plot2 <- renderPlot({
    if (input$dropdown2 == "All states"){
      employment = 0
      lf = 0
      wage = 0
      income = 0
      tax = 0
      job = 0
      economy = 0
      for (i in econ_data$Sub.categories){
        if (any(grepl("employment", i))){
          employment = employment + 1
        }
        if (any(grepl("labor force", i))){
          lf = lf + 1
        }
        if (any(grepl("wage", i))){
          wage = wage + 1
        }
        if (any(grepl("job", i))){
          job = job + 1
        }
        if (any(grepl("tax", i))){
          tax = tax + 1
        }
        if (any(grepl("income", i))){
          income = income + 1
        }
        if (any(grepl("economy", i))){
          economy = economy + 1
        }
      }
      category <- c("employment", "income", "tax", "labor force", "wage", "job","economy")
      counts <- c(employment, income, tax, lf, wage, job,economy)
      total_df = data.frame(category, counts)
      barplot(counts, names.arg = category, col = "steelblue", main = {paste("Distribution of Data Tools' Category in", input$dropdown2)}, xlab = "Category", ylab = "counts", cex.names = 0.9, ylim = c(0,60))
      text(x = c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), y = counts, labels = counts, pos = 3, cex = 0.8, col = "black")
    }
    else{
      employment = 0
      lf = 0
      wage = 0
      income = 0
      tax = 0
      job = 0
      economy = 0
      for (i in econ_data$Sub.categories[econ_data$State..Country == input$dropdown2 ]){
        if (any(grepl("employment", i))){
          employment = employment + 1
        }
        if (any(grepl("labor force", i))){
          lf = lf + 1
        }
        if (any(grepl("wage", i))){
          wage = wage + 1
        }
        if (any(grepl("job", i))){
          job = job + 1
        }
        if (any(grepl("tax", i))){
          tax = tax + 1
        }
        if (any(grepl("income", i))){
          income = income + 1
        }
        if (any(grepl("economy", i))){
          economy = economy + 1
        }
      }
      category <- c("employment", "income", "tax", "labor force", "wage", "job","economy")
      counts <- c(employment, income, tax, lf, wage, job,economy)
      total_df = data.frame(category, counts)
      barplot(counts, names.arg = category, col = "steelblue", main = {paste("Distribution of Data Tools' Category in", input$dropdown2)}, xlab = "Category", ylab = "counts", cex.names = 0.9, ylim = c(0,60))
      text(x = c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), y = counts, labels = counts, pos = 3, cex = 0.8, col = "black")
  }})
  
  output$text3 <- renderText({
    if (input$dropdown3 == "all"){
      "Word Cloud on all states' SDC Mission Statement"
    }
    else if (input$dropdown3 == "VA"){
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
