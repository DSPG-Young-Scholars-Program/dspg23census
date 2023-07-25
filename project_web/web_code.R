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

all_states <- c("All states", "Alabama", "Alaska","Arizona", "Arkansas", "California",
                "Colorado", "Connecticut", "Delaware", "District of Colombia", 
                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                "Massachusetts", "Michigan", "Minnesoda", "Mississippi", "Missouri",
                "Puerto Rico", "Guam")

econ_data <- read.csv("/Users/jianingcai/Downloads/Economy_compilation.csv")
econ_data$Sub.categories = tolower(econ_data$Sub.categories)

econ_category_plot <- function(selected_state) {
  # Initialize variables to store the counts
  if (selected_state == "All states"){
    data_to_use = econ_data$Sub.categories
  }
  else{
    data_to_use = econ_data$Sub.categories[econ_data$State..Country == selected_state]
  }
  employment <- 0
  lf <- 0
  wage <- 0
  income <- 0
  tax <- 0
  job <- 0
  economy <- 0
  for (i in data_to_use) {
    if (any(grepl("employment", i))) {
      employment <- employment + 1
    }
    if (any(grepl("labor force", i))) {
      lf <- lf + 1
    }
    if (any(grepl("wage", i))) {
      wage <- wage + 1
    }
    if (any(grepl("job", i))) {
      job <- job + 1
    }
    if (any(grepl("tax", i))) {
      tax <- tax + 1
    }
    if (any(grepl("income", i))) {
      income <- income + 1
    }
    if (any(grepl("economy", i))) {
      economy <- economy + 1
    }
  }
  # Create data frame for plotting
  category <- c("employment", "income", "tax", "labor force", "wage", "job", "economy")
  counts <- c(employment, income, tax, lf, wage, job, economy)
  total_df <- data.frame(category, counts)
  # Barplot
  barplot(counts, names.arg = category, col = "steelblue",
          main = paste("Distribution of Data Tools' Category in", selected_state),
          xlab = "Category", ylab = "counts", cex.names = 0.9, ylim = c(0, 60))
  # Add counts as text above the bars
  text(x = c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), y = counts,
       labels = counts, pos = 3, cex = 0.8, col = "black")
}



#------------------------------------------------------------------------------------
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
                          selectInput("dropdown2", "Which state's mission statement are you interested in?",
                                     all_states)),
                          mainPanel(textOutput("text2"),
                                    plotOutput("plot2")
                          ))),
             navbarMenu("Findings",
                        tabPanel("Demographics"),
                        tabPanel("Economy",
                                 br(),
                                 sidebarLayout(sidebarPanel(
                                   selectInput("dropdown3", "Which state are you interested in?",
                                               all_states),
                                   strong("What categories are you interested in?"),
                                   checkboxInput("demo", "Demographics", TRUE),
                                   checkboxInput("econ", "Economy", TRUE),
                                   checkboxInput("house", "Housing", TRUE),
                                   checkboxInput("diver", "Diversity", TRUE)),
                                   mainPanel(#textOutput("text3"),
                                             plotOutput("plot3")
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
  
  output$plot3 <- renderPlot({
    econ_category_plot(selected_state = input$dropdown3)
    
    })
  
  output$text2 <- renderText({
    {paste("Word cloud on", input$dropdown2)}
  })
  
}


shinyApp(ui = ui, server = server)
