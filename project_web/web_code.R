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
library(wordcloud)
library(RColorBrewer)
library(reshape)
library(tm)
library(stringr)
library(usmap)
library(readxl)


all_states <- c("All states", "Alabama", "Alaska","Arizona", "Arkansas", "California",
                "Colorado", "Connecticut", "Delaware", "District of Colombia", 
                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
                "Puerto Rico", "Guam")

states <- tolower(all_states)

#Data Imports
#Econ data
econ_data <- read.csv("/Users/marijkevandergeer/Downloads/economy_data.csv")
econ_data$Sub.categories = tolower(econ_data$Sub.categories)

#Housing data
housing_data <- read.csv('/Users/marijkevandergeer/Documents/GitHub/dspg23census/Housing/housing_cleaned.csv')

#Health and education data
HE_data <- read.csv('/Users/marijkevandergeer/Documents/GitHub/dspg23census/Health_and_Education/HE_cleaned.csv')

#Mission statement data
mission_statements <- read.csv('/Users/marijkevandergeer/Documents/GitHub/dspg23census/Mission_Statements/mission_statements.csv')

#Makes wordclouds
cloud <- function(combo) {
  #Turns string into corpus of words
  docs <- Corpus(VectorSource(combo))
  
  #Cleaning of corpus
  docs <- docs %>% tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  #Turns corpus into term-document-matrix
  dtm <- TermDocumentMatrix(docs)
  mtx <- as.matrix(dtm)
  words <- sort(rowSums(mtx), decreasing = TRUE)
  df <- data.frame(word = names(words), freq=words)
  
  #Creates wordcloud
  set.seed(33)
  wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0, colors = brewer.pal(4, "Set1"))
}

#Plot for economic data
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


#Word cloud for variable names
#Input is designated state
variable_cloud <- function(state, data_source) {
  if(state=="all states"){
    combo <- ""
    for (i in 1:nrow(data_source)) {
      combo <- paste(combo, data_source$Variables[i], sep="")
    }
  }
  else{
    #Combines all variables into one string
    combo <- ""
    for (i in 1:nrow(data_source)) {
      if(data_source$State[i]==state) {
        combo <- paste(combo, data_source$Variables[i], sep="")
      }
    }
  }
  cloud(combo)
}


#Wordcloud for tool names
tool_cloud <- function(state, data_source) {
  if(state=="all states"){
    combo <- ""
    for (i in 1:nrow(data_source)) {
      combo <- paste(combo, data_source$Tool.Name[i], sep="")
    }
  }
  else{
    #Combines all variables into one string
    combo <- ""
    for (i in 1:nrow(data_source)) {
      if(data_source$State[i]==state) {
        combo <- paste(combo, data_source$Tool.Name[i], sep="")
      }
    }
  }
  cloud(combo)
}


#Creates a bar graph of sub-category types
sub_cat_counts <- function(state, data_source) {
  if(state=="all states") {
    sub_cats <- ggplot(data_source, aes(x=Sub.categories)) + geom_bar(fill="steelblue") + labs(x="Sub-Categories", y="Count", title="Types of Sub-Categories") + theme(axis.text.x = element_text(angle = 25))
    sub_cats
  }
  else {
    State <- str_to_title(state)
    state_input <- data_source[data_source[, "State"]==state, ]
    state_df <- data.frame()
    state_df <- rbind(state_df, state_input)
    sub_cats <- ggplot(state_df, aes(x=Sub.categories)) + geom_bar(fill="steelblue") + labs(x="Sub-Categories", y="Count", title="Types of Sub-Categories") + theme(axis.text.x = element_text(angle = 25))
    sub_cats
  }
}

#map of host types for lead agencies
lead_types_map <- function() {
  hosts <- data.frame(state = mission_statements$State, type = mission_statements$Host_Type)
  host_map <- plot_usmap(data = hosts, values = "type") + labs(title="Type of Lead Agency")
  host_map
}

#Map of number of coordinating agencies
coord_num_map <- function() {
  coord <- data.frame(state = mission_statements$State, number = mission_statements$Coordinating)
  coord_map <- plot_usmap(data=coord, values="number") + labs(title="Number of Coordinating Agencies")
  coord_map
}

mission_cloud <- function(state) {
  if(state=="All states") {
    combo <- ""
    for (i in 1:nrow(mission_statements)) {
      if(mission_statements$Statement_Type[i]=='SDC') {
        print(mission_statements$Mission_Statment_Text[i])
        combo <- paste(combo, mission_statements$Mission_Statment_Text[i], sep="")
      }
    }
  }
  else {
    combo <- ""
    for (i in 1:nrow(mission_statements)) {
      if(mission_statements$State[i]==state){
        if(mission_statements$Statement_Type[i]=='SDC') {
          print(mission_statements$Mission_Statment_Text[i])
          combo <- paste(combo, mission_statements$Mission_Statment_Text[i], sep="")
        }
      }
    }
  }
  cloud(combo)
}

#------------------------------------------------------------------------------------
# Define UI for app that draws a histogram ----
ui <-  fluidPage(
  theme = "themes.css",
  
  navbarPage(title= tags$a(href = "https://datascienceforthepublicgood.org", target = "_blank", # "_blank" opens the link in a new tab
                           tags$img(src = "DSPG_black-01.png", width = "120px", style="margin-top:-10px")
                           ),
             tabPanel("Overview",
                      br(),
                        box(title="Project Overview",
                        p("The goal of this project is to provide the Census Bureau with information on the following:"),
                        p("1. What data sources do state, U.S. territories, and District of Columbia, data centers use?"),
                        p("2. How do they use these data sources?"),
                        p("3. What are their future data needs?")), 
                        box(title="State Data Centers",
                        p("State Data Centers are... Every state has a lead agency and various coordinating agenices. Agencies are hosted by different types of organizations. "),
                        plotOutput("plot1"),
                        p("*Other includes 501(c) nonprofits and public-private partnerships"),
                        plotOutput("plot2")),
                        ), 
             navbarMenu("Topic Modeling",
                      tabPanel("Gensim"),
                      tabPanel("BERT",
                               box(title="BERT",
                               p("We applied BERT to the top 5 State Constitutions with the most amendments."),
                               p("1.California"),
                               p("2.Hawaii"),
                               p("3.Maryland"),
                               p("4.Oregon"),
                               p("5.Texas")),
                               box(title="BERT Example: California Data"))),
             
             tabPanel("Mission Statements",
                        br(),
                        p("Out of all 56 SDCs, 50 had mission statements that related to the work of the SDC."),
                        sidebarLayout(sidebarPanel(
                          selectInput("dropdown1", "Which state's mission statement are you interested in?",
                                     all_states)),
                          mainPanel(textOutput("text1"),
                                    plotOutput("plot10")
                          ))),
             navbarMenu("Findings",
                        tabPanel("Demographics"),
                        tabPanel("Economy",
                                 br(),
                                 sidebarLayout(sidebarPanel(
                                   selectInput("dropdown2", "Which state are you interested in?",
                                               all_states),
                                   strong("What categories are you interested in?"),
                                   checkboxInput("demo", "Demographics", TRUE),
                                   checkboxInput("econ", "Economy", TRUE),
                                   checkboxInput("house", "Housing", TRUE),
                                   checkboxInput("diver", "Diversity", TRUE)),
                                   mainPanel(plotOutput("plot3")
                                   ))),
                        tabPanel("Housing",
                                 br(),
                                 sidebarLayout(sidebarPanel(
                                   selectInput("dropdown4", "Which state are you interested in?",
                                              states)),
                                   mainPanel(textOutput("text4"),
                                             plotOutput("plot4"),
                                             textOutput("text5"),
                                             plotOutput("plot5"),
                                             textOutput("text6"),
                                             plotOutput("plot6")
                                   ))),
                        tabPanel("Diversity"),
                        tabPanel("Health & Education",
                                 br(),
                                 sidebarLayout(sidebarPanel(
                                   selectInput("dropdown5", "Which state are you interested in?",
                                               states)),
                                   mainPanel(textOutput("text7"),
                                             plotOutput("plot7"),
                                             textOutput("text8"),
                                             plotOutput("plot8"),
                                             textOutput("text9"),
                                             plotOutput("plot9")
                                   ))),
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

  #Overview
  output$plot1 <- renderPlot({lead_types_map()})
  output$plot2 <- renderPlot({coord_num_map()})
  
  #Topic Modeling-BERT
  
  
  #Word Cloud
  output$text1 <- renderText({{paste("Word cloud on", input$dropdown1, "mission statement.")}})
  output$plot10 <- renderPlot({mission_cloud(state=input$dropdown1)})
  
  #Economy Findings
  output$plot3 <- renderPlot({econ_category_plot(selected_state = input$dropdown2)})
  
  #Housing Findings
  output$text4 <- renderText({{paste("Data Sub-Categories for ", input$dropdown4)}})
  output$plot4 <- renderPlot({sub_cat_counts(state=input$dropdown4, data_source = housing_data)})
  output$text5 <- renderText({{paste("Word cloud on tool names for ", input$dropdown4)}})
  output$plot5 <- renderPlot({tool_cloud(state=input$dropdown4, data_source = housing_data)})
  output$text6 <- renderText({{paste("Word cloud on variables for ", input$dropdown4)}})
  output$plot6 <- renderPlot({variable_cloud(state=input$dropdown4, data_source = housing_data)})
  
  #Health/Education Findings
  output$text7 <- renderText({{paste("Data Sub-Categories for ", input$dropdown4)}})
  output$plot7 <- renderPlot({sub_cat_counts(state=input$dropdown4, data_source = HE_data)})
  output$text8 <- renderText({{paste("Word cloud on tool names for ", input$dropdown4)}})
  output$plot8 <- renderPlot({tool_cloud(state=input$dropdown4, data_source = HE_data)})
  output$text9 <- renderText({{paste("Word cloud on variables for ", input$dropdown5)}})
  output$plot9 <- renderPlot({variable_cloud(state=input$dropdown5, data_source = HE_data)})
  
}


shinyApp(ui = ui, server = server)
