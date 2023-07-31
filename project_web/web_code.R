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

library(wordcloud)
library(RColorBrewer)
library(reshape)
library(tm)
library(stringr)
library(usmap)
library(readxl)
library(gcookbook)




cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

all_states <- c("All Sample States", "Alabama", "Alaska","Arizona", "Arkansas", "California",
                "Colorado", "Connecticut", "Delaware", "District of Colombia", 
                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
                "Puerto Rico", "Guam")


mission_states <- c("All Sample States", "Alabama", "Arizona", "Arkansas", "California",
                    "Connecticut", "Delaware", "District of Colombia", "Florida", "Hawaii", "Indiana",
                    "Iowa", "Kansas", "Kentucky", "Maine", "Maryland", "Massachusetts", "Minnesota", 
                    "Mississippi", "Missouri", "Montana", "Nevada", "New Hampshire", "New Jersey", "New York", 
                    "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
                    "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas",
                    "Utah", "Vermont", "Wisconsin")

#Data Imports
#Econ data

econ_data <- read.csv("economy_compilation.csv")

econ_data$Sub.categories = tolower(econ_data$Sub.categories)

#Housing data
housing_data <- read.csv('/Users/marijkevandergeer/Documents/GitHub/dspg23census/Housing/housing_cleaned.csv')

#Health and education data
HE_data <- read.csv('/Users/marijkevandergeer/Documents/GitHub/dspg23census/Health_and_Education/HE_cleaned.csv')

#Mission statement data
mission_statements <- read.csv('/Users/marijkevandergeer/Documents/GitHub/dspg23census/Mission_Statements/mission_statements.csv')


#Map of host types for lead agencies
lead_types_map <- function() {
  hosts <- data.frame(state = mission_statements$State, type = mission_statements$Host_Type)
  host_map <- plot_usmap(data = hosts, values = "type") + labs(title="Type of Lead Agency by State")
  host_map
}


#Map of number of coordinating agencies
coord_num_map <- function() {
  coord <- data.frame(state = mission_statements$State, number = mission_statements$Coordinating)
  coord_map <- plot_usmap(data=coord, values="number") + labs(title="Number of Coordinating Agencies by State")
  coord_map
}


#Makes wordclouds using input of 'combo'
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


#Wordcloud of mission statements
mission_cloud <- function(state) {
  if(state=="All Sample States") {
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


#Bar graph of sub-category types
sub_cat_counts <- function(state, data_source) {
  if(state=="All Sample States") {
    sub_cats <- ggplot(data_source, aes(x=Sub.categories)) + geom_bar(fill="steelblue") + labs(x="Sub-Category", y="Counts") + theme(axis.text.x = element_text(angle = 25))
    sub_cats
  }
  else {
    State <- str_to_title(state)
    state_input <- data_source[data_source[, "State"]==state, ]
    state_df <- data.frame()
    state_df <- rbind(state_df, state_input)
    sub_cats <- ggplot(state_df, aes(x=Sub.categories)) + geom_bar(fill="steelblue") + labs(x="Sub-Category", y="Counts") + theme(axis.text.x = element_text(angle = 25))
    sub_cats
  }
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

sub_cat_and_tool <- function(selected_state){
  df_stack2 <- data.frame(
    tool = character(),
    sub = character(),
    count = numeric()
  )
  col_name = c("tool","sub","count")
  colnames(df_stack2) <- col_name
  
  if (selected_state == "All states" ){
    data_to_use = econ_data
  }
  else{
    data_to_use = econ_data[econ_data$State..Country== selected_state,]
  }
  
  for (i in 1:nrow(data_to_use)) {
    #---------------------------employment----------------------------
    if (any(grepl("employment", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("table download", "employment",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("table", "employment",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("report", "employment",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("map", "employment",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("data visualization", "employment",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #--------------------------income----------------------------
    if (any(grepl("income", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("table download", "income",46)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("table", "income",46)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("report", "income",46)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("map", "income",46)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("data visualization", "income",46)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---------------------------tax----------------------------
    if (any(grepl("tax", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("table download", "tax",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("table", "tax",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("report", "tax",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("map", "tax",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("data visualization", "tax",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---------------------------labor force----------------------------
    if (any(grepl("labor force", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("table download", "labor force",18)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("table", "labor force",18)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("report", "labor force",18)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("map", "labor force",18)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("data visualization", "labor force",18)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---------------------------wage----------------------------
    if (any(grepl("wage", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("table download", "wage",22)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("table", "wage",22)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("report", "wage",22)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("map", "wage",22)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("data visualization", "wage",22)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---------------------------job----------------------------
    if (any(grepl("job", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("table download", "job",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("table", "job",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("report", "job",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("map", "job",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("data visualization", "job",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---------------------------economy----------------------------
    if (any(grepl("economy", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("table download", "economy",38)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("table", "economy",38)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("report", "economy",38)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("map", "economy",38)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("data visualization", "economy",38)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
  }
  colnames(df_stack2) <- col_name
  
  ggplot(df_stack2, aes(x = sub, y = 1, fill = tool)) +
    geom_col() +
    scale_fill_manual(values = cbPalette) +
    xlab("sub-category")+
    ylab("count")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold"))+
    ggtitle("Different type of tools inside each sub-category")
}

pie_graph <-function(selected_state, data_table){
  if (selected_state == "All states" ){
    data_to_use = data_table
  }
  else{
    data_to_use = data_table[data_table$State..Country== selected_state,]
  }
  count_result <- data_to_use %>% group_by(Data.Source.Census..Standardized.) %>% summarize(count = n())
  count_result <- count_result[-1, ]
  countinue <- data_to_use %>% group_by(Data.Source.Non.Census..Standardized.) %>% summarize(count = n())
  countinue <- countinue[-1, ]
  colnames(count_result) <- c("data source", "count")
  colnames(countinue) <- c("data source", "count")
  combined_df <- rbind(count_result, countinue)
  sorted_df <- combined_df[order(- combined_df$count), ]
  pie(sorted_df$count , labels = sorted_df$`data source`, border="white", col=cbPalette, cex=0.5)
}


#Word cloud for variable names
variable_cloud <- function(state, data_source) {
  if(state=="All Sample States"){
    combo <- ""
    for (i in 1:nrow(data_source)) {
      combo <- paste(combo, data_source$Variables[i], sep="")
    }
  }
  else{
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
  if(state=="All Sample States"){
    combo <- ""
    for (i in 1:nrow(data_source)) {
      combo <- paste(combo, data_source$Tool.Name[i], sep="")
    }
  }
  else{
    combo <- ""
    for (i in 1:nrow(data_source)) {
      if(data_source$State[i]==state) {
        combo <- paste(combo, data_source$Tool.Name[i], sep="")
      }
    }
  }
  cloud(combo)
}



#------------------------------------------------------------------------------------
# Define UI
ui <-  fluidPage(
  theme = "themes.css",
  tags$style(HTML("
    /* Custom CSS to center content */
    .center-content {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100%;
    }
  ")),
  tags$style(HTML("
    /* Custom CSS to align content evenly */
    .even-content {
      display: flex;
      justify-content: space-evenly;
      align-items: center;
      height: 100%;
    }
  ")),
  
  navbarPage(title= tags$a(href = "https://datascienceforthepublicgood.org", target = "_blank", # "_blank" opens the link in a new tab
                           tags$img(src = "DSPG_black-01.png", width = "120px", style="margin-top:-10px")
  ),
  tabPanel("Overview",
           div(class = "even-content",
               tags$a(href = "https://biocomplexity.virginia.edu/",
                      img(src = "biilogo.png", width = "120px")),
               p(style = "font-size: 25px; font-weight: bold;","Survey on State Data Use"),
               tags$a(href = "https://www.census.gov/",
                      img(src = "census.png", width = "65px"))),
           box(title="Project Overview",
               p("The goal of this project is to provide the Census Bureau with information on the following:"),
               p("1. What data sources do state, U.S. territories, and District of Columbia, data centers use?"),
               p("2. How do they use these data sources?"),
               p("3. What are their future data needs?")), 
           box(title="State Data Centers",
               p("State Data Centers are... Every state has a lead agency and various coordinating agenices. Agencies are hosted by different types of organizations. "),
               plotOutput("overview_plot1"),
               p("*Other includes 501(c) nonprofits and public-private partnerships"),
               plotOutput("overview_plot2"))), 
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
           p("Out of all 56 SDCs, 39 had mission statements that related to the work of the SDC."),
           sidebarLayout(sidebarPanel(
             selectInput("dropdownM", "Which state's mission statement are you interested in?",
                         mission_states)),
             mainPanel(textOutput("mission_text1"),
                       plotOutput("mission_plot1")
             ))),
  navbarMenu("Findings",
             tabPanel("Demographics"),
             tabPanel("Economy",
                      br(),
                      sidebarLayout(sidebarPanel(
                        selectInput("dropdown1", "Which state are you interested in?",
                                    all_states),
                        strong("What categories are you interested in?"),
                        checkboxInput("demo", "Demographics", TRUE),
                        checkboxInput("econ", "Economy", TRUE),
                        checkboxInput("house", "Housing", TRUE),
                        checkboxInput("diver", "Diversity", TRUE)),
                        mainPanel(plotOutput("fin_econ_plot1")
                        ))),
             tabPanel("Housing",
                      br(),
                      sidebarLayout(sidebarPanel(
                        selectInput("dropdownH", "Which state are you interested in?",
                                    all_states)),
                        mainPanel(textOutput("fin_hous_text1"),
                                  plotOutput("fin_hous_plot1"),
                                  textOutput("fin_hous_text2"),
                                  plotOutput("fin_hous_plot2"),
                                  textOutput("fin_hous_text3"),
                                  plotOutput("fin_hous_plot3")
                        ))),
             tabPanel("Diversity"),
             tabPanel("Health & Education",
                      br(),
                      sidebarLayout(sidebarPanel(
                        selectInput("dropdownHE", "Which state are you interested in?",
                                    all_states)),
                        mainPanel(textOutput("fin_HE_text1"),
                                  plotOutput("fin_HE_plot1"),
                                  textOutput("fin_HE_text2"),
                                  plotOutput("fin_HE_plot2"),
                                  textOutput("fin_HE_text3"),
                                  plotOutput("fin_HE_plot3")
                        )))),
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
  output$overview_plot1 <- renderPlot({lead_types_map()})
  output$overview_plot2 <- renderPlot({coord_num_map()})
  
  #Topic Modeling-BERT
  
  #Mission Statements
  output$mission_text1 <- renderText({{paste("Word cloud on", input$dropdownM, "mission statement.")}})
  output$mission_plot1 <- renderPlot({mission_cloud(state=input$dropdownM)})
  
  #Economy Findings
  output$fin_econ_plot1 <- renderPlot({econ_category_plot(selected_state = input$dropdown1)})
  

  #Housing Findings
  output$fin_hous_text1 <- renderText({{paste("Type of Sub-Category for: ", input$dropdownH)}})
  output$fin_hous_plot1 <- renderPlot({sub_cat_counts(state=input$dropdownH, data_source = housing_data)})
  output$fin_hous_text2 <- renderText({{paste("Word cloud on tool names for: ", input$dropdownH)}})
  output$fin_hous_plot2 <- renderPlot({tool_cloud(state=input$dropdownH, data_source = housing_data)})
  output$fin_hous_text3 <- renderText({{paste("Word cloud on variables for: ", input$dropdownH)}})
  output$fin_hous_plot3 <- renderPlot({variable_cloud(state=input$dropdownH, data_source = housing_data)})
  
  #Health/Education Findings
  output$fin_HE_text1 <- renderText({{paste("Type of Sub-Category for: ", input$dropdownHE)}})
  output$fin_HE_plot1 <- renderPlot({sub_cat_counts(state=input$dropdownHE, data_source = HE_data)})
  output$fin_HE_text2 <- renderText({{paste("Word cloud on tool names for: ", input$dropdownHE)}})
  output$fin_HE_plot2 <- renderPlot({tool_cloud(state=input$dropdownHE, data_source = HE_data)})
  output$fin_HE_text3 <- renderText({{paste("Word cloud on variables for: ", input$dropdownHE)}})
  output$fin_HE_plot3 <- renderPlot({variable_cloud(state=input$dropdownHE, data_source = HE_data)})

  output$plot3_1 <- renderPlot({
    econ_category_plot(selected_state = input$dropdown3)
    
    })
  
  output$plot3_2 <- renderPlot({
    sub_cat_and_tool(selected_state = input$dropdown3)
  })
  
  output$plot3_3 <- renderPlot({
    pie_graph(selected_state = input$dropdown3, data_table = econ_data)
  })
  
  output$text2 <- renderText({
    {paste("Word cloud on", input$dropdown2)}
  })

  
}


shinyApp(ui = ui, server = server)
