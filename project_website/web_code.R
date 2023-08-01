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
library(knitr)
library(kableExtra)




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
#Demo data
dem_data <- read.csv("Finding_Demographics/SDC_Demographics.csv")
dem_data$Sub.categories = tolower(dem_data$Sub.categories)

#Econ data
econ_data <- read.csv("Finding_Economy/economy_compilation.csv")
econ_data$Sub.categories = tolower(econ_data$Sub.categories)

#Housing data
housing_data <- read.csv('Finding_Housing/housing_cleaned.csv')

#Health and education data
HE_data <- read.csv('Finding_Health_and_Education/HE_cleaned.csv')

#Mission statement data
mission_statements <- read.csv('Mission_Statements/mission_statements.csv')


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


#Bar graph of sub-category types (For Housing & Health/Education)
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
  if (selected_state == "All Sample States"){
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
  category <- c("Employment", "Income", "Tax", "Labor Force", "Wage", "Job", "Economy")
  counts <- c(employment, income, tax, lf, wage, job, economy)
  total_df <- data.frame(category, counts)
  # Barplot
  barplot(counts, names.arg = category, col = "steelblue",
          main = paste("Types of Sub-category:", selected_state),
          xlab = "Category: Economy", ylab = "Counts", cex.names = 0.9, ylim = c(0, 60))
  # Add counts as text above the bars
  text(x = c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), y = counts,
       labels = counts, pos = 3, cex = 0.8, col = "black")
}

econ_sub_cat_and_tool <- function(selected_state){
  df_stack2 <- data.frame(
    Tools = character(),
    sub = character(),
    count = numeric()
  )
  col_name = c("Tools","sub","count")
  colnames(df_stack2) <- col_name
  
  if (selected_state == "All Sample States" ){
    data_to_use = econ_data
  }
  else{
    data_to_use = econ_data[econ_data$State..Country== selected_state,]
  }
  
  for (i in 1:nrow(data_to_use)) {
    #---------------------------employment----------------------------
    if (any(grepl("employment", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("Table Download", "Employment",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("Table", "Employment",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("Report", "Employment",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("Map", "Employment",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("Data Visualization", "Employment",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #--------------------------income----------------------------
    if (any(grepl("income", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("Table Download", "Income",46)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("Table", "Income",46)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("Report", "Income",46)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("Map", "Income",46)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("Data Visualization", "Income",46)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---------------------------tax----------------------------
    if (any(grepl("tax", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("Table Download", "Tax",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("Table", "Tax",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("Report", "Tax",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("Map", "Tax",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("Data Visualization", "Tax",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---------------------------labor force----------------------------
    if (any(grepl("labor force", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("Table Download", "Labor Force",18)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("Table", "Labor Force",18)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("Report", "Labor Force",18)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("Map", "Labor Force",18)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("Data Visualization", "Labor Lorce",18)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---------------------------wage----------------------------
    if (any(grepl("wage", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("Table Download", "Wage",22)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("Table", "Wage",22)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("Report", "Wage",22)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("Map", "Wage",22)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("Data Visualization", "Wage",22)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---------------------------job----------------------------
    if (any(grepl("job", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("Table Download", "Job",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("Table", "Job",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("Report", "Job",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("Map", "Job",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("Data Visualization", "Job",9)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---------------------------economy----------------------------
    if (any(grepl("economy", data_to_use[i,3]))) {
      if(any(grepl("table download", data_to_use[i,4]))){
        new_row <- c("Table Download", "Economy",38)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("table", data_to_use[i,4]))){
        new_row <- c("Table", "Economy",38)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("Report", "Economy",38)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("Map", "Economy",38)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("Data Visualization", "Economy",38)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
  }
  colnames(df_stack2) <- col_name
  
  ggplot(df_stack2, aes(x = sub, y = 1, fill = Tools)) +
    geom_col() +
    scale_fill_manual(values = cbPalette) +
    xlab("Sub-categories: Economy")+
    ylab("Counts")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold"))+
    ggtitle("Different type of tools inside each sub-category of Economy")
}

econ_pie_graph_census<- function(selected_state, data_table) {
  if (selected_state == "All Sample States") {
    data_to_use = data_table
  } else {
    data_to_use = data_table[data_table$State..Country == selected_state, ]
  }
  
  count_result <- data_to_use %>% group_by(Data.Source.Census..Standardized.) %>% summarize(count = n())
  colnames(count_result) <- c("data source", "count")
  count_result <- count_result[count_result$`data source` != "", ]

  sorted_df <- count_result[order(- count_result$count), ]
  
  # Adding a title to the pie graph
  title <- paste("Economy Data Census Source (Census) Distribution in", selected_state)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  pie(sorted_df$count, labels = sorted_df$`data source`, border = "white", col = cbPalette, cex = 1, main = title)
}

econ_pie_graph_noncensus <- function(selected_state, data_table) {
  if (selected_state == "All Sample States") {
    data_to_use = data_table
  } else {
    data_to_use = data_table[data_table$State..Country == selected_state, ]
  }
  
  countinue <- data_to_use %>% group_by(Data.Source.Non.Census..Standardized.) %>% summarize(count = n())
  colnames(countinue) <- c("data source", "count")
  countinue <- countinue[countinue$`data source` != "", ]

  sorted_df <- countinue[order(- countinue$count), ]
  
  # Adding a title to the pie graph
  title <- paste("Economy Data Source (Non Census) Distribution in", selected_state)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  pie(sorted_df$count, labels = sorted_df$`data source`, border = "white", col = cbPalette, cex = 1, main = title)
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

# Plot 1
dem_category_plot <- function(selected_state) {
  # Initialize variables to store the counts
  if (selected_state == "All Sample States"){
    data_to_use = dem_data$Sub.categories
  }
  else{
    data_to_use = dem_data$Sub.categories[dem_data$State..Country == selected_state]
  }
  estimates <- 0
  projections <- 0
  
  for (i in data_to_use) {
    if (any(grepl("estimates", i))) {
      estimates <- estimates + 1
    }
    if (any(grepl("projections", i))) {
      projections <- projections + 1
    }
  }
  # Create data frame for plotting
  category <- c("Estimates", "Projections")
  counts <- c(estimates, projections)
  total_df <- data.frame(category, counts)
  # Barplot
  barplot(counts, names.arg = category, col = "steelblue",
          main = paste("Types of Sub-category:", selected_state),
          xlab = "Category: Demographics", ylab = "Counts", cex.names = 0.9, ylim = c(0, 250))
  # Add counts as text above the bars
  text(x = c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), y = counts,
       labels = counts, pos = 3, cex = 0.8, col = "black")
}

# Plot 2
dem_data$Tool = tolower(dem_data$Tool)

dem_sub_cat_and_tool <- function(selected_state){
  df_stack2 <- data.frame(
    tool = character(),
    sub = character(),
    count = numeric()
  )
  col_name = c("tool","sub","count")
  colnames(df_stack2) <- col_name
  
  if (selected_state == "All Sample States" ){
    data_to_use = dem_data
  }
  else{
    data_to_use = dem_data[dem_data$State..Country== selected_state,]
  }
  
  for (i in 1:nrow(data_to_use)) {
    #---------------------------estimates----------------------------
    if (any(grepl("estimates", data_to_use[i,3]))) {
      if(any(grepl("viewer", data_to_use[i,4]))){
        new_row <- c("Table", "Estimates",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("download", data_to_use[i,4]))){
        new_row <- c("Table Download", "Estimates",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("Report", "Estimates",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("Map", "Estimates",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("Data Visualization", "Estimates",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("infographic", data_to_use[i,4]))){
        new_row <- c("Infographic", "Estimates",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("crosswalk", data_to_use[i,4]))){
        new_row <- c("Crosswalk", "Estimates",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---------------------------projections----------------------------
    if (any(grepl("projections", data_to_use[i,3]))) {
      if(any(grepl("viewer", data_to_use[i,4]))){
        new_row <- c("Table", "Projections",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("download", data_to_use[i,4]))){
        new_row <- c("Table Download", "Projections",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("report", data_to_use[i,4]))){
        new_row <- c("Report", "Projections",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("map", data_to_use[i,4]))){
        new_row <- c("Map", "Projections",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("visualization", data_to_use[i,4]))){
        new_row <- c("Data Visualization", "Projections",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("infographic", data_to_use[i,4]))){
        new_row <- c("Infographic", "Projections",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("crosswalk", data_to_use[i,4]))){
        new_row <- c("Crosswalk", "Projections",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
  }
  colnames(df_stack2) <- col_name
  
  ggplot(df_stack2, aes(x = sub, y = 1, fill = tool)) +
    geom_col() +
    scale_fill_manual(values = cbPalette) +
    xlab("Sub-categories: Demographics")+
    ylab("Counts")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.4, face = "bold"))+
    ggtitle("Different type of tools inside each sub-category of Demographics")
}


# Plot 3
dem_census_source <- function(selected_state){
  df_stack3 <- data.frame(
    source = character()
  )
  col_name = c("source")
  colnames(df_stack3) <- col_name
  
  if (selected_state == "All Sample States" ){
    data_to_use = dem_data
  }
  else{
    data_to_use = dem_data[dem_data$State..Country== selected_state,]
  }
  
  for (i in 1:nrow(data_to_use)) {
    #---------------------------Census Sources----------------------------
    if(any(grepl("Bureau", data_to_use[i,12]))){
      new_row <- c("Census Bureau")
      df_stack3 <- rbind(df_stack3, new_row)
    }
    if(any(grepl("Decennial", data_to_use[i,12]))){
      new_row <- c("Decennial Census")
      df_stack3 <- rbind(df_stack3, new_row)
    }
    if(any(grepl("Redistricting", data_to_use[i,12]))){
      new_row <- c("Census Redistricting Files")
      df_stack3 <- rbind(df_stack3, new_row)
    }
    if(any(grepl("ACS", data_to_use[i,12]))){
      new_row <- c("American Community Survey")
      df_stack3 <- rbind(df_stack3, new_row)
    }
    if(any(grepl("CPS", data_to_use[i,12]))){
      new_row <- c("Current Population Survey")
      df_stack3 <- rbind(df_stack3, new_row)
    }
  }
  colnames(df_stack3) <- col_name 
  source_types <- df_stack3 %>% group_by(source)%>%
    summarise(count = n())
  print(source_types)
  pie(source_types$count , labels = source_types$source, border="white", col=cbPalette, cex=0.5)
}


# Plot 4
dem_non_census_source <- function(selected_state){
  df_stack3 <- data.frame(
    source = character()
  )
  col_name = c("source")
  colnames(df_stack3) <- col_name
  
  if (selected_state == "All Sample States" ){
    data_to_use = dem_data
  }
  else{
    data_to_use = dem_data[dem_data$State..Country== selected_state,]
  }
  
  # Split the rows by commas and drop duplicates
  sources <- unique(unlist(strsplit(data_to_use$Data.Sources.Non.Census2, ",\\s*")))
  
  # Create a new data frame with a single column named "source" containing the sources
  df_stack3 <- data.frame(Source = sources)
  
  # Display the data frame using kable
  kable(df_stack3, format = "html") %>%
    kable_styling(full_width = FALSE) # You can set 'full_width = TRUE' for a wider table
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
  tags$style(HTML("
    .panel-default {
      border: none;
      box-shadow: none;
    }

  ")),
  
  navbarPage(title= tags$a(href = "https://biocomplexity.virginia.edu/data-science-public-good-young-scholars-program", target = "_blank", # "_blank" opens the link in a new tab
                           tags$img(src = "DSPG_black-01.png", width = "120px", style="margin-top:-10px")
                           ),
             tabPanel("Overview",
                      div(
                          tags$a(href = "https://biocomplexity.virginia.edu/",
                                 img(src = "biilogo.png", width = "170px")),
                          p(style = "font-size: 30px; font-weight: bold; color: #1B3766;text-align: center;","Survey on State Data Use"),
                          #tags$a(href = "https://www.census.gov/",
                           #      img(src = "census.png", width = "65px")),
                      ),
                      panel(h3("Project Overview", style = "color: #1B3766;"),
                            p("The goal of our project is to assist the U.S. Census Bureau in the creation of their Curated Data Enterprise, a tool that will combine data from multiple Census sources, to create a cohesive data lake that can be used as a tool by State governments."),
                            p("To accomplish this, we’ve focused on identifying how state governments use data and identifying what those data sources are, such as Census, State government, or private sources. We’ve started doing this through our data discovery process, which has included a review of individual State Constitutions, and as requested by the Census Bureau, we’re conducting a review of State Data Centers."),
                            p("The overall objective of this project is to report findings that the Census Bureau can use to better address state government data needs and to create a tool that facilitates user data access.   "),
                            
                      ),
                      panel(h3("Our Ideas", style = "color: #1B3766;"),
                            p("1. Text analysis of state constitutions and amendments."),
                            p("2. Text analysis of state data center mission statements."),
                            p("3. Email survey sent to all 56 The Federal-State Cooperative for Population Estimates (FSCPE) contacts."),
                            p("4. Evaluation of state, U.S. territories, and District of Columbia data centers."),
                            p("5. Search of UVA library databases: Policy Commons Database (Policy File Index Database, State and Local Government Databases. Policy Map Customer Stories)")
        
                      ),
                      panel(h3("Who We Are", style = "color: #1B3766;"),
                            h4("University of Virginia, Biocomplexity Institute, Social and Decision Analytics Division", style = "color: #E57200;"),
                            p("The Social and Decision Analytics Division (SDAD) is one of three research divisions within the Biocomplexity Institute and Initiative at the University of Virginia. SDAD combines expertise in statistics and social and behavioral sciences to develop evidence-based research and quantitative 
                              methods to inform policy decision-making and evaluation. The researchers at SDAD span many disciplines including statistics, economics, sociology, psychology, political science, policy, health IT, public health, program evaluation, and data science. The SDAD office is located near our nation's 
                              capital in Arlington, VA. You can learn more about us at", 
                              tags$a(href = "https://biocomplexity.virginia.edu/social-decision-analytics", "here.", style = "display: inline")),
                            h4("Data Science for the Public Good Program",  style = "color: #E57200;"),
                            p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at SDAD. Entering its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. 
                              DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy."),
                            h4("Team", style = "color: #E57200;"),
                            p("- Marijke van der Geer, Fourth Year at SDSU (Stats & DS)"),
                            p("- Jianing Cai, Fourth Year at UVA (CS & Math)"),
                            p("- Vicki Lancaster, Principal Scientist"),
                            p("- Neil Kattampallil, Research Scientist"),
                            p("- Treena Goswami *, Postdoctoral Research Associate"),
                            h6("*For more information on the project, please reach out to ",
                               tags$a(href = "mailto:gcm8gw@virgnia.edu", "gcm8gw@virgnia.edu"))
                        )),
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
             tabPanel("FSCPE Response",
                      ),
             navbarMenu("Findings",
                        tabPanel("Introduction"
                                 ),
                        tabPanel("Demographics",
                                 br(),
                                 sidebarLayout(sidebarPanel(
                                   selectInput("dropdownD", "Which state are you interested in?",
                                               all_states)
                                 ),
                                 mainPanel(plotOutput("fin_dem_1"),
                                           plotOutput("fin_dem_2"),
                                           plotOutput("fin_dem_3"),
                                           plotOutput("fin_dem_4")
                                           ))),
                        tabPanel("Economy",
                                 br(),
                                 sidebarLayout(sidebarPanel(
                                   selectInput("dropdown3", "Which state are you interested in?",
                                               all_states)
                                   ),
                                   mainPanel(plotOutput("fin_econ_plot1"),
                                             plotOutput("fin_econ_plot2"),
                                             plotOutput("fin_econ_plot3"),
                                             plotOutput("fin_econ_plot4")
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
                                             )))
                        )))
  
 

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  #Overview
  output$overview_plot1 <- renderPlot({lead_types_map()})
  output$overview_plot2 <- renderPlot({coord_num_map()})
  
  #Topic Modeling-BERT
  
  #Mission Statements
  output$mission_text1 <- renderText({{paste("Word cloud on", input$dropdownM, "mission statement.")}})
  output$mission_plot1 <- renderPlot({mission_cloud(state=input$dropdownM)})
  
  
  #Demographic Findings
  output$fin_dem_1 <- renderPlot({dem_category_plot(selected_state = input$dropdownD)})
  output$fin_dem_2 <- renderPlot({dem_sub_cat_and_tool(selected_state = input$dropdownD)})
  output$fin_dem_3 <- renderPlot({dem_census_source(selected_state = input$dropdownD)})
  output$fin_dem_4 <- renderPlot({dem_non_census_source(selected_state = input$dropdownD)})

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

  
  #Economy Findings
  output$fin_econ_plot1 <- renderPlot({econ_category_plot(selected_state = input$dropdown3)})
  output$fin_econ_plot2 <- renderPlot({econ_sub_cat_and_tool(selected_state = input$dropdown3)})
  output$fin_econ_plot3 <- renderPlot({econ_pie_graph_census(selected_state = input$dropdown3, data_table = econ_data)})
  output$fin_econ_plot4 <- renderPlot({econ_pie_graph_noncensus(selected_state = input$dropdown3, data_table = econ_data)})
  
}


shinyApp(ui = ui, server = server)
