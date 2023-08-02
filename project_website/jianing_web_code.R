#----------------------Import Packages ---------------------------
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
library(gcookbook)
library(knitr)
library(kableExtra)
#----------------------Global Vairables----------------------------------------
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


all_states <- c("All Sample States and Territories", "Alabama", "Alaska","Arizona", "Arkansas", "California",
                "Colorado", "Connecticut", "Delaware", "District of Colombia", 
                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
                "Puerto Rico", "Guam")


mission_states <- c("All Sample States and Territories", "Alabama", "Alaska", "Arizona", "Arkansas", "California",
                    "Connecticut", "Delaware", "District of Colombia", "Florida", "Hawaii", "Indiana",
                    "Iowa", "Kansas", "Kentucky", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                    "Mississippi", "Missouri", "Montana", "Nevada", "New Hampshire", "New Jersey", "New York", 
                    "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
                    "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas",
                    "Utah", "Vermont", "Wisconsin")

#-----------------------Data Imports-------------------------------
#Demo data
dem_data <- read.csv("Finding_Demographics/SDC_Demographics.csv")
dem_data$Sub.categories = tolower(dem_data$Sub.categories)
#Econ data
econ_data <- read.csv("Finding_Economy/economy_compilation.csv")
econ_data$Sub.categories = tolower(econ_data$Sub.categories)
#Housing data
housing_data <- read.csv('Finding_Housing/Housing_data.csv')
#Diversity data
div_data <- read.csv('Finding_Diversity/div_compilation.csv')
#Health and education data
HE_data <- read.csv('Finding_Health_and_Education/HE_data.csv')
#Mission statement data
mission_statements <- read.csv('Mission_Statements/mission_statements.csv')
#FSCPE Response data
fscpe <- read.csv('FSCPE Response.csv')

#-----------------------Mission Statement Cloud------------------------
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
  if(state=="All Sample States and Territories") {
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


#-----------------------Finding Intro Page--------------------------
#Maps
lead_types_map <- function() {
  custom_colors <- brewer.pal(4, "Set1")
  hosts <- data.frame(state = mission_statements$State, type = mission_statements$Host_Type)
  host_map <- plot_usmap(data = hosts, values = "type") + 
    labs(title = "Type of Lead Agency by State") + scale_fill_manual(values = cbPalette) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  host_map
}

#Map of number of coordinating agencies
coord_num_map <- function() {
  coord <- data.frame(state = mission_statements$State, number = mission_statements$Coordinating)
  coord_map <- plot_usmap(data = coord, values = "number") +
    labs(title = "Number of Coordinating Agencies by State") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
  coord_map
}

examined_states <- function() {
  custom_colors <- brewer.pal(2, "Set1")
  examined_SDC <- data.frame(state = mission_statements$State, value = mission_statements$Examined)
  examined_map <- plot_usmap(data = examined_SDC, values = "value") + 
    labs(title = "States That We Have Examined") + scale_fill_manual(values = cbPalette) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  examined_map
}


#-----------------------Finding Demographics Page-----------------------------------
# Plot 1 - Type of subcategories
dem_category_plot <- function(selected_state) {
  # Initialize variables to store the counts
  if (selected_state == "All Sample States and Territories"){
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
  
  
  # Create the ggplot bar plot
  ggplot(total_df, aes(x = category, y = counts)) +
    geom_bar(stat = "identity", position = "dodge", fill = "#0072B2") +
    labs(
      title = paste("Types of Sub-category:", selected_state),
      x = "Category: Demographics",
      y = "Counts"
    ) +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                            plot.title = element_text(hjust = 0.4, face = "bold"))
  # + coord_cartesian(ylim = c(0, 250))
}

# Plot 2 - Type of tools for subcategory

dem_data$Tool = tolower(dem_data$Tool)

dem_sub_cat_and_tool <- function(selected_state){
  df_stack2 <- data.frame(
    tool = character(),
    sub = character(),
    count = numeric()
  )
  col_name = c("tool","sub","count")
  colnames(df_stack2) <- col_name
  
  if (selected_state == "All Sample States and Territories" ){
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
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                            plot.title = element_text(hjust = 0.4, face = "bold"))+
    ggtitle("Different type of tools inside each sub-category of Demographics")
  
}

# Plot 3 - Geographic levels

dem_data$Geographic.Levels2 = tolower(dem_data$Geographic.Levels2)

dem_geography_plot <- function(selected_state) {
  df_stack3 <- data.frame(
    geography = character()
  )
  col_name = c("Geography")
  colnames(df_stack3) <- col_name
  
  
  # Initialize variables to store the counts
  if (selected_state == "All Sample States and Territories"){
    data_to_use = dem_data
  }
  else{
    data_to_use = dem_data[dem_data$State..Country == selected_state,]
  }
  
  
  for (i in 1:nrow(data_to_use)) {
    if(any(grepl("alaska", data_to_use[i,9]))){
      new_row <- c("Alaska Native")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("block", data_to_use[i,9]))) {
      new_row <- c("Block")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("group", data_to_use[i,9]))) {
      new_row <- c("Block Group")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("borough", data_to_use[i,9]))) {
      new_row <- c("Borough")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("census", data_to_use[i,9]))) {
      new_row <- c("Census Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("subarea", data_to_use[i,9]))) {
      new_row <- c("Census Subarea")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("census designated place", data_to_use[i,9]))) {
      new_row <- c("Census Designated Place")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("tract", data_to_use[i,9]))) {
      new_row <- c("Census Tract")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("town", data_to_use[i,9]))) {
      new_row <- c("City/Town")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("county", data_to_use[i,9]))) {
      new_row <- c("County")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("subdivision", data_to_use[i,9]))) {
      new_row <- c("County Subdivision")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("combined", data_to_use[i,9]))) {
      new_row <- c("Combined Statistical Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("congressional", data_to_use[i,9]))) {
      new_row <- c("Congressional District")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("development", data_to_use[i,9]))) {
      new_row <- c("Development District")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("economic", data_to_use[i,9]))) {
      new_row <- c("Economic Region")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("incorporated city", data_to_use[i,9]))) {
      new_row <- c("Incorporated City")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("incorporated place", data_to_use[i,9]))) {
      new_row <- c("Incorporated Place")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("jurisdiction", data_to_use[i,9]))) {
      new_row <- c("Jurisdiction")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("legislative", data_to_use[i,9]))) {
      new_row <- c("Legislative District")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("metropolitan", data_to_use[i,9]))) {
      new_row <- c("Metropolitan Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("micropolitan", data_to_use[i,9]))) {
      new_row <- c("Micropolitan Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("municipality", data_to_use[i,9]))) {
      new_row <- c("Municipality")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("nation", data_to_use[i,9]))) {
      new_row <- c("Nation")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("native", data_to_use[i,9]))) {
      new_row <- c("Native Village Statistical Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("corporation", data_to_use[i,9]))) {
      new_row <- c("Native Regional Corporation")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("neighborhood", data_to_use[i,9]))) {
      new_row <- c("Neighborhood Cluster")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("parish", data_to_use[i,9]))) {
      new_row <- c("Parish")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("places", data_to_use[i,9]))) {
      new_row <- c("Place")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("prosperity", data_to_use[i,9]))) {
      new_row <- c("Prosperity Region")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("puma", data_to_use[i,9]))) {
      new_row <- c("Public Use Microdata Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("school", data_to_use[i,9]))) {
      new_row <- c("School District")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("region", data_to_use[i,9]))) {
      new_row <- c("Region")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("state", data_to_use[i,9]))) {
      new_row <- c("State")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("substate", data_to_use[i,9]))) {
      new_row <- c("Substate Region")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("summary", data_to_use[i,9]))) {
      new_row <- c("Summary Level")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("township", data_to_use[i,9]))) {
      new_row <- c("Township")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("urbanized", data_to_use[i,9]))) {
      new_row <- c("Urbanized Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("rural", data_to_use[i,9]))) {
      new_row <- c("Urban/Rural")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("village", data_to_use[i,9]))) {
      new_row <- c("Village")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("ward", data_to_use[i,9]))) {
      new_row <- c("Ward")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("zcta", data_to_use[i,9]))) {
      new_row <- c("ZCTA")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("zip", data_to_use[i,9]))) {
      new_row <- c("Zip Code")
      df_stack3 <- rbind(df_stack3, new_row)}
  }
  colnames(df_stack3) <- col_name 
  geography_types <- df_stack3 %>% group_by(Geography)%>%
    summarise(count = n())
  
  
  ggplot(geography_types, aes(x = Geography, y = count)) +
    geom_col(width=.8) +
    scale_fill_manual(values = cbPalette) +
    labs(x="Geography: Demographics", y="Counts") + 
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                            plot.title = element_text(hjust = 0.4, face = "bold")) +coord_flip()+
    ggtitle("Types of Geographic Levels") 
  
  
}


# Plot 4 - Age of data 
# to be inserted

# Needed to make word clouds using input of 'combo'
cloud <- function(combo) {
  # Turns string into corpus of words
  docs <- Corpus(VectorSource(combo))
  
  # Cleaning of corpus
  docs <- docs %>% tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  # Turns corpus into term-document-matrix
  dtm <- TermDocumentMatrix(docs)
  mtx <- as.matrix(dtm)
  words <- sort(rowSums(mtx), decreasing = TRUE)
  df <- data.frame(word = names(words), freq=words)
  
  # Creates word cloud
  set.seed(33)
  wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0, colors = brewer.pal(4, "Set1"))
}


# Plot 5 - Word cloud for tool names

tool_cloud <- function(state, data_source) {
  if(state=="All Sample States and Territories"){
    combo <- ""
    for (i in 1:nrow(data_source)) {
      combo <- paste(combo, data_source[i,5], sep="")
    }
  }
  else{
    combo <- ""
    for (i in 1:nrow(data_source)) {
      if(data_source$State..Country[i]==state) {
        combo <- paste(combo, data_source[i,5], sep="")
      }
    }
  }
  cloud(combo)
}

# Plot 6 - Word cloud for list of variables

variable_cloud <- function(state, data_source) {
  if(state=="All Sample States and Territories"){
    combo <- ""
    for (i in 1:nrow(data_source)) {
      combo <- paste(combo, data_source[i,7], sep="")
    }
  }
  else{
    combo <- ""
    for (i in 1:nrow(data_source)) {
      if(data_source$State..Country[i]==state) {
        combo <- paste(combo, data_source[i,7], sep="")
      }
    }
  }
  cloud(combo)
}


# Plot 7 - Census Sources 

dem_census_source <- function(selected_state){
  df_stack3 <- data.frame(
    source = character()
  )
  col_name = c("source")
  colnames(df_stack3) <- col_name
  
  if (selected_state == "All Sample States and Territories" ){
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
  # Adding a title to the pie graph
  title <- paste("Demographic Data Source (Census) \n Distribution in", selected_state)
  pie(source_types$count , labels = source_types$source, border="white", col=cbPalette, cex=0.5, main=title)
}


# Plot 8 - Non Census sources
dem_non_census_source <- function(selected_state){
  df_stack3 <- data.frame(
    source = character()
  )
  col_name = c("source")
  colnames(df_stack3) <- col_name
  
  if (selected_state == "All Sample States and Territories" ){
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
  #kable(df_stack3, format = "html") %>%
  # kable_styling(full_width = FALSE) # You can set 'full_width = TRUE' for a wider table
}




# Plot 9 - Link to census data
# to be inserted

# Plot 10 - Historical data
# to be inserted
#----------------------Finding Economy Page------------------------
#econ geo level
econ_data$Geographic.Levels = tolower(econ_data$Geographic.Levels)
econ_geography_plot <- function(selected_state) {
  df_stack3 <- data.frame(
    geography = character()
  )
  col_name = c("Geography")
  colnames(df_stack3) <- col_name
  
  # Initialize variables to store the counts
  if (selected_state == "All Sample States and Territories"){
    data_to_use = econ_data
  }
  else{
    data_to_use = econ_data[econ_data$State..Country == selected_state,]
  }
  
  for (i in 1:nrow(data_to_use)) {
    if(any(grepl("House", data_to_use[i,7]))){
      new_row <- c("Alaska Native")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("county", data_to_use[i,7]))) {
      new_row <- c("County")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("metropolitan", data_to_use[i,7]))) {
      new_row <- c("Metropolitan \n Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("micropolitan", data_to_use[i,7]))) {
      new_row <- c("Micropolitan \n Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("region", data_to_use[i,7]))) {
      new_row <- c("Region")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("state", data_to_use[i,7]))) {
      new_row <- c("State")
      df_stack3 <- rbind(df_stack3, new_row)}
  }
  colnames(df_stack3) <- col_name 
  geography_types <- df_stack3 %>% group_by(Geography)%>%
    summarise(count = n())
  
  ggplot(geography_types, aes(x = Geography, y = count)) +
    geom_col(width = 0.8, fill = "#0072B2") +
    scale_fill_manual(values = "#0072B2") +
    labs(x = "Geography: Economy", y = "Counts") +  # Adjust the font size here
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.4, face = "bold"),
      axis.text.x = element_text(size = 14),  # Adjust the font size for x-axis labels here
      axis.text.y = element_text(size = 14),  # Adjust the font size for y-axis labels here
      axis.title.x = element_text(size = 14),  # Adjust the font size for x-axis label (x-axis title) here
      axis.title.y = element_text(size = 14)   # Adjust the font size for y-axis label (y-axis title) here
    ) +
    ggtitle(paste("Types of Geographic Levels", selected_state))
  
}

#econ age of data
econ_age_of_data <- function(state, data_source) {
  data_source$Age.of.data <- as.integer(data_source$Age.of.data)
  if(state=="All Sample States and Territories") {
    ggplot(data_source, aes(x=Age.of.data)) +
      geom_bar(width=0.7, col = "#999999", fill="#0072B2") +
      labs(x="Year of Latest Vintage", y="Counts") +
      theme_minimal()+
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1,size = 14),
        plot.title = element_text(hjust = 0.4, face = "bold"),
        axis.text.y = element_text(size = 14),  # Adjust the font size for y-axis labels here
        axis.title.x = element_text(size = 14),  # Adjust the font size for x-axis label (x-axis title) here
        axis.title.y = element_text(size = 14) 
      ) +
      ggtitle(paste("Age of Economy Data in", state))
  }
  else {
    State <- str_to_title(state)
    state_input <- data_source[data_source[, "State..Country"]==state, ]
    state_df <- data.frame()
    state_df <- rbind(state_df, state_input)
    state_df$Age.of.data <- as.integer(state_df$Age.of.data)
    ggplot(state_df, aes(x=Age.of.data)) + 
      geom_bar(width = 0.7, col = "#999999", fill="#0072B2") +
      labs(x="Year of Latest Vintage", y="Counts") + 
      theme_minimal()+
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1,size = 14),
        plot.title = element_text(hjust = 0.4, face = "bold"),
        axis.text.y = element_text(size = 14),  # Adjust the font size for y-axis labels here
        axis.title.x = element_text(size = 14),  # Adjust the font size for x-axis label (x-axis title) here
        axis.title.y = element_text(size = 14)   # Adjust the font size for y-axis label (y-axis title) here
      ) +
      ggtitle(paste("Age of Economy Data in", state))
  }
}
econ_direct_census_link <- function(selected_state, data_table) {
  if (selected_state == "All Sample States and Territories") {
    data_to_use <- data_table
  } else {
    data_to_use <- data_table[data_table$State..Country == selected_state, ]
  }
  
  count_result <- data_to_use %>% group_by(Direct.links.to.Census) %>% summarize(count = n())
  colnames(count_result) <- c("direct link", "count")
  count_result <- count_result[count_result$`direct link` != "", ]
  
  sorted_df <- count_result[order(-count_result$count), ]
  
  # Replace "N" with "No" and "Y" with "Yes"
  sorted_df$`direct link` <- ifelse(sorted_df$`direct link` == "N", "No", "Yes")
  
  # Adding a title to the pie graph
  title <- paste("Economy Data Census Link \n Distribution in", selected_state)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  pie(sorted_df$count, labels = sorted_df$`direct link`, border = "white", col = cbPalette, cex = 1, main = title)
}

econ_historical_data <- function(selected_state, data_table) {
  if (selected_state == "All Sample States and Territories") {
    data_to_use <- data_table
  } else {
    data_to_use <- data_table[data_table$State..Country == selected_state, ]
  }
  
  count_result <- data_to_use %>% group_by(Historical.data) %>% summarize(count = n())
  colnames(count_result) <- c("historical data", "count")
  count_result <- count_result[count_result$`historical data` != "", ]
  count_result <- count_result[count_result$`historical data` != "N/A", ]
  
  sorted_df <- count_result[order(-count_result$count), ]
  
  # Replace "N" with "No" and "Y" with "Yes"
  sorted_df$`historical data` <- ifelse(sorted_df$`historical data` == "N", "No", "Yes")
  
  # Adding a title to the pie graph
  title <- paste("Economy Historical Data \n Distribution in", selected_state)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  pie(sorted_df$count, labels = sorted_df$`historical data`, border = "white", col = cbPalette, cex = 1, main = title)
}

#Word cloud for variable names
econ_variable_cloud <- function(selected_state) {
  if (selected_state == "All Sample States and Territories") {
    data_to_use <- econ_data
  } else {
    data_to_use <- econ_data[econ_data$State..Country == selected_state, ]
  }
  combo <- ""
  for (i in 1:nrow(data_to_use)) {
    combo <- paste(combo, data_to_use$Variables.Used..list.all.that.apply.[i], sep="")
  }
  cloud(combo)
}

#Word cloud for variable names
econ_tool_name_cloud <- function(selected_state) {
  if (selected_state == "All Sample States and Territories") {
    data_to_use <- econ_data
  } else {
    data_to_use <- econ_data[econ_data$State..Country == selected_state, ]
  }
  combo <- ""
  for (i in 1:nrow(data_to_use)) {
    combo <- paste(combo, data_to_use$Name.of.tool[i], sep="")
  }
  cloud(combo)
}
#Plot for economic data
econ_category_plot <- function(selected_state) {
  # Initialize variables to store the counts
  if (selected_state == "All Sample States and Territories"){
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
  ggplot(total_df, aes(x = category, y = counts)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7, col = "#999999", fill = "#0072B2") +
    geom_text(aes(label = counts), position = position_stack(vjust = 0.5), vjust = -0.5, cex = 0.8, col = "black") +
    labs(title = paste("Types of Sub-category:", selected_state),
         x = "Category: Economy", y = "Counts") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  
}

econ_sub_cat_and_tool <- function(selected_state){
  df_stack2 <- data.frame(
    Tools = character(),
    sub = character(),
    count = numeric()
  )
  col_name = c("Tools","sub","count")
  colnames(df_stack2) <- col_name
  
  if (selected_state == "All Sample States and Territories" ){
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
        new_row <- c("Data Visualization", "Labor Force",18)
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
  
  bar1 <- ggplot(df_stack2, aes(x = sub, y = 1, fill = Tools)) +
    geom_col() +
    scale_fill_manual(values = cbPalette) +
    xlab("Sub-categories: Economy")+
    ylab("Counts")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold"))+
    ggtitle("Different type of tools inside each sub-category of Economy")
  
  bar1 + theme_minimal()
}

econ_pie_graph_census<- function(selected_state, data_table) {
  if (selected_state == "All Sample States and Territories") {
    data_to_use = data_table
  } else {
    data_to_use = data_table[data_table$State..Country == selected_state, ]
  }
  
  count_result <- data_to_use %>% group_by(Data.Source.Census..Standardized.) %>% summarize(count = n())
  colnames(count_result) <- c("data source", "count")
  count_result <- count_result[count_result$`data source` != "", ]
  # Assuming countinue$`data source` contains the labels you want to modify
  count_result$`data source` <- gsub("Small Area Income and Poverty Estimates", "Small Area Income \n and Poverty Estimates", count_result$`data source`)
  
  
  sorted_df <- count_result[order(- count_result$count), ]
  
  # Adding a title to the pie graph
  title <- paste("Economy Data Census Source (Census) \n Distribution in", selected_state)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  pie(sorted_df$count, labels = sorted_df$`data source`, border = "white", col = cbPalette, cex = 1, main = title)
}

econ_pie_graph_noncensus <- function(selected_state, data_table) {
  if (selected_state == "All Sample States and Territories") {
    data_to_use = data_table
  } else {
    data_to_use = data_table[data_table$State..Country == selected_state, ]
  }
  
  countinue <- data_to_use %>% group_by(Data.Source.Non.Census..Standardized.) %>% summarize(count = n())
  colnames(countinue) <- c("data source", "count")
  countinue <- countinue[countinue$`data source` != "", ]
  
  #sorted_df <- countinue[order(- countinue$count), ]
  
  # Adding a title to the pie graph
  title <- paste("Economy Data Source (Non Census) \n Distribution in", selected_state)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  #pie(sorted_df$count, labels = sorted_df$`data source`, border = "white", col = cbPalette, cex = 1, main = title)
  pie(countinue$count, labels = countinue$`data source`, border = "white", col = cbPalette, cex = 1, main = title)
}
#----------------------Finding Housing/HE Page-----------------
#Housing/Health/Education
#Bar graph of sub-category types
sub_cat_counts <- function(state, data_source) {
  if(state=="All Sample States and Territories") {
    sub_cats <- ggplot(data_source, aes(x=Sub.categories)) + geom_bar(width=0.7, col = "#999999", fill="#0072B2") + labs(x="Category", y="Counts") + theme_minimal() + theme(axis.text.x = element_text(hjust=1))
    sub_cats + coord_flip()
  }
  else {
    State <- str_to_title(state)
    state_input <- data_source[data_source[, "State..Country"]==state, ]
    state_df <- data.frame()
    state_df <- rbind(state_df, state_input)
    sub_cats <- ggplot(state_df, aes(x=Sub.categories)) + geom_bar(width=0.7, col = "#999999", fill="#0072B2") + labs(x="Category", y="Counts") + theme_minimal() + theme(axis.text.x = element_text(hjust=1))
    sub_cats + coord_flip()
  }
}

hous_sub_cat_and_tool <- function(selected_state){
  df_stack2 <- data.frame(
    Tools = character(),
    sub = character(),
    count = numeric())
  
  col_name = c("Tools","sub","count")
  colnames(df_stack2) <- col_name
  
  if (selected_state == "All Sample States and Territories" ){
    data_to_use = housing
  }
  else{
    data_to_use = housing[housing$State..Country== selected_state,]
  }
  
  for (i in 1:nrow(data_to_use)) {
    #---residential mobility---
    if (any(grepl("Residential Mobility", data_to_use[i,2]))) {
      if(any(grepl("Table", data_to_use[i,3]))){
        new_row <- c("Table", "Residential Mobility",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Table Download", data_to_use[i,3]))){
        new_row <- c("Table Download", "Residential Mobility",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Report", data_to_use[i,3]))){
        new_row <- c("Report", "Residential Mobility",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Map", data_to_use[i,3]))){
        new_row <- c("Map", "Residential Mobility",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Data Visualization", data_to_use[i,3]))){
        new_row <- c("Data Visualization", "Residential Mobility",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Infographic", data_to_use[i,3]))){
        new_row <- c("Infographic", "Residential Mobility",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---projections---
    if (any(grepl("Projections", data_to_use[i,2]))) {
      if(any(grepl("Table", data_to_use[i,3]))){
        new_row <- c("Table", "Projections",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Table Download", data_to_use[i,3]))){
        new_row <- c("Table Download", "Projections",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Report", data_to_use[i,3]))){
        new_row <- c("Report", "Projections",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Map", data_to_use[i,3]))){
        new_row <- c("Map", "Projections",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Data Visualization", data_to_use[i,3]))){
        new_row <- c("Data Visualization", "Projections",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Infographic", data_to_use[i,3]))){
        new_row <- c("Infographic", "Projections",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---permits---
    if (any(grepl("Permits", data_to_use[i,2]))) {
      if(any(grepl("Table", data_to_use[i,3]))){
        new_row <- c("Table", "Permits",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Table Download", data_to_use[i,3]))){
        new_row <- c("Table Download", "Permits",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Report", data_to_use[i,3]))){
        new_row <- c("Report", "Permits",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Map", data_to_use[i,3]))){
        new_row <- c("Map", "Permits",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Data Visualization", data_to_use[i,3]))){
        new_row <- c("Data Visualization", "Permits",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Infographic", data_to_use[i,3]))){
        new_row <- c("Infographic", "Permits",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #----occupancy---
    if (any(grepl("Occupancy", data_to_use[i,2]))) {
      if(any(grepl("Table", data_to_use[i,3]))){
        new_row <- c("Table", "Occupancy",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Table Download", data_to_use[i,3]))){
        new_row <- c("Table Download", "Occupancy",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Report", data_to_use[i,3]))){
        new_row <- c("Report", "Occupancy",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Map", data_to_use[i,3]))){
        new_row <- c("Map", "Occupancy",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Data Visualization", data_to_use[i,3]))){
        new_row <- c("Data Visualization", "Occupancy",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Infographic", data_to_use[i,3]))){
        new_row <- c("Infographic", "Occupancy",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---housing units---
    if (any(grepl("Housing Units", data_to_use[i,2]))) {
      if(any(grepl("Table", data_to_use[i,3]))){
        new_row <- c("Table", "Housing Units",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Table Download", data_to_use[i,3]))){
        new_row <- c("Table Download", "Housing Units",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Report", data_to_use[i,3]))){
        new_row <- c("Report", "Housing Units",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Map", data_to_use[i,3]))){
        new_row <- c("Map", "Housing Units",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Data Visualization", data_to_use[i,3]))){
        new_row <- c("Data Visualization", "Housing Units",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Infographic", data_to_use[i,3]))){
        new_row <- c("Infographic", "Housing Units",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }    
    #---housing prices/rent---
    if (any(grepl("Housing Prices / Rent", data_to_use[i,2]))) {
      if(any(grepl("Table", data_to_use[i,3]))){
        new_row <- c("Table", "Housing Prices / Rent",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Table Download", data_to_use[i,3]))){
        new_row <- c("Table Download", "Housing Prices / Rent",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Report", data_to_use[i,3]))){
        new_row <- c("Report", "Housing Prices / Rent",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Map", data_to_use[i,3]))){
        new_row <- c("Map", "Housing Prices / Rent",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Data Visualization", data_to_use[i,3]))){
        new_row <- c("Data Visualization", "Housing Prices / Rent",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Infographic", data_to_use[i,3]))){
        new_row <- c("Infographic", "Housing Prices / Rent",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }
    #---housing characteristics---
    if (any(grepl("Housing Characteristics", data_to_use[i,2]))) {
      if(any(grepl("Table", data_to_use[i,3]))){
        new_row <- c("Table", "Housing Characteristics",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Table Download", data_to_use[i,3]))){
        new_row <- c("Table Download", "Housing Characteristics",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Report", data_to_use[i,3]))){
        new_row <- c("Report", "Housing Characteristics",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Map", data_to_use[i,3]))){
        new_row <- c("Map", "Housing Characteristics",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Data Visualization", data_to_use[i,3]))){
        new_row <- c("Data Visualization", "Housing Characteristics",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Infographic", data_to_use[i,3]))){
        new_row <- c("Infographic", "Housing Characteristics",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }    
    #---household type---
    if (any(grepl("Household Type", data_to_use[i,2]))) {
      if(any(grepl("Table", data_to_use[i,3]))){
        new_row <- c("Table", "Household Type",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Table Download", data_to_use[i,3]))){
        new_row <- c("Table Download", "Household Type",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Report", data_to_use[i,3]))){
        new_row <- c("Report", "Household Type",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Map", data_to_use[i,3]))){
        new_row <- c("Map", "Household Type",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Data Visualization", data_to_use[i,3]))){
        new_row <- c("Data Visualization", "Household Type",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Infographic", data_to_use[i,3]))){
        new_row <- c("Infographic", "Household Type",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }      
    #---household size---
    if (any(grepl("Household Size", data_to_use[i,2]))) {
      if(any(grepl("Table", data_to_use[i,3]))){
        new_row <- c("Table", "Household Size",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Table Download", data_to_use[i,3]))){
        new_row <- c("Table Download", "Household Size",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Report", data_to_use[i,3]))){
        new_row <- c("Report", "Household Size",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Map", data_to_use[i,3]))){
        new_row <- c("Map", "Household Size",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Data Visualization", data_to_use[i,3]))){
        new_row <- c("Data Visualization", "Household Size",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Infographic", data_to_use[i,3]))){
        new_row <- c("Infographic", "Household Size",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }      
    #---household income---
    if (any(grepl("Household Income", data_to_use[i,2]))) {
      if(any(grepl("Table", data_to_use[i,3]))){
        new_row <- c("Table", "Household Income",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Table Download", data_to_use[i,3]))){
        new_row <- c("Table Download", "Household Income",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Report", data_to_use[i,3]))){
        new_row <- c("Report", "Household Income",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Map", data_to_use[i,3]))){
        new_row <- c("Map", "Household Income",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Data Visualization", data_to_use[i,3]))){
        new_row <- c("Data Visualization", "Household Income",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Infographic", data_to_use[i,3]))){
        new_row <- c("Infographic", "Household Income",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }    
    #---foreclosures---
    if (any(grepl("Foreclosures", data_to_use[i,2]))) {
      if(any(grepl("Table", data_to_use[i,3]))){
        new_row <- c("Table", "Foreclosures",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Table Download", data_to_use[i,3]))){
        new_row <- c("Table Download", "Foreclosures",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Report", data_to_use[i,3]))){
        new_row <- c("Report", "Foreclosures",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Map", data_to_use[i,3]))){
        new_row <- c("Map", "Foreclosures",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Data Visualization", data_to_use[i,3]))){
        new_row <- c("Data Visualization", "Foreclosures",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Infographic", data_to_use[i,3]))){
        new_row <- c("Infographic", "Foreclosures",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }       
    #---estimates---
    if (any(grepl("Estimates", data_to_use[i,2]))) {
      if(any(grepl("Table", data_to_use[i,3]))){
        new_row <- c("Table", "Estimates",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Table Download", data_to_use[i,3]))){
        new_row <- c("Table Download", "Estimates",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Report", data_to_use[i,3]))){
        new_row <- c("Report", "Estimates",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Map", data_to_use[i,3]))){
        new_row <- c("Map", "Estimates",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Data Visualization", data_to_use[i,3]))){
        new_row <- c("Data Visualization", "Estimates",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
      if(any(grepl("Infographic", data_to_use[i,3]))){
        new_row <- c("Infographic", "Estimates",54)
        df_stack2 <- rbind(df_stack2, new_row)
      }
    }   
  }
  
  colnames(df_stack2) <- col_name
  
  bar1 <- ggplot(df_stack2, aes(x = sub, y = 1, fill = Tools)) +
    geom_col() +
    scale_fill_manual(values = cbPalette) +
    xlab("Sub-categories: Housing") +
    ylab("Counts") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
    ggtitle("Different type of tools inside each sub-category of Housing")
  
  # Set the plot margin to center the title
  bar1 + theme_minimal() +
    theme(plot.title.position = "plot", plot.margin = margin(30, 0, 30, 0)) + coord_flip()
}

#Housing/Health/Education
#Bar graph of age of data
age_of_data_plot <- function(state, data_source) {
  data_source$Age.of.data <- as.integer(data_source$Age.of.data)
  if(state=="All Sample States and Territories") {
    ggplot(data_source, aes(x=Age.of.data)) + geom_bar(width=0.7, col = "#999999", fill="#0072B2") + labs(x="Year of Latest Vintage", y="Counts", title="Age of Data Used") + theme_minimal()
  }
  else {
    State <- str_to_title(state)
    state_input <- data_source[data_source[, "State..Country"]==state, ]
    state_df <- data.frame()
    state_df <- rbind(state_df, state_input)
    state_df$Age.of.data <- as.integer(state_df$Age.of.data)
    ggplot(state_df, aes(x=Age.of.data)) + geom_bar(width = 0.7, col = "#999999", fill="#0072B2") + labs(x="Year of Latest Vintage", y="Counts") + theme_minimal()
  }
}
#Bar graph of sub-category types (For Housing & Health/Education)
sub_cat_counts <- function(state, data_source) {
  if(state=="All Sample States and Territories") {
    sub_cats <- ggplot(data_source, aes(x=Sub.categories)) + geom_bar(fill="steelblue") + labs(x="Sub-Category", y="Counts") + theme(axis.text.x = element_text(angle = 25))
    sub_cats + coord_flip()
  }
  else {
    State <- str_to_title(state)
    state_input <- data_source[data_source[, "State"]==state, ]
    state_df <- data.frame()
    state_df <- rbind(state_df, state_input)
    sub_cats <- ggplot(state_df, aes(x=Sub.categories)) + geom_bar(fill="steelblue") + labs(x="Sub-Category", y="Counts") + theme(axis.text.x = element_text(angle = 25))
    sub_cats + coord_flip()
  }
}

#Word cloud for variable names
variable_cloud <- function(state, data_source) {
  if(state=="All Sample States and Territories"){
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
  if(state=="All Sample States and Territories"){
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
#Housing/Health/Education
##Census sources - pie chart
h_edu_pie_graph_census <- function(state, data_source) {
  if (state == "All Sample States and Territories") {
    data_to_use = data_source
  } else {
    data_to_use = data_source[data_source$State..Country == state, ]
  }
  
  countinue <- data_to_use %>% group_by(Data.Sources.Census) %>% summarize(count = n())
  colnames(countinue) <- c("data source", "count")
  countinue <- countinue[countinue$`data source` != "", ]
  
  #sorted_df <- countinue[order(- countinue$count), ]
  
  # Adding a title to the pie graph
  title <- paste("Data Source (Census) \n Distribution in", state)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  #pie(sorted_df$count, labels = sorted_df$`data source`, border = "white", col = cbPalette, cex = 1, main = title)
  pie(countinue$count, labels = countinue$`data source`, border = "white", col = cbPalette, cex = 1, main = title)
}

#Housing/Health/Education
#Non-census sources - pie chart
h_edu_pie_graph_noncensus <- function(state, data_source) {
  if (state == "All Sample States and Territories") {
    data_to_use = data_source
  } 
  else {
    data_to_use = data_source[data_source$State..Country == state, ]
  }
  
  countinue <- data_to_use %>% group_by(Data.Sources.Non.Census) %>% summarize(count = n())
  colnames(countinue) <- c("data source", "count")
  countinue <- countinue[countinue$`data source` != "", ]
  
  # Adding a title to the pie graph
  title <- paste("Data Source (Non Census) \n Distribution in", state)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  pie(countinue$count, labels = countinue$`data source`, border = "white", col = cbPalette, cex = 1, main = title)
}

#------------------------Finding Diversity ---------------------------------
#div geo level
div_data$Geographic.Levels = tolower(div_data$Geographic.Levels)
div_geography_plot <- function(selected_state) {
  df_stack3 <- data.frame(
    geography = character()
  )
  col_name = c("Geography")
  colnames(df_stack3) <- col_name
  
  # Initialize variables to store the counts
  if (selected_state == "All Sample States and Territories"){
    data_to_use = div_data
  }
  else{
    data_to_use = div_data[div_data$State..Country == selected_state,]
  }
  
  for (i in 1:nrow(data_to_use)) {
    if(any(grepl("minor civil division", data_to_use[i,8]))){
      new_row <- c("Minor Civil Division")
      df_stack3 <- rbind(df_stack3, new_row)}
    if(any(grepl("CBSA", data_to_use[i,8]))){
      new_row <- c("CBSA")
      df_stack3 <- rbind(df_stack3, new_row)}
    if(any(grepl("precinct", data_to_use[i,8]))){
      new_row <- c("Precinct")
      df_stack3 <- rbind(df_stack3, new_row)}
    if(any(grepl("alaska", data_to_use[i,8]))){
      new_row <- c("Alaska Native")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("block", data_to_use[i,8]))) {
      new_row <- c("Block")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("group", data_to_use[i,8]))) {
      new_row <- c("Block Group")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("borough", data_to_use[i,8]))) {
      new_row <- c("Borough")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("census", data_to_use[i,8]))) {
      new_row <- c("Census Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("subarea", data_to_use[i,8]))) {
      new_row <- c("Census Subarea")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("census designated place", data_to_use[i,8]))) {
      new_row <- c("Census Designated Place")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("tract", data_to_use[i,8]))) {
      new_row <- c("Census Tract")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("town", data_to_use[i,8]))) {
      new_row <- c("City/Town")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("county", data_to_use[i,8]))) {
      new_row <- c("County")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("subdivision", data_to_use[i,8]))) {
      new_row <- c("County Subdivision")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("combined", data_to_use[i,8]))) {
      new_row <- c("Combined Statistical Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("congressional", data_to_use[i,8]))) {
      new_row <- c("Congressional District")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("development", data_to_use[i,8]))) {
      new_row <- c("Development District")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("economic", data_to_use[i,8]))) {
      new_row <- c("Economic Region")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("incorporated city", data_to_use[i,8]))) {
      new_row <- c("Incorporated City")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("incorporated place", data_to_use[i,8]))) {
      new_row <- c("Incorporated Place")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("jurisdiction", data_to_use[i,8]))) {
      new_row <- c("Jurisdiction")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("legislative", data_to_use[i,8]))) {
      new_row <- c("Legislative District")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("metropolitan", data_to_use[i,8]))) {
      new_row <- c("Metropolitan Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("micropolitan", data_to_use[i,8]))) {
      new_row <- c("Micropolitan Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("municipality", data_to_use[i,8]))) {
      new_row <- c("Municipality")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("nation", data_to_use[i,8]))) {
      new_row <- c("Nation")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("native", data_to_use[i,8]))) {
      new_row <- c("Native Village Statistical Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("corporation", data_to_use[i,8]))) {
      new_row <- c("Native Regional Corporation")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("neighborhood", data_to_use[i,8]))) {
      new_row <- c("Neighborhood Cluster")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("parish", data_to_use[i,8]))) {
      new_row <- c("Parish")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("places", data_to_use[i,8]))) {
      new_row <- c("Place")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("prosperity", data_to_use[i,8]))) {
      new_row <- c("Prosperity Region")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("puma", data_to_use[i,8]))) {
      new_row <- c("Public Use Microdata Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("school", data_to_use[i,8]))) {
      new_row <- c("School District")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("region", data_to_use[i,8]))) {
      new_row <- c("Region")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("state", data_to_use[i,8]))) {
      new_row <- c("State")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("substate", data_to_use[i,8]))) {
      new_row <- c("Substate Region")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("summary", data_to_use[i,8]))) {
      new_row <- c("Summary Level")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("township", data_to_use[i,8]))) {
      new_row <- c("Township")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("urban", data_to_use[i,8]))) {
      new_row <- c("Urban Area")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("rural", data_to_use[i,8]))) {
      new_row <- c("Urban/Rural")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("village", data_to_use[i,8]))) {
      new_row <- c("Village")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("ward", data_to_use[i,8]))) {
      new_row <- c("Ward")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("zcta", data_to_use[i,8]))) {
      new_row <- c("ZCTA")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("zip", data_to_use[i,8]))) {
      new_row <- c("Zip Code")
      df_stack3 <- rbind(df_stack3, new_row)}
    if (any(grepl("territory", data_to_use[i,8]))) {
      new_row <- c("Territory")
      df_stack3 <- rbind(df_stack3, new_row)}
  }
  colnames(df_stack3) <- col_name 
  geography_types <- df_stack3 %>% group_by(Geography)%>%
    summarise(count = n())
  
  ggplot(geography_types, aes(x = Geography, y = count)) +
    geom_col(width = 0.8, fill = "#0072B2") +
    scale_fill_manual(values = "#0072B2") +
    labs(x = "Geography: Diversity", y = "Counts") +  # Adjust the font size here
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.4, face = "bold"),
      axis.text.x = element_text(size = 14),  # Adjust the font size for x-axis labels here
      axis.text.y = element_text(size = 14),  # Adjust the font size for y-axis labels here
      axis.title.x = element_text(size = 14),  # Adjust the font size for x-axis label (x-axis title) here
      axis.title.y = element_text(size = 14)   # Adjust the font size for y-axis label (y-axis title) here
    ) +coord_flip()+
    ggtitle(paste("Types of Geographic Levels", selected_state))
  
}

#div age of data
div_age_of_data <- function(state, data_source) {
  data_source$Age.of.data <- as.integer(data_source$Age.of.data)
  if(state=="All Sample States and Territories") {
    ggplot(data_source, aes(x=Age.of.data)) +
      geom_bar(width=0.7, col = "#999999", fill="#0072B2") +
      labs(x="Year of Latest Vintage", y="Counts") +
      theme_minimal()+
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1,size = 14),
        plot.title = element_text(hjust = 0.4, face = "bold"),
        axis.text.y = element_text(size = 14),  # Adjust the font size for y-axis labels here
        axis.title.x = element_text(size = 14),  # Adjust the font size for x-axis label (x-axis title) here
        axis.title.y = element_text(size = 14) 
      ) +
      ggtitle(paste("Age of Diversity Data in", state))
  }
  else {
    State <- str_to_title(state)
    state_input <- data_source[data_source[, "State..Country"]==state, ]
    state_df <- data.frame()
    state_df <- rbind(state_df, state_input)
    state_df$Age.of.data <- as.integer(state_df$Age.of.data)
    ggplot(state_df, aes(x=Age.of.data)) + 
      geom_bar(width = 0.7, col = "#999999", fill="#0072B2") +
      labs(x="Year of Latest Vintage", y="Counts") + 
      theme_minimal()+
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1,size = 14),
        plot.title = element_text(hjust = 0.4, face = "bold"),
        axis.text.y = element_text(size = 14),  # Adjust the font size for y-axis labels here
        axis.title.x = element_text(size = 14),  # Adjust the font size for x-axis label (x-axis title) here
        axis.title.y = element_text(size = 14)   # Adjust the font size for y-axis label (y-axis title) here
      ) +
      ggtitle(paste("Age of Diversity Data in", state))
  }
}

#Word cloud for subcat
div_sub_cat_cloud <- function(selected_state) {
  if (selected_state == "All Sample States and Territories") {
    data_to_use <- div_data
  } else {
    data_to_use <- div_data[div_data$State..Country == selected_state, ]
  }
  combo <- ""
  for (i in 1:nrow(data_to_use)) {
    combo <- paste(combo, data_to_use$Sub.categories[i], sep="")
  }
  cloud(combo)
}

#Word cloud for variable names
div_variable_cloud <- function(selected_state) {
  if (selected_state == "All Sample States and Territories") {
    data_to_use <- econ_data
  } else {
    data_to_use <- econ_data[econ_data$State..Country == selected_state, ]
  }
  combo <- ""
  for (i in 1:nrow(data_to_use)) {
    combo <- paste(combo, data_to_use$Variables.Used..list.all.that.apply.[i], sep="")
  }
  cloud(combo)
}

#Diversity tool type bar graph
div_tool_type_plot <- function(selected_state) {
  # Initialize variables to store the counts
  if (selected_state == "All Sample States and Territories"){
    data_to_use = div_data$Tool
  }
  else{
    data_to_use = div_data$Tool[div_data$State..Country == selected_state]
  }
  map <- 0
  table <- 0
  download <- 0
  infographic <- 0
  visualization <- 0
  report <- 0
  for (i in data_to_use) {
    if (any(grepl("Map", i))) {
      map <- map + 1
    }
    else if (any(grepl("Table Download", i))) {
       download <- download + 1
    }
    else if (any(grepl("Table", i))) {
      table <- table + 1
    }
    if (any(grepl("Infographic", i))) {
      infographic <- infographic + 1
    }
    if (any(grepl("Visual", i))) {
      visualization <- visualization + 1
    }
    if (any(grepl("Report", i))) {
      report <- report + 1
    }
  }
  # Create data frame for plotting
  tool_name <- c("Map", "Table Download", "Table", "Infographic", "Visualization", "Report")
  counts <- c(map, download, table, infographic, visualization, report)
  total_df <- data.frame(tool_name, counts)
  # Barplot
  ggplot(total_df, aes(x = tool_name, y = counts)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7, col = "#999999", fill = "#0072B2") +
    geom_text(aes(label = counts), position = position_stack(vjust = 0.5), vjust = -0.5, cex = 0.8, col = "black") +
    labs(title = paste("Types of Tools:", selected_state),
         x = "Type of Tools: Diversity", y = "Counts") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
}
#Div census source pie
div_pie_graph_census<- function(selected_state, data_table) {
  if (selected_state == "All Sample States and Territories") {
    data_to_use = data_table
  } else {
    data_to_use = data_table[data_table$State..Country == selected_state, ]
  }
  
  count_result <- data_to_use %>% group_by(Data.Sources.Census) %>% summarize(count = n())
  colnames(count_result) <- c("data source", "count")
  count_result <- count_result[count_result$`data source` != "", ]
  # Assuming countinue$`data source` contains the labels you want to modify
  #count_result$`data source` <- gsub("Small Area Income and Poverty Estimates", "Small Area Income \n and Poverty Estimates", count_result$`data source`)
  sorted_df <- count_result[order(- count_result$count), ]
  # Adding a title to the pie graph
  title <- paste("Diveristy Data Census Source (Census) \n Distribution in", selected_state)
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  pie(sorted_df$count, labels = sorted_df$`data source`, border = "white", col = cbPalette, cex = 1, main = title)
}

div_direct_census_link <- function(selected_state, data_table) {
  if (selected_state == "All Sample States and Territories") {
    data_to_use <- data_table
  } else {
    data_to_use <- data_table[data_table$State..Country == selected_state, ]
  }
  
  count_result <- data_to_use %>% group_by(Direct.links.to.Census) %>% summarize(count = n())
  colnames(count_result) <- c("direct link", "count")
  count_result <- count_result[count_result$`direct link` != "", ]
  count_result <- na.omit(count_result)
  sorted_df <- count_result[order(-count_result$count), ]
  # Replace "N" with "No" and "Y" with "Yes"
  #sorted_df$`direct link` <- ifelse(sorted_df$`direct link` == "N", "No", "Yes")
  
  # Adding a title to the pie graph
  title <- paste("Diversity Data Census Link \n Distribution in", selected_state)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  pie(sorted_df$count, labels = sorted_df$`direct link`, border = "white", col = cbPalette, cex = 1, main = title)
}

div_historical_data <- function(selected_state, data_table) {
  if (selected_state == "All Sample States and Territories") {
    data_to_use <- data_table
  } else {
    data_to_use <- data_table[data_table$State..Country == selected_state, ]
  }
  
  count_result <- data_to_use %>% group_by(Historical.data) %>% summarize(count = n())
  colnames(count_result) <- c("historical data", "count")
  count_result <- count_result[count_result$`historical data` != "", ]
  count_result <- na.omit(count_result)
  sorted_df <- count_result[order(-count_result$count), ]
  
  # Replace "N" with "No" and "Y" with "Yes"
  #sorted_df$`historical data` <- ifelse(sorted_df$`historical data` == "N", "No", "Yes")
  
  # Adding a title to the pie graph
  title <- paste("Diversity Historical Data \n Distribution in", selected_state)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  pie(sorted_df$count, labels = sorted_df$`historical data`, border = "white", col = cbPalette, cex = 1, main = title)
}






#-------------------------Define UI------------------------
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
      font-weight: bold;
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
  tags$head(tags$style(HTML("
  /* Custom CSS to center and make text bold */
  .center-bold-text {
    text-align: center;
    font-weight: bold;
  }
"))),
  
  navbarPage(title= tags$a(href = "https://biocomplexity.virginia.edu/data-science-public-good-young-scholars-program", target = "_blank", # "_blank" opens the link in a new tab
                           tags$img(src = "DSPG_black-01.png", width = "120px", style="margin-top:-10px")
                           ),
             tabPanel("Overview",
                      div(class="even-content",
                          tags$a(href = "https://biocomplexity.virginia.edu/",
                                 img(src = "biilogo.png", width = "170px")),
                          p(style = "font-size: 30px; font-weight: bold; color: #1B3766;text-align: center;","Survey on State Data Use"),
                          tags$a(href = "https://www.census.gov/",
                                 img(src = "census.png", width = "90px")),
                      ),
                      panel(h3("Project Overview", style = "color: #1B3766;"),
                            p("The goal of our project is to assist the U.S. Census Bureau in the creation of their Curated Data Enterprise, 
                              a tool that will combine data from multiple Census sources, to create a cohesive data lake that can be used as 
                              a tool by State governments."),
                            p("To accomplish this, we’ve focused on identifying how state governments use data and identifying what those data
                              sources are, such as Census, State government, or private sources. We’ve started doing this through our data 
                              discovery process in which we collected data from state constitutions, state data centers, and the Federal-State 
                              Cooperative for Population Estimates contacts."),
                            p("The overall objective of this project is to gain insight into how entities harness the power of data and report 
                              findings on how states leverage their data, the tools they use, and the deliverables they produce, so that the 
                              Census Bureau can use out fidnings to better address state government data needs and to create a tool that facilitates
                              user data access."),        
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
             tabPanel("Topic Modeling",
                      h3("Topic Modeling", style = "color: #1B3766;"),
                      br(),
                      h4("What is Topic Modeling?", style = "color: #E57200;"),
                      p("In statistics and natural language processing, a topic model is a type of statistical model for discovering the abstract 'topics' that occur in a collection of documents."),
                      p("Some commonly used packages for topic modeling include GENSIM, BERT, and NLTK."),
                      p("In our project, we used GENSIM and BERT to examine topics within State Constitutions."),
                      br(),
                      h4("BERT",style = "color: #E57200;"),
                      p("We applied BERT to the top 5 State Constitutions with the most amendments."),
                      p("1.California"),
                      p("2.Hawaii"),
                      p("3.Maryland"),
                      p("4.Oregon"),
                      p("5.Texas"),
                      br(),
                      fluidRow(
                        column(width = 6,
                               h4("BERT Example: California Topics",style = "color: #E57200;"),
                               tags$img(height=380, width=450, src="BERT_CA_Topics.png")
                               ),
                        column(width = 6, 
                               h4("BERT Example: California Data",style = "color: #E57200;"),
                               tags$img(height=450, width=450, src="CABert.png")))),
             tabPanel("Mission Statements",
                      br(),
                      h3("Examining Mission Statements of State Data Centers", style = "color: #1B3766;"),
                      p("Out of the 56 State Data Centers that we examined, 42 had mission statements that related to the work of the SDC."),
                      br(),
                      sidebarLayout(sidebarPanel(
                        selectInput("dropdownM", "Which state's mission statement are you interested in?", mission_states)),
                        mainPanel(p("States that did not have an SDC mission statement included: Colorado, Georgia, Idaho, Illinois, 
                      Louisiana, Nebraska, New Mexico, Virginia, Washington, West Virginia, Wyoming, Puerto Rico, Guam,
                      U.S. Virgin Islands, American Samoa"))),
                      br(),
                      div(class="center-content",
                          textOutput("mission_text1")),
                      plotOutput("mission_plot1")
                     ),
             tabPanel("FSCPE Response",
                      h3("FSCPE Emails and Responses", style = "color: #1B3766;"),
                      br(),
                      h4("What is FSCPE?", style = "color: #E57200;"),
                      p("FSCPE stands for", 
                        tags$a(href = "https://www.census.gov/programs-surveys/popest/about/fscpe.html", "The Federal-State Cooperative for Population Estimates", style = "display: inline"),
                        ". it is an informal cooperation between the Federal Government and the states in the area of local population estimates
                        existed as early as 1953. State FSCPE agencies, designated by their respective governors, work in cooperation with the
                        Census Bureau's Program Branches to produce population estimates."),
                      br(),
                      h4("Emails and Responses", style = "color: #E57200;"),
                      p("We emailed 56 FSCPE contacts, asking about the top six data sources that they use."),
                      p("Of the 56 contacts, we received responses from 17. See their responses from the table below."),
                      br(),
                      div(dataTableOutput("fscpe_table"))
                      ),
             navbarMenu("State Data Center Findings",
                        tabPanel("Intro",
                                 h3("Survey Finding on State Data Centers", style = "color: #1B3766;"),
                                 br(),
                                 h4("What is State Data Center?", style = "color: #E57200;"),
                                 p(tags$a(href = "https://www.census.gov/about/partners/sdc.html", "The State Data Center (SDC) Program", style = "display: inline"),
                                   " is one of the Census Bureau's longest and most successful partnerships. 
                                   This partnership between the 50 states, the District of Columbia, Puerto Rico, the island areas and the Census 
                                   Bureau was created in 1978 to make data available locally to the public through a network of state agencies, 
                                   universities, libraries, and regional and local governments."),
                                 p("State Data Center empowers data users with understandable, accurate and timely information through the mutually
                                   beneficial partnership between the State Data Centers and the Census Bureau."),
                                 h4("Type of Lead SDC Agency", style = "color: #E57200;"),
                                 p("For each state, they have one lead SDC agency and one or more coordinating agency. And those agencies are from 
                                   numerous types of organizations: Universities, Libraries, Research Centers, etc. Here we built an visualization 
                                   on what type of lead agency it is for each state."),
                                 plotlyOutput("intro_plot1"),
                                 h4("Number of Coordinating SDC Agency", style = "color: #E57200;"),
                                 p("As mentioned above, there could be multiple coordinating agencies for one state. Here we built an visualization 
                                   on how many coordinating agencies there are for each state."),
                                 plotlyOutput("intro_plot2"),
                                 h4("States that We Have Examined", style = "color: #E57200;"),
                                 p("Our project is still an on-going project. For now we have covered 28 states and U.S. territories in alphabetical
                                   order. Here is a map showing which states and U.S. territories we have examined so far."),
                                 plotlyOutput("intro_plot3")),
                        tabPanel("Demographics",
                                 h3(style ="color: #1B3766;","Demographics Findings"),
                                 br(),
                                 sidebarLayout(sidebarPanel(
                                   selectInput("dropdownD", "Which state are you interested in?",
                                               all_states),
                                   downloadButton("download_demo_data", "Download Demographics Data")
                                 ),
                                 mainPanel(plotOutput("fin_dem_1"),
                                           plotOutput("fin_dem_2"),
                                           plotOutput("fin_dem_3"),
                                           plotOutput("fin_dem_4")
                                           ))),
                        tabPanel("Economy",
                                 h3("Economy Findings", style ="color: #1B3766;"),
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("dropdown3", "Which state are you interested in?", all_states),
                                     downloadButton("download_econ_data", "Download Economy Data")
                                   ),
                                   mainPanel(
                                     plotOutput("fin_econ_plot1"),
                                     plotOutput("fin_econ_plot2"),
                                   )
                                 ),
                                 br(),
                                 fluidRow(
                                   column(width = 6, 
                                          plotOutput("fin_econ_geo"),
                                          div(class="center-content",
                                              textOutput("fin_econ_text7")),
                                          plotOutput("fin_econ_plot7"), 
                                          plotOutput("fin_econ_plot3"), 
                                          plotOutput("fin_econ_plot5")),
                                   column(width = 6, 
                                          plotOutput("fin_econ_age"),
                                          div(class="center-content",
                                              textOutput("fin_econ_text8")),
                                          plotOutput("fin_econ_plot8"), 
                                          plotOutput("fin_econ_plot4"), 
                                          plotOutput("fin_econ_plot6"))
                                 )),
                        
                        tabPanel("Housing",
                                 h3("Housing Findings",style ="color: #1B3766;"),
                                 br(),
                                 sidebarLayout(sidebarPanel(
                                   selectInput("dropdownH", "Which state are you interested in?",
                                               all_states),
                                   downloadButton("download_housing_data", "Download Housing Data")
                                   ),
                                   mainPanel(textOutput("fin_hous_text1"), 
                                             plotOutput("fin_hous_plot1"), 
                                             textOutput("fin_hous_text2"), 
                                             plotOutput("fin_hous_plot2"), 
                                             textOutput("fin_hous_text3"), 
                                             plotOutput("fin_hous_plot3")
                                             ))),
                        
                        tabPanel("Diversity",
                                 h3("Diversity Findings",style ="color: #1B3766;"),
                                 br(),
                                 sidebarLayout(sidebarPanel(
                                   selectInput("dropdownDiv", "Which state are you interested in?",
                                               all_states),
                                   downloadButton("download_diversity_data", "Download Diversity Data")
                                 ),
                                   mainPanel(plotOutput("fin_div_plot1"),
                                             plotOutput("fin_div_geo"),
                                             plotOutput("fin_div_age"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(width = 6, 
                                          div(class="center-content",
                                              textOutput("fin_div_sub_cat_text")),
                                          plotOutput("fin_div_sub_cat_plot"),
                                          plotOutput("fin_div_census_plot"),
                                          plotOutput("fin_div_link_plot")), 
                                   column(width = 6, 
                                          div(class="center-content",
                                              textOutput("fin_div_varname_text")),
                                          plotOutput("fin_div_varname_plot"),
                                          plotOutput("fin_div_historical_plot"))
                                 )),
                        
                        tabPanel("Health & Education",
                                 h3("Health and Education Findings",style ="color: #1B3766;"),
                                 br(),
                                 sidebarLayout(sidebarPanel(
                                   selectInput("dropdownHE", "Which state are you interested in?",
                                               all_states),
                                   downloadButton("download_HE_data", "Download Health & Education Data")),
                                   mainPanel(textOutput("fin_HE_text1"), 
                                             plotOutput("fin_HE_plot1"), 
                                             textOutput("fin_HE_text2"), 
                                             plotOutput("fin_HE_plot2"), 
                                             textOutput("fin_HE_text3"), 
                                             plotOutput("fin_HE_plot3")
                                             )))
                        )))
  
 

#------------------------Server function-------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Topic Modeling-BERT
  
  #Mission Statements
  output$mission_text1 <- renderText({{paste("Word cloud on", input$dropdownM, "mission statement.")}})
  output$mission_plot1 <- renderPlot({mission_cloud(state=input$dropdownM)})
  
  #FSCPE
  output$fscpe_table <- renderDataTable(fscpe)

  #Intro Findings
  output$intro_plot1 <- renderPlotly({lead_types_map()})
  output$intro_plot2 <- renderPlotly({coord_num_map()})
  output$intro_plot3 <- renderPlotly({examined_states()})

  #Demographic Findings
  output$download_demo_data <- downloadHandler(
    filename = function() {paste("demo_data_", Sys.Date(), ".csv", sep = "")},
    content = function(file) {write.csv(dem_data, file)}
  )
  output$fin_dem_1 <- renderPlot({dem_category_plot(selected_state = input$dropdownD)})
  output$fin_dem_2 <- renderPlot({dem_sub_cat_and_tool(selected_state = input$dropdownD)})
  output$fin_dem_3 <- renderPlot({dem_census_source(selected_state = input$dropdownD)})
  output$fin_dem_4 <- renderPlot({dem_non_census_source(selected_state = input$dropdownD)})

  #Housing Findings
  output$download_housing_data <- downloadHandler(
    filename = function() {paste("housing_data_", Sys.Date(), ".csv", sep = "")},
    content = function(file) {write.csv(housing_data, file)}
  )
  output$fin_hous_text1 <- renderText({{paste("Type of Sub-Category for: ", input$dropdownH)}})
  output$fin_hous_plot1 <- renderPlot({sub_cat_counts(state=input$dropdownH, data_source = housing_data)})
  output$fin_hous_text2 <- renderText({{paste("Word cloud on tool names for: ", input$dropdownH)}})
  output$fin_hous_plot2 <- renderPlot({tool_cloud(state=input$dropdownH, data_source = housing_data)})
  output$fin_hous_text3 <- renderText({{paste("Word cloud on variables for: ", input$dropdownH)}})
  output$fin_hous_plot3 <- renderPlot({variable_cloud(state=input$dropdownH, data_source = housing_data)})
  
  #Health/Education Findings
  output$download_HE_data <- downloadHandler(
    filename = function() {paste("health_edu_data_", Sys.Date(), ".csv", sep = "")},
    content = function(file) {write.csv(HE_data, file)}
  )
  output$fin_HE_text1 <- renderText({{paste("Type of Sub-Category for: ", input$dropdownHE)}})
  output$fin_HE_plot1 <- renderPlot({sub_cat_counts(state=input$dropdownHE, data_source = HE_data)})
  output$fin_HE_text2 <- renderText({{paste("Word cloud on tool names for: ", input$dropdownHE)}})
  output$fin_HE_plot2 <- renderPlot({tool_cloud(state=input$dropdownHE, data_source = HE_data)})
  output$fin_HE_text3 <- renderText({{paste("Word cloud on variables for: ", input$dropdownHE)}})
  output$fin_HE_plot3 <- renderPlot({variable_cloud(state=input$dropdownHE, data_source = HE_data)})
  
  #Economy Findings
  output$download_econ_data <- downloadHandler(
    filename = function() {paste("econ_data_", Sys.Date(), ".csv", sep = "")},
    content = function(file) {write.csv(econ_data, file)}
  )
  output$fin_econ_plot1 <- renderPlot({econ_category_plot(selected_state = input$dropdown3)})
  output$fin_econ_plot2 <- renderPlot({econ_sub_cat_and_tool(selected_state = input$dropdown3)})
  output$fin_econ_plot3 <- renderPlot({econ_pie_graph_census(selected_state = input$dropdown3, data_table = econ_data)})
  output$fin_econ_plot4 <- renderPlot({econ_pie_graph_noncensus(selected_state = input$dropdown3, data_table = econ_data)})
  output$fin_econ_plot5 <- renderPlot({econ_direct_census_link(selected_state = input$dropdown3, data_table = econ_data)})
  output$fin_econ_plot6 <- renderPlot({econ_historical_data(selected_state = input$dropdown3, data_table = econ_data)})
  output$fin_econ_text7 <- renderText({{paste("Econ Tool Name Cloud in", input$dropdown3)}})
  output$fin_econ_plot7 <- renderPlot({econ_tool_name_cloud(selected_state = input$dropdown3)})
  output$fin_econ_text8 <- renderText({{paste("Econ Variable Cloud in", input$dropdown3)}})
  output$fin_econ_plot8 <- renderPlot({econ_variable_cloud(selected_state = input$dropdown3)})
  output$fin_econ_geo <-renderPlot({econ_geography_plot(selected_state = input$dropdown3)})
  output$fin_econ_age <-renderPlot({econ_age_of_data(state = input$dropdown3, data_source = econ_data)})
  
  #Diversity Findings
  output$download_diversity_data <- downloadHandler(
    filename = function() {paste("diversity_data_", Sys.Date(), ".csv", sep = "")},
    content = function(file) {write.csv(div_data, file)}
  )
  output$fin_div_plot1 <- renderPlot({div_tool_type_plot(selected_state = input$dropdownDiv)})
  output$fin_div_census_plot <- renderPlot({div_pie_graph_census(selected_state = input$dropdownDiv, data_table = div_data)})
  output$fin_div_sub_cat_text <- renderText({{paste("Diversity Sub-category Cloud in", input$dropdownDiv)}})
  output$fin_div_sub_cat_plot <- renderPlot({div_sub_cat_cloud(selected_state = input$dropdownDiv)})
  output$fin_div_varname_text <- renderText({{paste("Diversity Variable Cloud in", input$dropdownDiv)}})
  output$fin_div_varname_plot <- renderPlot({div_variable_cloud(selected_state = input$dropdownDiv)})
  output$fin_div_link_plot <- renderPlot({div_direct_census_link(selected_state = input$dropdownDiv, data_table = div_data)})
  output$fin_div_historical_plot <- renderPlot({div_historical_data(selected_state = input$dropdownDiv, data_table = div_data)})
  output$fin_div_geo <- renderPlot({div_geography_plot(selected_state = input$dropdownDiv)})
  output$fin_div_age <- renderPlot({div_age_of_data(state = input$dropdownDiv, data_source = div_data)})
}


shinyApp(ui = ui, server = server)
