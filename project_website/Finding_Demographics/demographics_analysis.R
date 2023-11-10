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

library(wordcloud)
library(RColorBrewer)
library(reshape)
library(tm)
library(stringr)
library(usmap)
library(readxl)


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

all_states <- c("All Sample States and Territories", "Alabama", "Alaska","Arizona", "Arkansas", "California",
                "Colorado", "Connecticut", "Delaware", "District of Colombia", 
                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                "Massachusetts", "Michigan", "Minnesoda", "Mississippi", "Missouri",
                "Puerto Rico", "Guam")

dem_data <- read.csv("/home/gcm8gw/Git/dspg23census/project_website/Finding_Demographics/SDC_Demographics.csv")
dem_data$Sub.categories = tolower(dem_data$Sub.categories)

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
  col_name = c("Tools","sub","count")
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
  
  ggplot(df_stack2, aes(x = sub, y = 1, fill = Tools)) +
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

dem_age_of_data_plot <- function(state, data_source) {
  data_source$Age.of.data2 <- as.integer(data_source$Age.of.data2)
  if(state=="All Sample States and Territories") {
    ggplot(data_source, aes(x=Age.of.data2)) + geom_bar(width=0.7, col = "#999999", fill="#0072B2") + 
      labs(x="Year of Latest Vintage", y="Counts", title=paste("Age of Demographic Data in", state)) + theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.4, face = "bold"))
    
  }
  else {
    State <- str_to_title(state)
    state_input <- data_source[data_source[, "State..Country"]==state, ]
    state_df <- data.frame()
    state_df <- rbind(state_df, state_input)
    state_df$Age.of.data2 <- as.integer(state_df$Age.of.data2)
    ggplot(state_df, aes(x=Age.of.data2)) + geom_bar(width = 0.7, col = "#999999", fill="#0072B2") + 
      labs(x="Year of Latest Vintage", y="Counts", title=paste("Age of Demographic Data in", state)) + theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
                            plot.title = element_text(hjust = 0.4, face = "bold")) 
      
  }
}

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

dem_direct_census_link <- function(selected_state, data_table) {
  if (selected_state == "All Sample States and Territories") {
    data_to_use <- dem_data
  } else {
    data_to_use <- dem_data[dem_data$State..Country == selected_state, ]
  }
  
  count_result <- data_to_use %>% group_by(Direct.links.to.Census) %>% summarize(count = n())
  colnames(count_result) <- c("direct link", "count")
  count_result <- count_result[count_result$`direct link` != "", ]
  
  sorted_df <- count_result[order(-count_result$count), ]
  
  # Replace "N" with "No" and "Y" with "Yes"
  sorted_df$`direct link` <- ifelse(sorted_df$`direct link` ==  "N", "No", "Yes")
  
  # Adding a title to the pie graph
  title <- paste("Demographics Data Census Link \n Distribution in", selected_state)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  pie(sorted_df$count, labels = sorted_df$`direct link`, border = "white", col = cbPalette, cex = 1, main = title)
}



# Plot 10 - Historical data

dem_historical_data <- function(selected_state, data_table) {
  if (selected_state == "All Sample States and Territories") {
    data_to_use <- dem_data
  } else {
    data_to_use <- dem_data[dem_data$State..Country == selected_state, ]
  }
  
  count_result <- data_to_use %>% group_by(Historical.data) %>% summarize(count = n())
  colnames(count_result) <- c("historical data", "count")
  count_result <- count_result[count_result$`historical data` != "", ]
  count_result <- count_result[count_result$`historical data` != "N/A", ]
  
  sorted_df <- count_result[order(-count_result$count), ]
  
  # Replace "N" with "No" and "Y" with "Yes"
  sorted_df$`historical data` <- ifelse(sorted_df$`historical data` == "N ", "No", "Yes")
  
  # Adding a title to the pie graph
  title <- paste("Demographics Historical Data \n Distribution in", selected_state)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  pie(sorted_df$count, labels = sorted_df$`historical data`, border = "white", col = cbPalette, cex = 1, main = title)
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
  
  navbarPage(title= tags$a(href = "https://biocomplexity.virginia.edu/institute/divisions/social-and-decision-analytics/dspg", target = "_blank", # "_blank" opens the link in a new tab
                           tags$img(src = "DSPG_black-01.png", width = "120px", style="margin-top:-10px")
  ),
  tabPanel("Overview",
           div(class = "even-content",
               tags$a(href = "https://biocomplexity.virginia.edu/",
                      img(src = "biilogo.png", width = "120px")),
               p(style = "font-size: 25px; font-weight: bold;","Survey on State Data Use"),
               tags$a(href = "https://www.census.gov/",
                      img(src = "census.png", width = "65px")),
           ),
           box(title="Project Overview",
               p("To provide the Census Bureau with information on the following."),
               p("1. What data sources do state, U.S. territories, and District of Columbia, data centers use?"),
               p("2. How do they use these data sources?"),
               p("3. What are their future data needs?")
           )
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
             tabPanel("Demographics",
                      h3(style ="color: #1B3766;","Demographics Findings"),
                      br(),
                      sidebarLayout(sidebarPanel(
                        selectInput("dropdownD", "Which state are you interested in?",
                                    all_states),
                        downloadButton("download_demo_data", "Download Demographics Data")
                      ),
                      mainPanel(#textOutput("text3"),
                        plotOutput("fin_dem_plot1"),
                        br(),
                        plotOutput("fin_dem_plot2"),
                        br(),
                        plotOutput("fin_dem_plot3"),
                        br(),
                        plotOutput("fin_dem_plot4"),
                        )
                      ),
                      br(),
                      fluidRow(
                        column(width = 6,
                          div(class="center-content",
                              textOutput("fin_dem_text5")),
                            plotOutput("fin_dem_plot5"),
                            plotOutput("fin_dem_plot7"),
                            br(),
                            br(),
                            plotOutput("fin_dem_plot9")),
                        column(width = 6,
                            div(class="center-content",
                                  textOutput("fin_dem_text6")),
                                plotOutput("fin_dem_plot6"),
                            br(),
                            div(class="center-content",
                                textOutput("fin_dem_text8")),
                            dataTableOutput("fin_dem_plot8"),
                            br(),
                            plotOutput("fin_dem_plot10"))
                      )),
             
             tabPanel("Economy"),
             tabPanel("Housing"),
             tabPanel("Diversity"),
             tabPanel("Health & Education")
             ),
  tabPanel("Search Platform and Database",
           h3("Search Platform and Database Search Results"),
           p("To address the question from Census, “What are state data needs?” 
                   the following search platforms were queried and databases searched."),
           h4("Search Platform"),
           p(tags$a(href = "https://elicit.org", "elicit.org - a relatively new search platform that uses natural language prompts.",
                    style = "display: inline")),
           p("- What are the data needs of U.S. state and local governments?"),
           p(tags$a(href = "https://www.choicesmagazine.org/UserFiles/file/cmsarticle_323.pdf", "Why We Need Federal Statistical Data for States and Counties",
                    style = "display: inline")),
           
           p("- What can Census do to help U.S. State Data Centers?"),
           p("- What data do U.S. local governments want?"),
           p(tags$a(href = "https://www.igi-global.com/viewtitle.aspx?TitleId=261846&isxn=9781799807841", "A Survey of Municipal Open Data Repositories in the U.S.",
                    style = "display: inline")),
           p("- How the U.S. CENSUS can help State Data Centers?"),
           p(tags$a(href = "https://elicit.org/search?q=How+the+U.S.+CENSUS+can+help+State+Data+Centers%3F&token=01H6VF95XHYNCG9T1EEY4T41EM&paper=a417ceac8a01d856d3e4fdd7cf09f0ad24064062&column=title", "The Use of Blended Data to Improve Public Assistance Programs: Results from a Partnership between the U.S. Census Bureau, USDA, and State Program Agencies",
                    style = "display: inline")),
           p("- What are United States local government data needs?"),
           p(tags$a(href = "https://journals.sagepub.com/doi/pdf/10.1177/0002716210374414?casa_token=8q6dlQ8dKloAAAAA:hc_HuZrqkD8un65O2822htXjRgvNOSJsz-MrQt7YTmwKZ3V7ztBK742_m9f0pjsagUW7Lx5Xh1tP", "The Federal Statistical System: The Local Government Perspective",
                    style = "display: inline")),
           p(tags$a(href = "https://www.sciencedirect.com/science/article/pii/S0736585320301854?via%3Dihub", "Beyond the supply side: Use and impact of municipal open data in the U.S.",
                    style = "display: inline")),
           h4("Databases"),
           p("Various iterations of the above questions were used to search the following data bases with no success."),
           p("- Policy Commons (accessed through the UVA library)"),
           p("- Policy Index File (accessed through the UVA library)"),
           h4("Nonprofits and Associations"),
           p("- ", tags$a(href = "https://www.urban.org/", "Urban Institute",
                         style = "display: inline")),
           p("- ",tags$a(href = "https://www.apdu.org/", "Association of Public Data Users",
                        style = "display: inline")),
           p("- ",tags$a(href = "https://guides.lib.virginia.edu/datascience/gov-docs#s-lg-box-wrapper-28547820", "tate and Local Government Documents",
                        style = "display: inline")),
           p("- ",tags$a(href = "https://www.naco.org/", "National Association of Counties",
                        style = "display: inline")),
           p("- ",tags$a(href = "https://www.ncsl.org/", "National Conference of State Legislatures",
                        style = "display: inline")),
           h4("Mapping Platform"),
           p("- ",tags$a(href = "https://www.policymap.com/resources/customer-stories", "Customer Stories",
                        style = "display: inline")),
           p("- ",tags$a(href = "https://www.policymap.com/resources/customer-stories/city-of-philadelphia", "City of Philadelphia – “The City began using PolicyMap for grant applications, but that was just the start. The Philadelphia Division of Housing and Community Development worked with PolicyMap to create a custom report that supplies up-to-date information needed for quarterly reports to HUD to meet the requirements for the Choice Neighborhood grant, which provides funding to revitalize neighborhoods with distressed public or HUD-assisted housing.",
                        style = "display: inline")),
           p("- ",tags$a(href = "https://www.policymap.com/resources/customer-stories/usda-rural-housing-service", "USDA Rural Housing Service – “RHS needed a better way to track progress of projects to stay organized and show positive impact. In 2015, the Rural Housing Service was reaching unprecedented levels of success in helping build small-town health clinics and offering successful loans to families as a result of a huge budget increase. However, with the flurry of activity within the department, the RHS needed a better way to track the progress of these projects to stay organized and show off their positive impact.",
                        style = "display: inline")),
           p("- ",tags$a(href = "https://www.policymap.com/resources/customer-stories/city-of-dallas", "City of Dallas – “The City of Dallas’s Department of Planning and Urban Design found they were frequently reacting to repetitive requests for data and maps, and they needed a proactive solution, both across city government departments and to better serve public requests for information.",
                        style = "display: inline")),
           p("- ",tags$a(href = "https://www.policymap.com/resources/customer-stories/hawaii-data-collaborative", "Hawaii Data Collaborative – “Finding meaningful, cohesive data about trends in Hawaii’s communities and a shared understanding of the state of Hawaii residents was challenging due to siloed information and lack of data.",
                        style = "display: inline")),
           p("- ",tags$a(href = "https://www.policymap.com/resources/customer-stories/north-carolina-housing-finance-agency", "North Carolina Housing Finance Agency – “To better inform decision-making around affordable housing strategies, NCHFA needed a platform to easily share housing data with the public. When spreadsheets of data proved too inaccessible, and a self-maintained GIS platform proved too cumbersome, NCHFA turned to an interactive, embedded, automatically updated map from PolicyMap.",
                        style = "display: inline")),
           p("- ",tags$a(href = "https://www.policymap.com/resources/customer-stories/connecticut-housing-finance-authority-chfa", "Connecticut Housing Finance Authority -  CHFA works to alleviate the shortage of housing for low- to moderate-income families and persons in Connecticut. To communicate the impact of their investments, staff relied on analysts and complex GIS software. As requests for data pulls, quick maps, and reports grew, CHFA needed a solution for making community housing information more accessible to anyone in the organization. Staff needed the ability to explore specific geographies, perform their own impact analysis, and geocode information based on legislative districts for meetings with state legislators, community partners, and municipal planners.",
                        style = "display: inline"))
           #p(tags$a(href = "", "",
           #style = "display: inline"))
           
  
           
           
)))



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  #Demographic Findings
  output$download_demo_data <- downloadHandler(
    filename = function() {paste("demo_data_", Sys.Date(), ".csv", sep = "")},
    content = function(file) {write.csv(dem_data, file)}
  )
  
  output$fin_dem_plot1 <- renderPlot({dem_category_plot(selected_state = input$dropdownD)})
  output$fin_dem_plot2 <- renderPlot({dem_sub_cat_and_tool(selected_state = input$dropdownD)})
  output$fin_dem_plot3 <- renderPlot({dem_geography_plot(selected_state = input$dropdownD)})
  output$fin_dem_plot4 <- renderPlot({dem_age_of_data_plot (state = input$dropdownD, data_source = dem_data)})
  output$fin_dem_text5 <- renderText({{paste("Word cloud on tool names for: ", input$dropdownD)}})
  output$fin_dem_plot5 <- renderPlot({tool_cloud(state=input$dropdownD, data_source = dem_data)})
  output$fin_dem_text6 <- renderText({{paste("Word cloud on variables for: ", input$dropdownD)}})
  output$fin_dem_plot6 <- renderPlot({variable_cloud(state=input$dropdownD, data_source = dem_data)})
  output$fin_dem_plot7 <- renderPlot({dem_census_source(selected_state = input$dropdownD)})
  output$fin_dem_text8 <- renderText({{paste("Demographic Data Source (Non Census) \n Distribution in", input$dropdownD)}})
  output$fin_dem_plot8 <- renderDataTable({data <- dem_non_census_source(selected_state = input$dropdownD)
  datatable(data, options = list(pageLength = 5))})
  output$fin_dem_plot9 <- renderPlot({dem_direct_census_link(selected_state = input$dropdownD)})
  output$fin_dem_plot10 <- renderPlot({dem_historical_data(selected_state = input$dropdownD)})


}




shinyApp(ui = ui, server = server)


