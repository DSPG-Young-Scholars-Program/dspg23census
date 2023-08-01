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
library(gcookbook)
library(stringr)
library(reshape2)
library(knitr)
library(kableExtra)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

all_states <- c("All sample states", "Alabama", "Alaska","Arizona", "Arkansas", "California",
                "Colorado", "Connecticut", "Delaware", "District of Colombia", 
                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                "Massachusetts", "Michigan", "Minnesoda", "Mississippi", "Missouri",
                "Puerto Rico", "Guam")

dem_data <- read.csv("/home/gcm8gw/Git/dspg23census/project_website/Finding_Demographics/SDC_Demographics.csv")
dem_data$Sub.categories = tolower(dem_data$Sub.categories)
dem_data$Tool = tolower(dem_data$Tool)

dem_data$Name.of.tool
dem_data$Variables.Used..list.all.that.apply.

### Census Sources
dem_census_source <- function(selected_state){
  df_stack3 <- data.frame(
    source = character()
  )
  col_name = c("source")
  colnames(df_stack3) <- col_name
  
  if (selected_state == "All sample states" ){
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

dem_census_source(selected_state = "All sample states")


###  Non Census Sources
dem_non_census_source <- function(selected_state){
  df_stack3 <- data.frame(
    source = character()
  )
  col_name = c("source")
  colnames(df_stack3) <- col_name
  
  if (selected_state == "All sample states" ){
    data_to_use = dem_data
  }
  else{
    data_to_use = dem_data[dem_data$State..Country== selected_state,]
  }
  
  # Split the rows by commas and drop duplicates
  sources <- unique(unlist(strsplit(data_to_use$Data.Sources.Non.Census2, ",\\s*")))
  
  # Create a new data frame with a single column named "source" containing the sources
  df_stack3 <- data.frame(Source = sources)
  
}
  
  
dem_non_census_source(selected_state = "All sample states")





# Plot 5

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



#Word cloud for variable names
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

variable_cloud(state = 'All Sample States and Territories', data_source = dem_data)


# Geographic levels
dem_data$Geographic.Levels2 = tolower(dem_data$Geographic.Levels2)

dem_geography_plot <- function(selected_state) {
  # Initialize variables to store the counts
  if (selected_state == "All Sample States and Territories"){
    data_to_use = dem_data$Geographic.Levels2
  }
  else{
    data_to_use = dem_data$Geographic.Levels2[dem_data$State..Country == selected_state]
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
    if (any(grepl("projections", i))) {
      projections <- projections + 1
    }
    if (any(grepl("projections", i))) {
      projections <- projections + 1
    }
    if (any(grepl("projections", i))) {
      projections <- projections + 1
    }
    if (any(grepl("projections", i))) {
      projections <- projections + 1
    }
    if (any(grepl("projections", i))) {
      projections <- projections + 1
    }
    if (any(grepl("projections", i))) {
      projections <- projections + 1
    }
    if (any(grepl("projections", i))) {
      projections <- projections + 1
    }
    if (any(grepl("projections", i))) {
      projections <- projections + 1
    }
    if (any(grepl("projections", i))) {
      projections <- projections + 1
    }
    if (any(grepl("projections", i))) {
      projections <- projections + 1
    }
    if (any(grepl("projections", i))) {
      projections <- projections + 1
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
  barplot(counts, names.arg = category, col = cbPalette,
          main = paste("Types of Sub-category:", selected_state),
          xlab = "Category: Demographics", ylab = "Counts", cex.names = 0.9, ylim = c(0, 250))
  # Add counts as text above the bars
  text(x = c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), y = counts,
       labels = counts, pos = 3, cex = 0.8, col = "black")
}

############################################################################

# codes

temp <- dem_data %>%
  group_by(Data.Sources.Non.Census2) %>%
  summarise(count=length(Data.Sources.Census2))


temp1 <- dem_data %>%
  mutate(acs=as.numeric(str_detect(Data.Sources.Census2, 'ACS')),
         bureau=as.numeric(str_detect(Data.Sources.Census2, 'Bureau')),
         decennial=as.numeric(str_detect(Data.Sources.Census2, 'Decennial')),
         redistricting=as.numeric(str_detect(Data.Sources.Census2, 'Redistricting')),
         cps=as.numeric(str_detect(Data.Sources.Census2, 'CPS')))

temp2 <- temp1 %>%
  group_by(state=`State..Country`) %>%
  summarise(acs=sum(acs),
            bureau=sum(bureau),
            decennial=sum(decennial),
            redistricting= sum(redistricting),
            cps=sum(cps)) 

allstate <- temp2 %>%
  summarise(state='all states',
            acs=sum(acs),
            bureau=sum(bureau),
            decennial=sum(decennial),
            redistricting= sum(redistricting),
            cps=sum(cps))

temp3 <- rbind(temp2, allstate)

temp4 <- melt(temp3, id.vars = 'state') %>%
  mutate(data_source=case_when(
    variable=='acs' ~ 'American Community Survey',
    variable=='bureau' ~ 'Census Bureau',
    variable=='decennial' ~ 'Decennial Census',
    variable=='cps' ~ 'Current Population Survey',
    variable=='redistricting' ~ 'Census Redistricting Files'
  )) %>%
  select(state,data_source,value)

subset <- temp4 %>% filter(state=='all states')
pie(subset$value , labels = subset$data_source, border="white", col=cbPalette, cex=0.5)


