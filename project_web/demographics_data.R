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

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

all_states <- c("All sample states", "Alabama", "Alaska","Arizona", "Arkansas", "California",
                "Colorado", "Connecticut", "Delaware", "District of Colombia", 
                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                "Massachusetts", "Michigan", "Minnesoda", "Mississippi", "Missouri",
                "Puerto Rico", "Guam")

dem_data <- read.csv("/home/gcm8gw/Git/dspg23census/Finding_Demographics/SDC_Demographics.csv")
dem_data$Sub.categories = tolower(dem_data$Sub.categories)
dem_data$Tool = tolower(dem_data$Tool)

dem_pie_graph <-function(selected_state, data_table){
  if (selected_state == "All sample states" ){
    data_to_use = data_table
  }
  else{
    data_to_use = data_table[data_table$State..Country== selected_state,]
  }
  count_result <- data_to_use %>% group_by(Data.Sources.Census2) %>% summarize(count = n())
  count_result <- count_result[-1, ]
  colnames(count_result) <- c("Data Source", "Counts")
  print(combined_df)
  #sorted_df <- combined_df[order(- combined_df$count), ]
  pie(combined_df$Counts , labels = combined_df$`data source`, border="white", col=cbPalette, cex=0.5)
}

dem_census_source <- function(selected_state){
  df_stack3 <- data.frame(
    source = character(),
    count = numeric()
  )
  col_name = c("source","count")
  colnames(df_stack3) <- col_name
  
  if (selected_state == "All sample states" ){
    data_to_use = dem_data
  }
  else{
    data_to_use = dem_data[dem_data$State..Country== selected_state,]
  }
  
  for (i in 1:nrow(data_to_use)) {
    #---------------------------estimates----------------------------
      if(any(grepl("Bureau", data_to_use[i,11]))){
        new_row <- c("Census Bureau",54)
        df_stack3 <- rbind(df_stack3, new_row)
      }
      if(any(grepl("Decennial", data_to_use[i,11]))){
        new_row <- c("Decennial Census",54)
        df_stack3 <- rbind(df_stack3, new_row)
      }
      if(any(grepl("Redistricting", data_to_use[i,11]))){
        new_row <- c("Census Redistricting Files", 11)
        df_stack3 <- rbind(df_stack3, new_row)
      }
      if(any(grepl("ACS", data_to_use[i,11]))){
        new_row <- c("American Community Survey",54)
        df_stack3 <- rbind(df_stack3, new_row)
      }
      if(any(grepl("CPS", data_to_use[i,11]))){
        new_row <- c("Current Population Survey",54)
        df_stack3 <- rbind(df_stack3, new_row)
      }
    }
  colnames(df_stack3) <- col_name  
  pie(combined_df$Counts , labels = combined_df$`data source`, border="white", col=cbPalette, cex=0.5)
}

dem_census_source(selected_state = "All sample states", data_table = dem_data)


temp <- dem_data %>%
  group_by(Data.Sources.Census2) %>%
  summarise(count=length(Data.Sources.Census2))


# codes
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


