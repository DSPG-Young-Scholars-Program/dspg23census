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

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

all_states <- c("All sample states", "Alabama", "Alaska","Arizona", "Arkansas", "California",
                "Colorado", "Connecticut", "Delaware", "District of Colombia", 
                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                "Massachusetts", "Michigan", "Minnesoda", "Mississippi", "Missouri",
                "Puerto Rico", "Guam")

dem_data <- read.csv("/home/gcm8gw/Git/dspg23census/project_web/SDC_Demographics.csv")
dem_data$Sub.categories = tolower(dem_data$Sub.categories)

dem_category_plot <- function(selected_state) {
  # Initialize variables to store the counts
  if (selected_state == "All sample states"){
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

dem_data$Tool = tolower(dem_data$Tool)

dem_sub_cat_and_tool <- function(selected_state){
  df_stack2 <- data.frame(
    tool = character(),
    sub = character(),
    count = numeric()
  )
  col_name = c("tool","sub","count")
  colnames(df_stack2) <- col_name
  
  if (selected_state == "All sample states" ){
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

dem_pie_graph <-function(selected_state, data_table){
  if (selected_state == "All sample states" ){
    data_to_use = data_table
  }
  else{
    data_to_use = data_table[data_table$State..Country== selected_state,]
  }
  count_result <- data_to_use %>% group_by(Data.Source.Census2) %>% summarize(count = n())
  count_result <- count_result[-1, ]
  countinue <- data_to_use %>% group_by(Data.Source.Non.Census) %>% summarize(count = n())
  countinue <- countinue[-1, ]
  colnames(count_result) <- c("Data Source", "Counts")
  colnames(countinue) <- c("Data Source", "Counts")
  combined_df <- rbind(count_result, countinue)
  sorted_df <- combined_df[order(- combined_df$count), ]
  pie(sorted_df$count , labels = sorted_df$`data source`, border="white", col=cbPalette, cex=0.5)
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
                      br(),
                      sidebarLayout(sidebarPanel(
                        selectInput("dropdown3", "Which state are you interested in?",
                                    all_states)
                      ),
                      mainPanel(#textOutput("text3"),
                        plotOutput("fin_dem_1"),
                        plotOutput("fin_dem_2"),
                        plotOutput("fin_dem_3")
                      ))),
             tabPanel("Economy"),
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
  
  output$fin_dem_1 <- renderPlot({
    dem_category_plot(selected_state = input$dropdown3)
    
  })
  
  output$fin_dem_2 <- renderPlot({
    dem_sub_cat_and_tool(selected_state = input$dropdown3)
  })
  
  output$fin_dem_3 <- renderPlot({
    dem_pie_graph(selected_state = input$dropdown3, data_table = dem_data)
  })
  
  output$text2 <- renderText({
    {paste("Word cloud on", input$dropdown2)}
  })
  
}


shinyApp(ui = ui, server = server)

