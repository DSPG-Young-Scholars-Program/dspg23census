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

econ_data <- read.csv("/Users/jianingcai/Documents/GitHub/dspg23census/project_web/economy_compilation.csv")
econ_data$Sub.categories = tolower(econ_data$Sub.categories)

econ_category_plot <- function(selected_state) {
  # Initialize variables to store the counts
  if (selected_state == "All sample states"){
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
          main = paste("Distribution of Data Tools' Category in", selected_state),
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
  
  if (selected_state == "All sample states" ){
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

econ_pie_graph <- function(selected_state, data_table) {
  if (selected_state == "All sample states") {
    data_to_use = data_table
  } else {
    data_to_use = data_table[data_table$State..Country == selected_state, ]
  }
  
  count_result <- data_to_use %>% group_by(Data.Source.Census..Standardized.) %>% summarize(count = n())
  colnames(count_result) <- c("data source", "count")
  count_result <- count_result[count_result$`data source` != "", ]
  
  countinue <- data_to_use %>% group_by(Data.Source.Non.Census..Standardized.) %>% summarize(count = n())
  colnames(countinue) <- c("data source", "count")
  countinue <- countinue[countinue$`data source` != "", ]
  
  combined_df <- rbind(count_result, countinue)
  sorted_df <- combined_df[order(- combined_df$count), ]
  
  # Adding a title to the pie graph
  title <- paste("Economy Data Source Distribution in", selected_state)
  
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
  pie(sorted_df$count, labels = sorted_df$`data source`, border = "white", col = cbPalette, cex = 0.6, main = title)
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
  
  navbarPage(title= tags$a(href = "https://datascienceforthepublicgood.org", target = "_blank", # "_blank" opens the link in a new tab
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
                      panel(h3("Project", style = "color: #1B3766;"),
                                p("The goal of our project is to assist the U.S. Census Bureau in the creation of their Curated Data Enterprise, a tool that will combine data from multiple Census sources, to create a cohesive data lake that can be used as a tool by State governments."),
                                p("To accomplish this, we’ve focused on identifying how state governments use data and identifying what those data sources are, such as Census, State government, or private sources. We’ve started doing this through our data discovery process, which has included a review of individual State Constitutions, and as requested by the Census Bureau, we’re conducting a review of State Data Centers."),
                                p("The overall objective of this project is to report findings that the Census Bureau can use to better address state government data needs and to create a tool that facilitates user data access.   ")
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
                            p("- Treena Goswami*, Postdoc Researcher Associates"),
                        )),
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
                                               all_states)
                                   ),
                                   mainPanel(plotOutput("fin_econ_plot1"),
                                             plotOutput("fin_econ_plot2"),
                                             plotOutput("fin_econ_plot3")
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
  
  output$fin_econ_plot1 <- renderPlot({
    econ_category_plot(selected_state = input$dropdown3)
    
    })
  
  output$fin_econ_plot2 <- renderPlot({
    econ_sub_cat_and_tool(selected_state = input$dropdown3)
  })
  
  output$fin_econ_plot3 <- renderPlot({
    econ_pie_graph(selected_state = input$dropdown3, data_table = econ_data)
  })
  
  output$text2 <- renderText({
    {paste("Word cloud on", input$dropdown2)}
  })
  
}


shinyApp(ui = ui, server = server)
