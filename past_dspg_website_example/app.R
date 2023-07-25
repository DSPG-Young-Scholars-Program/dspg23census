library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(rsconnect)
library(shiny)
library(forcats)
library(plotly)
library(DT)

merged_cert_fam <- read.csv("merged_cert_fam.csv" ) 


ui <- fluidPage(
  theme ="themes.css",
  
  navbarPage(title = HTML("<img src='./DSPG_black-01.png' width='120px' style='margin-top:-10px;'/>"),
             tabPanel("About",style = "margin:45px",
                      fluidRow(
                        #column(3, tags$img(height = "80%", width = "80%", src = "biilogo.png")),
                        column(6, h1("Skilled Technical Workforce (STW)")),
                        column(3, tags$img(height = "80%", width = "80%", src = "partnerlogos.png", align = "right"))
                      ),
                      
                      h5("DSPG21STW Summer Project"),
                      p("Skilled Technical Workforce (STW) jobs are those which do not require a bachelor’s degree for entry, but do require a high level of knowledge in technical domains.  Examples of STW jobs would be Construction Laborers, Mechanics, Firefighters, Nurses/Surgical Assistants."),
                      p("This workforce is important: as of 2017, STW jobs made up approximately 13% of the total US workforce.  They also provide a path to the middle class.  For example, according to the NSF, the median salary of skilled technical workers is $45,000 compared to $29,000 of workers with less than a bachelor’s degree."),
                      p("The National Science Board projects that by 2022, the US will have 3.4 million unfilled STW jobs, so it is important to learn more about how to gain credentials for these occupations."),
                      p("Information about credentials for employment in the skilled technical workforce is inconsistent between data sources and it is common for a credential to be referred to differently depending on the job ad or data source.  During the 10-week DSPG program, we aimed to explore occupation data and certifications with the goal of creating a standardized list of nondegree credentials for jobs in the STW."),
                      p("There are almost a million nondegree credentials (including certifications, licenses, and badges) and 550,000 of these are offered by non-academic institutions.  Our summer work is exploratory, and we worked toward obtaining this standard credential set with various data sources and text matching.  We also explore the portability of these certifications using network analysis, which gives us information about how useful an individual certification is in moving throughout STW jobs."),
                      
                      
                      
                      h5("Our Team"),
                      
                      
                      p("SDAD: Vicki Lancaster and Cesar Montalvo"),
                      p("DSPG: Emily Kurtz (Fellow), Haleigh Tomlin (Intern), Madeline Garrett (Intern)"),
                      p("Sponsor: Gigi Jones, National Science Foundation (NSF), National Center for Science and Engineering (NCSES)"),
                      
                      h5("SDAD/DSPG"),
                      p("The Social and Decision Analytics Division (SDAD) is one of three research divisions within the Biocomplexity Institute and Initiative at the University of Virginia.
                        SDAD combines expertise in statistics and social and behavioral sciences to develop evidence-based research
                        and quantitative methods to inform policy decision-making and evaluation.
                        The researchers at SDAD span many disciplines including statistics, economics, sociology, psychology,
                        political science, policy, health IT, public health, program evaluation, and data science.
                        The SDAD office is located near our nation's capital in Arlington, VA. You can learn more about us at",
                        tags$a(href="https://biocomplexity.virginia.edu/social-decision-analytics.", "https://biocomplexity.virginia.edu/social-decision-analytics."), style = "color:#232D4B"),
                      p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at SDAD. Entering its eighth year, the program engages students from across the country
                        to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today.
                        DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information
                        generated within every community can be leveraged to improve quality of life and inform public policy. ", style = "color:#232D4B")
                      
                      
                      
             ),
             
             tabPanel("Data Sources",style = "margin:45px",
                       fluidRow(
                         #column(3, tags$img(height = "80%", width = "80%", src = "biilogo.png")),
                         column(6, h1("Data Sources")),
                         column(3, tags$img(height = "80%", width = "80%", src = "partnerlogos.png", align = "right"))
                       ),
                      
                      
                      fluidRow(style = "margin-top:100px",
                               column(3, h5("Burning Glass Technologies")),
                               column(6, wellPanel(p(style = "font-size:15px","The Burning Glass Technologies delivers job market analytics that empower employers, workers and educators to make data-driven decisions. The company’s artificial intelligence technology analyzes hundreds of millions of job postings and real-life career transitions to provide insight into workforce demand patterns. This real-time strategic intelligence offers crucial insights, such as which jobs are most in demand, the specific skills employers need and the career directions that offer the highest potential for workers. We used data from only 2019, but for the entire country. For more information, visit burning-glass.com. ")))
                      ),
                      hr(),
                      fluidRow(style = "margin-top:100px",
                               column(3, h5("Occupation Information Network (O*NET)")),
                               column(6, wellPanel(p(style = "font-size:15px","O*NET is a program sponsored by the Department of Labor that establishes and maintains a framework for organizing occupational data aligned with the Bureau of Labor Statistics (BLS) Standard Occupation Classification (SOC) system. The SOC system is the federal standard for classifying workers into occupations based on work performed. We scraped SOC and credential data on the 133 STW occupations from O*NET. We were particularly interested in how credentials and occupations were connected. Thus, for each SOC, we collected information on the Certification Names associated with it, the Certifying Organization for that certification, and the Type of certification, which is a measure of specificity (i.e. “basic” or “advanced.”) This data source was crucial to the majority of our work, and more information on how we collected it can be found on the methods tab.")))
                      ),
                      hr(),
                      fluidRow(style = "margin-top:100px",
                               column(3, h5("Virginia Community College Industry Credentials")),
                               column(6, wellPanel(p(style = "font-size:15px","Virginia’s Community Colleges focus on cultivating a skilled workforce that best matches the needs of regional businesses. Workforce training at Virginia’s Community Colleges includes more than 400 different industry-recognized credentials. These include full industry certifications, from a recognized industry, trade, or professional association validating essential skills of a particular occupation and pathway industry certifications, which may consist of entry-level exams as a component of a suite of exams in an industry certification program leading toward full certification.")))
                      )
                      
                      
                      
                      
                      
             ),
             
             tabPanel("Methods",style = "margin:45px",
                      fluidRow(
                        #column(3, tags$img(height = "80%", width = "80%", src = "biilogo.png")),
                        column(6, h1("Methods")),
                        column(3, tags$img(height = "80%", width = "80%", src = "partnerlogos.png", align = "right"))
                      ),
                      
                      
                      fluidRow(style = "margin-top:100px",
                               column(3, h5("Web Scraping")),
                               column(6, wellPanel(p(style = "font-size:15px","To collect the certification and SOC data from O*NET, we used R’s rvest web scraping package. We looped through the list of 133 Skilled Technical Workforce SOC codes and found all associated certifications for each. O*NET also reports the certifying organization and the type of certification, so we collected that information as well. In the end, we created a data frame with 1137 rows and 4 columns.  ")))
                      ),
                      hr(),
                      fluidRow(style = "margin-top:100px",
                               column(3, h5("Cleaning")),
                               column(6, wellPanel(p(style = "font-size:15px","To match certifications across the O*NET, BGT, and VA Community Colleges data sources, we had to perform some preliminary data cleaning. We used regular expressions to remove acronyms within parentheses in the BGT dataset and states in the Community Colleges dataset. We also removed stop words and stemmed the words in the datasets, removing common, superfluous suffixes. Once the certifications were cleaned within all three datasets, we were ready to perform our text matching.")))
                      ),
                      hr(),
                      fluidRow(style = "margin-top:100px",
                               column(3, h5("Text Matching")),
                               column(6, wellPanel(p(style = "font-size:15px","Filling crucial jobs in the Skilled Technical Workforce requires coordination between governments, industries, and educational institutions. Unfortunately, there are inconsistencies in the language these three bodies use to describe the certifications required for these jobs. We used text matching methods to address some of these inconsistencies. To do this, we used the stringdist and lingmatch R packages. Specifically, we used the stringdist function within the stringdist package to calculate the Damerau-Levenshtein distance between each pair of certifications across our three datasets. The lingmatch package allowed us to easily create matrices where words were represented by columns and certifications were represented by rows. This aided the distance calculations across each pair of data sources.  ")))
                      ),
                      hr(),
                      fluidRow(style = "margin-top:100px",
                               column(3, h5("Network Analysis")),
                               column(6, wellPanel(p(style = "font-size:15px","We used the R package igraph in order to visualize the links between occupations and certifications within the Skilled Technical Workforce. By visualizing these links and combining supplemental information, such as the projected growth of specific occupations in the STW, we are able to analyze the paths workers can take in the STW.  ")))
                      )
                      
                      
                      
                      
             ),#),#end navbar
             
             #ui
             #navbarMenu("Data Profiling",
                        

                         
                        tabPanel("Data Profiling", style = "margin:60px",
                                 fluidRow(
                                   #column(3, tags$img(height = "80%", width = "80%", src = "biilogo.png")),
                                   column(6, h1("Data Profiling")),
                                   column(3, tags$img(height = "80%", width = "80%", src = "partnerlogos.png", align = "right"))
                                 ),
                                 
                                 
                                 #h5("Profiling", align = "center"),
                                 p(style = "margin-top:25px","The profiling of the Burning Glass Technologies (BGT) data seeks to determine the quality and fitness-for-use of the information. Nine variables were selected for the profiling, including, job identification, Standard Occupational Classification code, latitude, longitude, state, name of occupation, minimum years of experience, minimum salary, and certification." ),
                                 br(),
                                 p("Three metrics offered a glimpse about the completeness, uniqueness, and duplicates of data. Completeness corresponds to the percentage indicating the number of observations that have values compared to the number of observations that “should” have values. Uniqueness is defined as the number of unique valid entries for each variable. Duplication shows the precent of replication of distinct observations per observation unit type."),
                                 br(),
                                 p("Before we evaluate completeness, duplicates, and uniqueness of variables, we had to do some manipulation to the data set. To successfully filter for STW jobs down the line, we needed to remove job-ads from BGT Main that did not have a SOC code. This decreased the size of the original data set from 33,859,698 to 32,488,447, removing 1,371,251 rows. Next, we had to remove “-” from SOC variable and make SOC codes integers in both the BGT Main and STW data sets. We were then able to merge the data by matching on the SOC variable, so that the data only contained job-ads that were in the STW. This decreased our data set down to 2,234,115 rows. This shows that 5.2% of the BGT job ads with SOC codes are in the STW. Next, we needed add in certification data. We merged BGT Main and BGT Certification datasets using the job id variable. This decreased our data set to 1,763,507, indicating that 78.9% of STW job ads have credentials listed."),
                                 sidebarLayout(
                                   sidebarPanel(
                                     width = 6,
                                     selectInput("selectTable", "Select", choices = c("Completeness, Uniqueness, Duplicates")),
                                     h4("Definitions: ", style = "margin-top:50px"),
                                     helpText("Note: All definitions are provided by Dow Jones Developer Platform"),
                                     tags$ul(
                                       tags$li("soc code: SOC code of the job assigned using BGT occupation coding rules. SOC codes are always the first 6- digits of a job’s O*NET code. We use SOCs based off of the most recent 2010 SOC delineations.The Standard Occupational Classification (SOC) system is a standard used by federal agencies to classify workers into occupational categories. As of 2018, there are 867 occupations within 23 major groups. Link to source: https://www.bls.gov/soc/"),
                                       tags$li("Latitude: Latitude for the Canonicalized Location."),
                                       tags$li("Longitude: Longitude for the Canonicalized Location."),
                                       tags$li("Fipsstate: FIPS is a 5-digit code, representing the concatenation of the state + county FIPS codes. Ex. 29019 is the FIPS code for Boone County, MO, where the first 2-digits, 29, represent MO, and the last 3-digits, 019, represent Boone County in MO."),
                                       tags$li("Jobid: A unique ID generated by Burning Glass. BGTJobId is used to link this table with both the main table, and subsequent tables. "),
                                       tags$li("Certification: Certification is a standardized version of the certifications listed in the posting to enable improved search and categorization.Note that there may be multiple certifications per job posting (or, alternatively, none at all). As such, the data in this table is presented vertically, where each certification for a particular job posting will be in its own row, with the job posting’s BGTJobId in its own column to the left of each certification, identifying which job posting each certification came from. "),
                                       tags$li("Name: Name of the occupation associated with a given SOC Code "),
                                       tags$li("MinSalary: Minimum Annual salary derived from Canonicalized Minimum salary. If hourly rate is provided, this rate is coverted to annual salary through multiplying the rate by 2080."),
                                       tags$li("MinExperience: Computed minimum of the required experience range in years.")
                                     )
                                     
                                   ),
                                   mainPanel(width = 3, tableOutput("tables"))
                                 )),
             #),
             #end profiling tab------------------------------------------
             
             
             
             
             

             
             #end Data Sources and Methods tabs-----------------
             
             
             navbarMenu("Results",
                        tabPanel("Certifications and Occupations", style = "margin:20px",
                                 
                                 h5("Top Certifications"),
                                 p(style = "margin-top:25px","This figure shows the top 30 Burning Glass Technology Certifications for each SOC Occupation family in the Skilled Technical Workforce. Two types of credentials were removed from the analysis: driver’s license, including the type CDL Class A, B, C and D; and credentials identified as security clearance.  These two types of information refer to requisites for job applications rather than certifications accredited by an academic institution." ),
                                 
                                 br(),
                                 br(),
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Top Certifications for each Occupation Family"),
                                     selectInput("family", "Occupation Family", choices = list(
                                       'Management' = '11' ,
                                       'Business and Financial Operations' = '13' ,
                                       'Architecture and Engineering' = '17' ,
                                       'Life, Physical, and Social Science' = "19" ,
                                       'Arts, Design, Entertainment, Sports, and Media' = "27" ,
                                       'Healthcare Practitioners and Technical' = "29" ,
                                       'Protective Service' = "33" ,
                                       'Food Preparation and Serving Related' = "35" ,
                                       'Office and Administrative Support' = "43" ,
                                       'Farming, Fishing, and Forestry' = "45" ,
                                       'Construction and Extraction' = "47" ,
                                       'Installation, Maintenance, and Repair' = "49" ,
                                       'Production' = "51" ,
                                       'Transportation and Material Moving' = "53"
                                     ))),
                                   mainPanel(
                                     plotlyOutput('plot_20top_certificates' , width = "auto" ,  height = "700"))
                                 )
                                 
                        ),
                        
                        tabPanel("Credential Matching",
                                 h5("Credential Matching Across Data Sources"),
                                 p("We used three main data sources in our project. These sources represented the three main players in the training and employing of STW workers. First, the O*NET data, coming from the U.S. Government's Bureau of Labor Statistics, represented government entities. Second, the Burning Glass data, pulling job ads from employers, represented industries. Finally, the Virginia Community College data set represented educational institutions."),
                                 p("Each data source included credentials by name, but there were quite a few inconsistencies with these names. We thus matched the text across all three pairs of data sources: Burning Glass to O*NET, VA community colleges to O*NET, and Burning Glass to VA community colleges. We treat O*NET as the 'truth,' except in the Burning Glass to VA community colleges comparison, where we treat the community college data set as the truth. Below, you can pick a pairwise comparison and see our findings. Note that many credentials match up perfectly, but many more do not match well at all. This highlights the information gaps in the field of the STW."),
                                 selectInput("across", "Select", choices = c("ONETxBGT", "ONETxVA", "VAxBGT")),
                                 dataTableOutput("AcrossData")
                                 
                                 
                                 
                        ),
                        
                        
                        tabPanel("Boxplots - Confidence of Text Matching",style="margin:20px",
                                 h5("Boxplots - Confidence of Text Matching", align = "center"),
                                 p(style = "margin-top:25px","To visualize the relationships between the named credentials, we created boxplots. One natural question to ask is whether there are more precise matches in how credentials are referenced within certain types of jobs. Within the Skilled Technical Workforce, there are 14 occupational groups."),
                                 #https://www.bls.gov/soc/2018/major_groups.htm
                                 
                                 p("The boxplot below shows the confidences for our text matching algorithm between Burning Glass credentials and O*NET credentials for each of these occupational groups. While some groups' confidences are somewhat higher than others, all results are somewhat low. This highlights the gaps between industries, governments, and educational institutions, and shows these gaps manifest even in the language used to discuss occupations and credentials. " ),
                                 #br(),
                                 #p("Originally, the dataset contained 1,942,855 data entries. Given a restriction on memory and running power, we decided to only have unique and complete entries as it diminished the dataset by 96.2% to 73, 688 entries, while still fulfilling our main goal of understanding what companies are producing innovation. The visualization [above/below] demonstrates the total percentage of data entries that passed our validity checks.  About 78.3% of the total unique entries passed the validity check, 100% of the entries were published after 2010, and 91.7% of the entries contained valid company codes.  "),
                                 img(src = "onetbgtconfidence.png", height = 600, width = 1000),
                                 p("We can also look at the confidences when matching Burning Glass data to the Virginia Community College offerings. Again, confidences are pretty uniformly low. The variances, however, are higher for these comparisons."),
                                 img(src = "vabgt.png", height = 600, width = 1000),
                                 p("For Virginia Community College to O*NET comparisons, we were interested in certifying organizations. We did not use occupational groups as supplementary information when making these comparisons. Comparisons across certifying organizations could technically be made, but there are over 200, so we do not include a boxplot here.")
                                 
                                 
                                 
                        ),#end results tab 
                        
                        tabPanel("Network Analysis - Occupation Clusters", style = "margin:20px",
                                 h5("Occupation Clusters"),
                                 p("Displayed are the network graphs for STW and STEM (Science, Technology, Engineering, and Mathematics) occupations in the occupation clusters, Health Sciences, Cybersecurity, and Manufacturing.
                                       In all three networks, STW occupations are identified with yellow squares and STEM occupations with grey squares, inside the square are the Standard Occupational Classification System (SOC) titles.
                                       Only STEM occupations that require a bachelor’s degree or less are included. Each square is connected to the certifications listed for that occupation in O*NET online.
                                       The goal is to identify certifications that are portable across STW and STEM occupations"),
                                 br(),
                                 br(),
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Occupation"),
                                     selectInput("network", "Occupation", choices = c("Health Sciences", "Cybersecurity", "Manufacturing")),
                                     textOutput("vardescrip")),
                                   # tableOutput("nettab1"),
                                   mainPanel(
                                     imageOutput("netgraph"))),
                                 fluidRow(
                                   column(3, wellPanel(
                                     tags$b("Network Statistics & Observations"),
                                     uiOutput("selectedvar1"))
                                   ))),
                        #<<<<<<< HEAD
                        tabPanel("Network Analysis - STW by Major Occupation Group", style = "margin:20px",
                                 h5("STW by Major Occupation Group"),
                                 p("The network is displayed for the all 133 STW occupations and separately for seven of the major occupation groups within the STW. The squares represent occupations, and they are colored to represent 2019-2029 Bureau of Labor Employment Projections. The two shades of blue indicate occupations that will decline in number over the coming years, the two shades of yellow identify occupations that will increase, and red indicates occupations whose numbers will increase by more than 50 percent."),
                                 br(),
                                 br(),
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Occupation Group"),
                                     selectInput("occ_group", "Occupation", choices = c("Entire Network",
                                                                                        "Architecture and Engineering Occupations",
                                                                                        "Art, Design, Entertainment, Sports, and Media Occupations",
                                                                                        "Healthcare Practitioners and Technical Occupations",
                                                                                        "Construction and Extraction Occupations",
                                                                                        "Installation, Maintenance, and Repair Occupations",
                                                                                        "Production Occupations",
                                                                                        "Transportation and Material Moving Occupations")),
                                     uiOutput("occ_descr"),
                                     
                                     img(height = "100%", width = "100%", src = "test.png")),
                                   #tableOutput("nettab2")),
                                   mainPanel(
                                     imageOutput("occ_graph"))),
                                 fluidRow(
                                   column(3, wellPanel(
                                     tags$b("Network Statistics & Observations"),
                                     uiOutput("selectedvar2"))
                                   )))#end results tab 

                        
             ) #end navbarPage
             
             
)) #end fluid page





server <- function(input, output) {
  
  output$plot_20top_certificates <- renderPlotly({
    
    # When input$n is 3, filename is ./images/image3.jpeg
    dat <- merged_cert_fam %>% filter(fam==input$family, 
                                      certification != "Driver's License",
                                      certification != "CDL Class A",
                                      certification != "Cdl Class B",
                                      certification != "Cdl Class C",
                                      certification != "CDL Class D",
                                      certification != "Security Clearance"
    ) %>%
      arrange(desc(number)) %>%
      slice(1:30)
    
    # Return a list containing the filename and alt text
    plot_cert <- ggplot(data = dat, mapping = aes(x = reorder( factor(name_cert), number), number)) + 
      geom_bar(stat = "identity") + coord_flip()+
      labs(x='', y='Number',
           #title = 'Top 20 Certifications for   Occupation')
           title = paste('Top 30 Certifications for', 'Occupation') )
    
    
    plot_cert
    
    
    
  })
  
  output$tables <- renderTable({
    if(input$selectTable == "Completeness, Uniqueness, Duplicates"){
      
      
      profTable <- read.csv("profilingTable.csv")
      
      names(profTable)[names(profTable) == "X"] <- "Column Name"
      
      profTable
    }
    
  })
  
  
  
  
  output$AcrossData <- renderDataTable({
    if(input$across == "ONETxBGT"){
      acrossTable <- read.csv("onetxbg.csv")
      
      datatable(acrossTable, options=list(pageLength = 10))
    }else if(input$across == "ONETxVA"){
      acrossTable <- read.csv("onetva.csv")
      datatable(acrossTable, options=list(pageLength = 10))
    }else{
      acrossTable <- read.csv("vabgt.csv")
      
      datatable(acrossTable, options=list(pageLength = 10))
    }
    
  })
  
  output$netgraph <- renderImage({
    
    # When input$network is nursing, filename is ./www/nursing.png
    filename <- normalizePath(file.path('www',
                                        paste(input$network, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste(input$network, "Network"),
         width = 1000,
         height = 1000)
    
    
    
    
    
  }, deleteFile = FALSE)
  
  output$vardescrip <- renderText({
    if (input$network == "Health Sciences"){
      paste("The Health Sciences Occupation Cluster is defined by O*NET. O*NET occupation clusters include occupations in the same filed that require similar skills, knowledge, and abilities. The Health Science cluster contains six STW occupations.")
    }
    
    else if (input$network == "Cybersecurity"){
      paste("There is no Cybersecurity Occupation Cluster in O*NET, so occupations in the cluster were identified using two sources. The first used O*NET online and searched on the term “cyber” keeping only those occupations in the STW. The second used information on the National Institute of Standards and Technology’s National Initiative for Cybersecurity Education (NICE) website. In conjunction with CompTIA and Burning Glass, NICE developed CyberSeek which provides detailed information about the credentials demanded by employers and career pathways in cybersecurity that maps opportunities for advancement in the field. Feeder and entry role occupations listed on CyberSeek along with all the common job titles were entered into O*NET online and those occupations in the STW and STEM occupations that require a bachelor’s degree or less were included.")
    }
    
    else {
      paste("The Manufacturing Occupation Cluster is defined by O*NET career clusters. O*NET career clusters include occupations in the same filed that require similar skills, knowledge, and abilities. There are 148 occupations in the Manufacturing cluster, thirty-three (22%) are STW occupations.")
    }
  })
  
  output$selectedvar1 <- renderUI({
    if (input$network == "Health Sciences"){
      HTML("<ul><li>Network size: 485 nodes</li><li>Network density (# of edges / # of possible edges): 0.004</li><li>Number of components: 14</li><li>Diameter (the greatest distance between any pair of connected vertices): 10</li></ul>")
    }
    
    else if (input$network == "Cybersecurity"){
      HTML("<ul><li>Network size: 722 nodes</li><li>Network density (# of edges / # of possible edges): 0.003</li><li>Number of components: 2</li><li>Diameter (the greatest distance between any pair of connected vertices): 10</li></ul>")
    }
    
    else {
      HTML("<ul><li>Network size: 294 nodes</li><li>Network density (# of edges / # of possible edges): 0.007</li><li>Number of components: 23</li><li>Diameter (the greatest distance between any pair of connected vertices): 8</li></ul>")
    }
    
  })
  
  
  
  
  #<<<<<<< HEAD
  #The two below may need to be in an if/else statement to work with the 2 dropdowns.
  output$occ_group <- renderImage({
    
    # When input$network is nursing, filename is ./www/nursing.png
    filename <- normalizePath(file.path('www',
                                        paste(input$occ_group, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste(input$occ_group, "Network"),
         width = 900,
         height = 900)})
  
  
  #=======
  
  
  
  #<<<<<<< HEAD
  output$occ_graph <- renderImage({
    
    # When input$network is nursing, filename is ./www/nursing.png
    filename <- normalizePath(file.path('www',
                                        paste(input$occ_group, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste(input$occ_graph, "Network"),
         width = 900,
         height = 900)
    
    
    
  }, deleteFile = FALSE)
  
  output$"occ_descr" <- renderUI({
    if (input$occ_group == "Entire Network"){
      HTML("<ul><li>11: Management</li><li>13: Business and Financial Operations</li><li>17: Archictecture and Engineering</li><li>19: Life, Physical, and Social Science</li><li>27: Art, Design, Entertainment, Sports, and Media</li><li>29: Healthcare Practitioners and Technical Occupations</li><li>33: Protective Service</li><li>35: Food Preparation and Serving</li><li>43: Office and Administrative Support</li><li>45: Farming, Fishing, and Forestry</li><li>47: Construction and Extraction</li><li>49: Installation, Maintenance, and Repair</li><li>51: Production Occupations</li><li>53: Transportation and Material Moving</li></ul>")
    }
  })
  
  
  
  output$selectedvar2 <- renderUI({
    if (input$occ_group == "Entire Network"){
      HTML("<ul><li>Network size: 1054 nodes</li><li>Network density (# of edges / # of possible edges): 0.002</li><li>Number of components: 50</li><li>Diameter (the greatest distance between any pair of connected vertices): 21</li></ul>")
    }
    
    else if (input$occ_group == "Architecture and Engineering Occupations"){
      HTML("<ul><li>Network size: 120 nodes</li><li>Network density (# of edges / # of possible edges): 0.018</li><li>Number of components: 6</li><li>Diameter (the greatest distance between any pair of connected vertices): 6")
    }
    else if (input$occ_group == "Art, Design, Entertainment, Sports, and Media Occupations"){
      HTML("<ul><li>Network size: 64 nodes</li><li>Network density (# of edges / # of possible edges): 0.03</li><li>Number of components: 5</li><li>Diameter (the greatest distance between any pair of connected vertices): 4")
    }
    else if (input$occ_group == "Healthcare Practitioners and Technical Occupations"){
      HTML("<ul><li>Network size: 31 nodes</li><li>Network density (# of edges / # of possible edges): 0.062</li><li>Number of components: 4</li><li>Diameter (the greatest distance between any pair of connected vertices): 3")
    }
    else if (input$occ_group == "Construction and Extraction Occupations"){
      HTML("<ul><li>Network size: 216 nodes</li><li>Network density (# of edges / # of possible edges): 0.005</li><li>Number of components: 7</li><li>Diameter (the greatest distance between any pair of connected vertices): 1")
    }
    else if (input$occ_group == "Installation, Maintenance, and Repair Occupations"){
      HTML("<ul><li>Network size: 345 nodes</li><li>Network density (# of edges / # of possible edges): 0.003</li><li>Number of components: 18</li><li>Diameter (the greatest distance between any pair of connected vertices): 1")
    }
    else if (input$occ_group == "Production Occupations"){
      HTML("<ul><li>Network size: 85 nodes</li><li>Network density (# of edges / # of possible edges): 0.023</li><li>Number of components: 11</li><li>Diameter (the greatest distance between any pair of connected vertices): 4")
    }
    
    else {
      HTML("<ul><li>Network size: 55 nodes</li><li>Network density (# of edges / # of possible edges): 0.034</li><li>Number of components: 4</li><li>Diameter (the greatest distance between any pair of connected vertices): 2")
    }
  })
  #=======
  
  #>>>>>>> 3814624d0ea896e9d7d428b327547af7d7df317d
}

# Run the application
shinyApp(ui = ui, server = server)





