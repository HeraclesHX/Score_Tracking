library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(shinydashboard)
library(tidyr)


tagList(
  # shinythemes::themeSelector(),
  navbarPage(
    # theme = "cosmo",  # <--- To use a theme, uncomment this
    theme = shinytheme("superhero"),
    "Xi'an Janssen",
    id = "panels",
    
    tabPanel("HOME PAGE",
             fluidPage(
               # fluidRow(h3("Outline")),
               fluidRow(
                 h3("Outline", 
                    style = "font-family: 'Lobster', cursive;
                    font-weight: 500; line-height: 1.1; 
                    color: #ad1d28;")
                 ),
               fluidRow(h4("1. Overview of Survey Physicians")),
               fluidRow(actionLink("link_to_1.1", 
                                   "1.1 Tracking of Number of Survey Physicians")),
               fluidRow(actionLink("link_to_1.2", 
                                   "1.2 Distributions of Survey Physicians")),
               fluidRow(actionLink("link_to_1.3", 
                                   "1.3 Tracking of Physicians Score and Ratio")),
               fluidRow(actionLink("link_to_1.4", 
                                   "1.4 Distributions of Overall Physicians Scores")),
               fluidRow(actionLink("link_to_1.5", 
                                   "1.5 Attitude to Sporanox and Promotional Activities")),
               fluidRow(h4("2. Physician Perception Progression")),
               fluidRow(actionLink("link_to_2.1", 
                                   "2.1 Perception Progression in Physician")),
               fluidRow(actionLink("link_to_2.2", 
                                   "2.2 Distribution of Physician with Scores Progression Variation")),
               fluidRow(actionLink("link_to_2.3", 
                                   "2.3 Attitude to Sporanox and Promotional Activities for Physicians with Score Progression")),
               fluidRow(h4("3. Overview of Promotion Activities")),
               fluidRow(actionLink("link_to_3.1", 
                                   "3.1 Overview of Promotion Activities for Physicians at Different Level")),
               fluidRow(actionLink("link_to_3.2", 
                                   "3.2 Tracking of Promotion Activities for Physicians at Different Level")),
               fluidRow(h4("4. Promotion Activities for Physician with Progression")),
               fluidRow(actionLink("link_to_4.1", 
                                   "4.1 Relationship of Promotion Activities and Physician Perception Progression")),
               fluidRow(actionLink("link_to_4.2", 
                                   "4.2 Tracking of Promotion Activities for Physicians with Progression and Overall"))
               
               
             )),
    
    navbarMenu("1. Overview of Survey Physicians",
               tabPanel("1.1 Tracking of Number of Survey Physicians",
                        sidebarPanel(
                          tags$h3("Tracking of Survey Physicians and Newly Added Number by Quarter"),
                          actionLink("link_to_home_page_1.1", 
                                     "Back To Home Page")
                        ),
                        
                        mainPanel(
                          fluidRow(
                            plotlyOutput("total_doc_bar")
                          ),
                          fluidRow(
                            tags$h2("")
                          ),
                          fluidRow(
                            plotlyOutput("new_doc_bar")
                          )
                        )
               ),
               
               tabPanel("1.2 Distributions of Survey Physicians",
                        sidebarPanel(
                          tags$h3("Distributions of Survey Physicians Will Be Provided Quarterly"),
                          selectizeInput("year", "Year",
                                         choices = NULL,
                                         multiple = FALSE),
                          selectizeInput("quarter",
                                         "Quarter", 
                                         choices = NULL,
                                         multiple = FALSE),
                          actionLink("link_to_home_page_1.2", 
                                     "Back To Home Page")
                        ),
                        mainPanel(
                          #- several distribution of physicians
                          fluidRow(
                            # h4("Total Physicians' Count by Quarter"),
                            splitLayout(cellWidths = c("50%", "50%"), 
                                        plotlyOutput("region_pie"), 
                                        plotlyOutput("level_pie"))
                          ),
                          fluidRow(
                            tags$h2("")
                          ),
                          fluidRow(
                            # h4("Total Physicians' Count by Quarter"),
                            splitLayout(cellWidths = c("50%", "50%"), 
                                        plotlyOutput("department_pie"))
                          )
                        )
               ),
               tabPanel("1.3 Tracking of Physicians Score and Ratio",
                        sidebarPanel(
                          tags$h3("Tracking of Physicians Score and Ratio Based on Their Highest Scores by Quarter"),
                          selectizeInput("level",
                                         "Doctor Tier", 
                                         choices = NULL,
                                         multiple = FALSE),
                          actionLink("link_to_home_page_1.3", 
                                     "Back To Home Page")
                        ),
                        mainPanel(
                          #- Tracking of physicians score and ratio 
                          # fluidRow(
                          #   # h4("Total Physicians' Count by Quarter"),
                          #   splitLayout(cellWidths = c("50%", "50%"),
                          #               plotlyOutput("doc_line"),
                          #               plotlyOutput("doc_cnt_stk_bar"))
                          #   )
                          fluidRow(
                            plotlyOutput("doc_line")
                          ),
                          fluidRow(
                            tags$h2("")
                          ),
                          fluidRow(
                            plotlyOutput("doc_cnt_stk_bar")
                          )
                        )
               ),
               tabPanel("1.4 Distributions of Overall Physicians Scores",
                        sidebarPanel(
                          tags$h3("Distributions of Overall Physicians Scores according to Their Latest Response Will Be Investigated Quarterly"),
                          selectizeInput("year1", "Year",
                                         choices = NULL,
                                         multiple = FALSE),
                          selectizeInput("quarter1",
                                         "Quarter", 
                                         choices = NULL,
                                         multiple = FALSE),
                          actionLink("link_to_home_page_1.4", 
                                     "Back To Home Page")
                        ),
                        mainPanel(
                          #- Tracking of distribution of physicians score and ratio 
                          fluidRow(
                            # h4("Total Physicians' Count by Quarter"),
                            plotlyOutput("region_stk")
                          ),
                          fluidRow(
                            tags$h2("")
                          ),
                          fluidRow(
                            # h4("Total Physicians' Count by Quarter"),
                            plotlyOutput("level_stk")
                          ),
                          fluidRow(
                            tags$h2("")
                          ),
                          fluidRow(
                            # h4("Total Physicians' Count by Quarter"),
                            plotlyOutput("department_stk")
                          )
                        )
               ),
               tabPanel("1.5 Attitude to Sporanox and Promotional Activities",
                        sidebarPanel(
                          tags$h3("Attitude to Sporanox and Promotional Activities for All Physicians(Q15&Q16)"),
                          selectizeInput("year2", "Year",
                                         choices = NULL,
                                         multiple = FALSE),
                          selectizeInput("quarter2",
                                         "Quarter", 
                                         choices = NULL,
                                         multiple = FALSE),
                          actionLink("link_to_home_page_1.5", 
                                     "Back To Home Page")
                        ),
                        
                        mainPanel(
                          fluidRow(
                            plotlyOutput("q15_line")
                          ),
                          fluidRow(
                            tags$h2("")
                          ),
                          fluidRow(
                            plotlyOutput("q16_line")
                          )
                        ))
    ),
    navbarMenu("2. Physician Perception Progression",
               tabPanel("2.1 Perception Progression in Physician",
                        sidebarPanel(
                          tags$h3("Perception Progression in Overall Physician or A Certain Field"),
                          selectInput("field", "Select A Field to Investigate",
                                      list(`Region` = c("region"),
                                           `Doctor Level` = c("tier"),
                                           `Department` = c("department")),
                                      selected = "Region"),
                          
                          selectizeInput("item", "Select A Region/ Doctor Level/ Department",
                                         choices = NULL,
                                         multiple = FALSE),
                          
                          
                          actionLink("link_to_home_page_2.1", 
                                     "Back To Home Page")
                        ),
                        
                        mainPanel(
                          fluidRow(
                            plotlyOutput("track_pb_overall")
                          ),
                          fluidRow(
                            tags$h1("")
                          ),
                          fluidRow(
                            plotlyOutput("pb_in_field")
                          )
                        )),
               
               tabPanel("2.2 Distribution of Physician with Scores Progression Variation",
                        sidebarPanel(
                          tags$h3("The Distribution for Physician with Scores Progression Variation"),
                          selectizeInput("year3", "Year",
                                         choices = NULL,
                                         multiple = FALSE),
                          selectizeInput("quarter3", "Quarter",
                                         choices = NULL,
                                         multiple = FALSE),
                          actionLink("link_to_home_page_2.2", 
                                     "Back To Home Page")
                        ),
                        
                        mainPanel(
                          # fluidRow(
                          #   splitLayout(cellWidths = c("50%", "50%"), 
                          #               plotlyOutput("region_stk_pb_var"), 
                          #               plotlyOutput("tier_stk_pb_var"))
                          # ),
                          fluidRow(
                            plotlyOutput("region_stk_pb_var")
                          ),
                          fluidRow(
                            tags$h1("")
                          ),
                          fluidRow(
                            plotlyOutput("tier_stk_pb_var")
                          ),
                          # fluidRow(
                          #   splitLayout(cellWidths = c("50%", "50%"), 
                          #               plotlyOutput("department_stk_pb_var"))
                          # )
                          fluidRow(
                            tags$h1("")
                          ),
                          fluidRow(
                            plotlyOutput("department_stk_pb_var")
                          )
                        )),
               
               tabPanel("2.3 Attitude to Sporanox and Promotional Activities for Physicians with Score Progression",
                        sidebarPanel(
                          tags$h3("Attitude to Sporanox and Promotional Activities for Physicians with Progression(Q15&Q16)"),
                          selectizeInput("year4", "Year",
                                         choices = NULL,
                                         multiple = FALSE),
                          selectizeInput("quarter4", "Quarter",
                                         choices = NULL,
                                         multiple = FALSE),
                          actionLink("link_to_home_page_2.3", 
                                     "Back To Home Page")
                        ),
                        
                        mainPanel(
                          fluidRow(
                            plotlyOutput("q15_bar_adv")
                          ),
                          fluidRow(
                            tags$h1("")
                          ),
                          fluidRow(
                            plotlyOutput("q16_bar_adv")
                          )
                        ))
               
    ),
    
    navbarMenu("3. Overview of Promotion Activities",
               tabPanel("3.1 Overview of Promotion Activities for Physicians at Different Level",
                        sidebarPanel(
                          tags$h3("Overview of Promotion Activities for Physicians at Different Level"),
                          selectizeInput("year5", "Year",
                                         choices = NULL,
                                         multiple = FALSE),
                          selectizeInput("quarter5", "Quarter",
                                         choices = NULL,
                                         multiple = FALSE),
                          actionLink("link_to_home_page_3.1", 
                                     "Back To Home Page")
                        ),
                        
                        mainPanel(
                          fluidRow(
                            plotlyOutput("meeting_bar_score")
                          ),
                          fluidRow(
                            tags$h1("")
                          ),
                          fluidRow(
                            plotlyOutput("call_bar_score")
                          )
                        )
               ),
               tabPanel("3.2 Tracking of Promotion Activities for Physicians at Different Level",
                        sidebarPanel(
                          tags$h3("Tracking of Promotion Activities for Physicians with Progression and Overall"),
                          selectizeInput("meeting_type", "Imeeting Type",
                                         choices = NULL,
                                         multiple = FALSE),
                          
                          selectizeInput("region1", "Region",
                                         choices = NULL,
                                         multiple = FALSE),
                          
                          actionLink("link_to_home_page_3.2", 
                                     "Back To Home Page")
                          
                        ),
                        
                        mainPanel(
                          fluidRow(
                            plotlyOutput("average_meeting_line")
                          ),
                          fluidRow(
                            tags$h1("")
                          ),
                          fluidRow(
                            plotlyOutput("average_call_line")
                          )
                        )
               )
    ),
    
    navbarMenu("4. Promotion Activities for Physician with Progression",
               tabPanel("4.1 Relationship of Promotion Activities and Physician Perception Progression",
                        sidebarPanel(
                          tags$h3("Relationship of Promotion Activities and Physician Perception Progression"),
                          selectizeInput("year6", "Year",
                                         choices = NULL,
                                         multiple = FALSE),
                          selectizeInput("quarter6", "Quarter",
                                         choices = NULL,
                                         multiple = FALSE),
                          actionLink("link_to_home_page_4.1", 
                                     "Back To Home Page")
                        ),
                        
                        mainPanel(
                          fluidRow(
                            plotlyOutput("meeting_bar_adv")
                          ),
                          fluidRow(
                            tags$h1("")
                          ),
                          fluidRow(
                            plotlyOutput("call_bar_adv")
                          )
                        )
               ),
               tabPanel("4.2 Tracking of Promotion Activities for Physicians with Progression and Overall",
                        sidebarPanel(
                          tags$h3("Tracking of Promotion Activities for Physicians with Progression and Overall"),
                          selectizeInput("meeting_type1", "Imeeting Type",
                                         choices = NULL,
                                         multiple = FALSE),
                          
                          selectizeInput("region2", "Region",
                                         choices = NULL,
                                         multiple = FALSE),
                          actionLink("link_to_home_page_4.2", 
                                     "Back To Home Page")
                        ),
                        
                        mainPanel(
                          fluidRow(
                            plotlyOutput("average_meeting_line_adv")
                          ),
                          fluidRow(
                            tags$h1("")
                          ),
                          fluidRow(
                            plotlyOutput("average_call_line_adv")
                          )
                        )
               )
               
    )
  )
  
)