library(DT)

fluidPage(
  includeCSS("./www/styles.css"),
  
  theme = shinytheme("readable"),
  
  sidebarPanel(width = 3,
               fluidRow(
                 conditionalPanel(condition = "input.tabs == 'dashboard'",
                                  br(), br(), br(), br(),
                                  h4("Upload Data"),
                                  p("Provide the data to be displayed in this dashboard, for more info, ", 
                                    tags$a(href = "mailto:olivier.celhay@gmail.com", "contact us.")),
                                  fileInput("file_RData", label = NULL, accept = ".RData", buttonLabel = "Browse..."),
                                  
                                  # PLH1
                                  div(class = "info-box", 
                                      htmlOutput("data_status")),
                                  hr()
                 ),
                 conditionalPanel(condition = "input.tabs == 'dengue_trends' | input.tabs == 'dengue_patients' | input.tabs == 'dengue_virus'",
                                  br(), br(), br(), br(),
                                  h4("Filter Data"),
                                  dateRangeInput("filter_date", "Filter by collection date"),
                                  checkboxGroupInput("filter_age", "Filter by age categories", choices = c("Under 5 y.o.", "5 to 15 y.o.", "Above 15 y.o.", "Unknown"), 
                                                     selected = c("Under 5 y.o.", "5 to 15 y.o.", "Above 15 y.o.", "Unknown"), inline = TRUE),
                                  checkboxGroupInput("filter_status", "Filter by case status", choices = c("Presumptive", "Confirmed", "No evidence of dengue virus"), 
                                                     selected = c("Presumptive", "Confirmed", "No evidence of dengue virus"), inline = TRUE),
                                  # PLH1bis
                                  htmlOutput("data_filter"),
                                  hr()
                 )
               )
  ),
  
  mainPanel(width = 9,
            navbarPage(NULL, position = "static-top", id = "tabs", collapsible = TRUE,  windowTitle = "LOMWRU Dengue Dashboard",
                       tabPanel("Dashboard", value = "dashboard",
                                includeMarkdown("./www/disclaimer.md"),
                                h2("What is Dengue?"),
                                div(class = "img_text",
                                    img(src = "aedes_aegypti.jpg", alt = "Aedes Aegypti")
                                ),
                                includeMarkdown("./www/about_dengue.md")
                       ),
                       tabPanel("Epidemic Trends", value = "dengue_trends",
                                tabsetPanel(
                                  tabPanel("Focus per week",
                                           plotOutput("plot_dengue_week"), # PLH2
                                           dataTableOutput("table_dengue_week") # PLH3
                                  ),
                                  tabPanel("Focus per day",
                                           plotOutput("plot_dengue_day", height = "600px") # PLH4
                                  ),
                                  tabPanel("Focus per month",
                                           plotOutput("plot_dengue_month") # PLH5
                                  ),
                                  tabPanel("Focus on patient origin",
                                           fluidRow(
                                             column(width = 6, plotOutput("plot_dengue_district")), # PLH6
                                             column(width = 6, leafletOutput("map_dengue_district")) # PLH7
                                           )
                                  )
                                )
                       ),
                       tabPanel("Patient info summary", value = "dengue_patients",
                                htmlOutput("text_patients_gender"), # PLH8
                                tableOutput("table_patients_age"), # PLH9
                                plotOutput("plot_patients_age"), #PLH9.2
                                tableOutput("table_patients_origin_province"), # PLH10
                                tableOutput("table_patients_origin_district") # PLH11
                                
                       ),
                       tabPanel("Dengue Virus, Test & Results", value = "dengue_virus",
                                tabsetPanel(
                                  tabPanel("ELISA Method", value = "elisa",
                                           tableOutput("table_patients_elisa"), # PLH12
                                           plotOutput("plot_patients_elisa"), #PLH12.2
                                           tableOutput("table_patients_elisa_igm"), # PLH13
                                           plotOutput("plot_patients_elisa_igm") #PLH13.2
                                           
                                  ),
                                  tabPanel("RDT Method", value = "rdt",
                                           tableOutput("table_patients_rdt_ns1"), # PLH14
                                           plotOutput("plot_patients_rdt_ns1"),  # PLH15
                                           tableOutput("table_patients_rdt_igm"), # PLH15bis
                                           plotOutput("plot_patients_rdt_igm")  # PLH16 
                                  ),
                                  tabPanel("PCR Method", value = "pcr",
                                           tableOutput("table_patients_pcr"), # PLH17
                                           plotOutput("plot_patients_pcr") # PLH18
                                  )
                                )
                       ),
                       tabPanel("About", value = "about", icon = icon("info-circle", "fa-1x"),
                                fluidRow(
                                  div(class = "img_text",
                                      a(href = "http://www.tropmedres.ac/lomwru-laos", img(src = "moru-logo.png", alt = "MORU"))
                                  )
                                ),
                                includeMarkdown("./www/about_app.md")
                       )
            )
  )
)