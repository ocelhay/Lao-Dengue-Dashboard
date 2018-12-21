fluidPage(
  theme = shinytheme("spacelab"),
  includeCSS("./www/styles.css"),
  
  sidebarPanel(width = 3,
               fluidRow(
                 div(class = "imgsidebar",
                     img(src = 'aedes_aegypti.jpg')
                 )
               ),
               fluidRow(
                 conditionalPanel(condition = "input.tabs == 'welcome'",
                                  h4(icon("upload"), "Upload Data"),
                                  p("Provide the data to be displayed in this dashboard, for more info, ", 
                                    tags$a(href = "mailto:olivier.celhay@gmail.com", "contact us.")),
                                  fileInput("file_RData", label = NULL, accept = ".RData", buttonLabel = "Browse..."),
                                  htmlOutput("data_status")
                 ),
                 
                 conditionalPanel(condition = "input.tabs == 'dengue_trends' | input.tabs == 'info_patients' | input.tabs == 'dengue_virus'",
                                  div(class = "filterbox", 
                                      h4(icon("filter"), "Filter Dataset"),
                                      dateRangeInput("filter_date", "Filter by collection date"),
                                      checkboxGroupInput("filter_age", "Filter by age categories", choices = c("Under 5 y.o.", "5 to 15 y.o.", "Above 15 y.o.", "Unknown"), 
                                                         selected = c("Under 5 y.o.", "5 to 15 y.o.", "Above 15 y.o.", "Unknown"), inline = TRUE),
                                      checkboxGroupInput("filter_status", "Filter by case status", choices = c("presumptive dengue infection", "confirmed dengue infection", "no evidence of dengue infection"), 
                                                         selected = c("presumptive dengue infection", "confirmed dengue infection", "no evidence of dengue infection"), inline = TRUE)
                                  ),
                                  div(class = 'floatbottom', htmlOutput("data_filter"))
                 )
               )
  ),
  
  mainPanel(width = 9,
            navbarPage(NULL, position = "static-top", id = "tabs", collapsible = TRUE,  windowTitle = "LOMWRU Dengue Dashboard",
                       tabPanel("Welcome", value = "welcome",
                                includeMarkdown("./www/disclaimer.md"),
                                h2("What is Dengue?"),
                                div(class = "imgtext",
                                    img(src = "dengue_fever_symptoms.png", alt = "Aedes Aegypti")
                                ),
                                includeMarkdown("./www/about_dengue.md")
                       ),
                       tabPanel("Epidemic Trends", value = "dengue_trends",
                                h2("Focus per month"),
                                highchartOutput("plot_dengue_month") %>% withSpinner(type = 4), # PLH5
                                br(),
                                h2("Focus per week"),
                                highchartOutput("plot_dengue_week") %>% withSpinner(type = 4), # PLH2
                                br(),
                                fluidRow(
                                  column(width = 8,
                                         dataTableOutput("table_dengue_week") %>% withSpinner(type = 4) # PLH3
                                  )
                                ),
                                br(),
                                h2("Focus per day"),
                                plotOutput("plot_dengue_day", height = "600px") %>% withSpinner(type = 4) # PLH4
                       ),
                       tabPanel("Patients Info", value = "info_patients",
                                # fluidRow(
                                #   column(width = 12, plotOutput("plot_dengue_district")) # PLH6
                                # ),
                                fluidRow(
                                  column(width = 4, 
                                         htmlOutput("text_patients_gender"), # PLH8
                                         br(), br(),
                                         tableOutput("table_patients_age") %>% withSpinner(type = 4) # PLH9
                                  ),
                                  column(width = 8, 
                                         plotOutput("plot_patients_age", height = "300px") %>% withSpinner(type = 4) #PLH9.2
                                  )
                                ),
                                br(), 
                                h2("Origin of patients"),
                                fluidRow(
                                  column(width = 4, 
                                         tableOutput("table_patients_origin_province") %>% withSpinner(type = 4), # PLH10
                                         tableOutput("table_patients_origin_district") %>% withSpinner(type = 4) # PLH11
                                  ),
                                  column(width = 8, 
                                         leafletOutput("map_dengue_district", height = 600) %>% withSpinner(type = 4) # PLH7
                                  ) 
                                )
                       ),
                       tabPanel("Dengue Virus, Test & Results", value = "dengue_virus",
                                tabsetPanel(
                                  tabPanel("ELISA Method", value = "elisa",
                                           fluidRow(column(width = 4,
                                                           tableOutput("table_patients_elisa") %>% withSpinner(type = 4) # PLH12
                                           ),
                                           column(width = 8,
                                                  plotOutput("plot_patients_elisa") %>% withSpinner(type = 4) #PLH12.2
                                           )),
                                           br(),
                                           fluidRow(column(width = 4,
                                                           tableOutput("table_patients_elisa_igm") %>% withSpinner(type = 4) # PLH13
                                           ),
                                           column(width = 8,
                                                  plotOutput("plot_patients_elisa_igm") %>% withSpinner(type = 4) #PLH13.2
                                           ))
                                  ),
                                  tabPanel("RDT Method", value = "rdt",
                                           fluidRow(column(width = 4,
                                                           tableOutput("table_patients_rdt_ns1") %>% withSpinner(type = 4) # PLH14
                                           ),
                                           column(width = 8,
                                                  plotOutput("plot_patients_rdt_ns1") %>% withSpinner(type = 4)  # PLH15
                                           )),
                                           fluidRow(column(width = 4,
                                                           tableOutput("table_patients_rdt_igm") %>% withSpinner(type = 4) # PLH15bis
                                           ),
                                           column(width = 8,
                                                  plotOutput("plot_patients_rdt_igm") %>% withSpinner(type = 4)  # PLH16 
                                           ))
                                  ),
                                  tabPanel("PCR Method", value = "pcr",
                                           fluidRow(column(width = 4,
                                                           tableOutput("table_patients_pcr_res") %>% withSpinner(type = 4) # PLH17
                                           ),
                                           column(width = 8,
                                                  plotOutput("plot_patients_pcr_res") %>% withSpinner(type = 4) # PLH18
                                           )),
                                           fluidRow(column(width = 4,
                                                           tableOutput("table_patients_pcr") %>% withSpinner(type = 4) # PLH17
                                           ),
                                           column(width = 8,
                                                  plotOutput("plot_patients_pcr") %>% withSpinner(type = 4) # PLH18
                                           ))
                                           
                                           
                                           
                                  )
                                )
                       ),
                       tabPanel("About", value = "about",
                                fluidRow(
                                  p("Dashboard in development — for more info, ", 
                                    tags$a(href = "mailto:olivier.celhay@gmail.com", "contact us."))
                                  # div(class = "img_text",
                                  #     a(href = "http://www.tropmedres.ac/lomwru-laos", img(src = "moru-logo.png", alt = "MORU"))
                                  # )
                                )
                                # includeMarkdown("./www/about_app.md")
                       )
            )
  )
)