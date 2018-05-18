library(DT)

fluidPage(
  includeCSS("./www/styles.css"),
  theme = shinytheme("readable"),
  
  sidebarPanel(width = 3,
               h2("LOMWRU Dengue Dashboard"),
               a(href = "http://www.tropmedres.ac/lomwru-laos", "Learn more about LOMWRU")
  ),
  
  
  mainPanel(width = 9,
            navbarPage(NULL, position = "static-top", id = "tabs", collapsible = TRUE,  windowTitle = "LOMWRU Dengue Dashboard",
                       tabPanel("Dashboard", value = "dashboard",
                                
                                fluidRow(
                                  column(width = 7,
                                         includeMarkdown("./www/disclaimer.md")
                                  ),
                                  column(width = 1, br()),
                                  column(width = 4,
                                         h2(icon("upload", "fa-1x"), "Upload data"),
                                         p("Provide the data to be displayed in this dashboard, for more info, contact", 
                                           tags$a(href = "mailto:olivier.celhay@gmail.com", "Olivier Celhay.")),
                                         fileInput("file_RData", label = NULL, accept = ".RData", buttonLabel = "Browse..."),
                                         p("placeholder—status of data"),
                                         p("placeholder—dimensions of data")
                                  )
                                ),
                                h2("About Dengue"),
                                div(class = "img_text",
                                    img(src = "aedes_aegypti.jpg", alt = "Aedes Aegypti")
                                ),
                                includeMarkdown("./www/about_dengue.md")
                       ),
                       tabPanel("Dengue Virus, Epidemic Trends", value = "dengue_trends",
                                tabsetPanel(
                                  tabPanel("Focus per week",
                                           p("Placeholder for barplot with dengue cases per week"),
                                           p("Placeholder for table with dengue cases per week")
                                  ),
                                  tabPanel("Focus per day",
                                           p("Placeholder for barplot with dengue cases per day")
                                  ),
                                  tabPanel("Focus per month",
                                           p("Placeholder for barplot with dengue cases per month")
                                  ),
                                  tabPanel("Focus on patient origin",
                                           p("Placeholder for barplot with dengue cases per month and per district"),
                                           p("Placeholder for interactive map")
                                  )
                                )
                       ),
                       tabPanel("Patient info summary", value = "dengue_patients",
                                p("Placeholder for info on patient gender"),
                                p("Placeholder for info on patient ages"),
                                p("Placeholder for info on patient province of origin"),
                                p("Placeholder for info on patient district of origin (only Vientiane)")
                                
                       ),
                       tabPanel("Dengue Virus, Test & Results", value = "dengue_virus",
                                tabsetPanel(
                                  tabPanel("ELISA Method", value = "elisa",
                                           p("Placeholder for table on ELISA test results")
                                  ),
                                  tabPanel("RDT Method", value = "rdt",
                                           p("Placeholder for table on RDT test results"),
                                           p("Placeholder for barplot on RDT / NS1 test results"),
                                           p("Placeholder for barplot on RDT / IgM test results")
                                  ),
                                  tabPanel("PCR Method", value = "pcr",
                                           p("Placeholder for table on PCR test results"),
                                           p("Placeholder for barplot on PCR test results")
                                  )
                                )
                       ),
                       tabPanel("About", value = "about", includeMarkdown("./www/about_app.md")
                       )
            )
  )
)