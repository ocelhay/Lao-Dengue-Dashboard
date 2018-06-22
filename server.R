shinyServer(
  function(input, output, session) {
    
    # Initiate reactive values.
    data_available <- reactiveVal(FALSE)
    dengue_data_dl <- reactiveVal(NULL)
    source_data <- reactiveVal(NULL)
    date_generation <- reactiveVal(NULL)
    
    
    # Load .Rdata from input and update reactive values.
    observeEvent(input$file_RData,{
      
      # escape if there is no data
      if (is.null(input$file_RData)) return(NULL)
      
      # load data
      inFile <- input$file_RData
      file <- inFile$datapath
      load(file, envir = .GlobalEnv)
      
      # update reactive values
      data_available(TRUE)
      dengue_data_dl(dengue_data$dengue)
      source_data(dengue_data$source)
      date_generation(dengue_data$date_generation)
    })
    
    
    # PLH1 / information on the status of data
    output$data_status <- renderText({
      ifelse(data_available(),
             paste0(div(class = "info", icon("info-circle", "fa-2x"), strong("Data provided with:"), tags$ul( 
                        tags$li("source: ", source_data()),
                        tags$li("generated on the: ", date_generation()),
                        tags$li("number of elements: ", nrow(dengue_data_dl())))
                        )),
             paste0(div(class = "alert", icon("exclamation-triangle", "fa-2x"), strong("There is no data to display,"), " please upload a dataset."))
             )
    })
  }
)