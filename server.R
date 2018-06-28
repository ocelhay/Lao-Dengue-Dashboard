shinyServer(
  function(input, output, session) {
    
    # Initiate reactive values.
    data_available <- reactiveVal(FALSE)
    dengue_data_dl <- reactiveVal(NULL)
    source_data <- reactiveVal(NULL)
    date_generation <- reactiveVal(NULL)
    
    # Hide several tabs at the launch of the app
    observeEvent(NULL, {
      hideTab(inputId = "tabs", target = "dengue_trends")
      hideTab(inputId = "tabs", target = "dengue_patients")
      hideTab(inputId = "tabs", target = "dengue_virus")
      }, ignoreNULL = FALSE)
    
    # Load .Rdata from input and update reactive values.
    observeEvent(input$file_RData,{
      
      # Escape if there is no data, otherwise load data
      if (is.null(input$file_RData)) return(NULL)
      inFile <- input$file_RData
      file <- inFile$datapath
      load(file, envir = .GlobalEnv)
      
      # Update reactive values
      data_available(TRUE)
      dengue_data_dl(dengue_data$dengue)
      source_data(dengue_data$source)
      date_generation(dengue_data$date_generation)
      
      # Show Tabs
      showTab(inputId = "tabs", target = "dengue_trends")
      showTab(inputId = "tabs", target = "dengue_patients")
      showTab(inputId = "tabs", target = "dengue_virus")
      
      # Update elements of the UI
      min_collection_date <- min(dengue_data_dl()$collection_date)
      max_collection_date <- max(dengue_data_dl()$collection_date)
      updateDateRangeInput(session = session, inputId = "filter_date", start = min_collection_date, end = max_collection_date)
    })
    
    # Filter the dataset based on UI
    dengue_data_filt <- reactive({
      dengue_data_dl() %>% filter(
        collection_date >= input$filter_date[1],
        collection_date <= input$filter_date[2],
        age_category %in% input$filter_age,
        dengue_virus %in% input$filter_status
      )
    })
    
    
    # PLH1: information on the status of data
    output$data_status <- renderText({
      ifelse(data_available(),
             paste0(div(class = "info", icon("info-circle", "fa-2x"), strong("Data uploaded"), tags$ul( 
               tags$li("source: ", source_data()),
               tags$li("generated on the: ", date_generation()),
               tags$li("number of elements: ", nrow(dengue_data_dl())))
             )),
             paste0(div(class = "alert", icon("exclamation-triangle", "fa-2x"), strong("There is no data to display,"), " please upload a dataset."))
      )
    })
    
    # PLH1bis: information on the number of elements filtered
    output$data_filter <- renderText({
      req(data_available())
      
      paste0("Original dataset contains ", nrow(dengue_data_dl()), " elements", br(), 
             "Filtered dataset contains ", nrow(dengue_data_filt()), " elements")
    })
    
    
    # PLH2: plot dengue cases per week
    output$plot_dengue_week <- renderPlot({
      req(data_available())
      
      ggplot(data = dengue_data_filt(), aes(x = collection_week, fill = dengue_virus)) +
        geom_bar(stat = "count", width = 0.5) +
        labs(x = "Week number", y = "Cases", title = "Dengue Tests Results", subtitle = " per week") +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 16) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        guides(fill = guide_legend(nrow = 1)
        )
    })
    
    # PLH3: table of dengue cases per week
    output$table_dengue_week <- renderDT({
      req(data_available())
      
      datatable(dengue_data_filt() %>% 
                  group_by(collection_year, collection_week, collection_week_str, dengue_virus) %>% 
                  summarise(cases = n()) %>%
                  rename(Year = collection_year, `Week Nb` = collection_week, Week = collection_week_str, Status = dengue_virus),
                rownames = FALSE)
    })
    
    # PLH4: plot dengue cases per day
    output$plot_dengue_day <- renderPlot({
      req(data_available())
      
      ggplot(data = dengue_data_filt(), aes(x = collection_day, fill = dengue_virus)) +
        geom_bar(stat = "count", width = 0.8) +
        labs(x = NULL, y = NULL, title = "Dengue Tests Results", subtitle = "per day of the month") +
        facet_wrap(~ collection_year + collection_month, scales = "fixed", labeller = label_value) +
        theme_minimal(base_size = 14) +
        theme(panel.spacing = unit(2, "lines"), axis.text.x = element_text(size = 9))
    })
    
    # PLH5: plot dengue cases per month
    output$plot_dengue_month <- renderPlot({
      req(data_available())
      
      ggplot(data =  dengue_data_filt() %>% 
               mutate(month = month(collection_date, label = TRUE)) %>% 
               group_by(collection_year, month, dengue_virus) %>% 
               summarise(total = n()), 
             aes(x = month, y = total, fill = dengue_virus)) +
        geom_col() +
        labs(x = NULL, y = NULL, title = "Dengue Tests Results", subtitle = "per month") +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 16)
    })
    
    # PLH6:  plot dengue cases per district
    output$plot_dengue_district <- renderPlot({
      req(data_available())
      
      ggplot(data = dengue_data_filt() %>% 
               filter(patient_province == "Vientiane Capital"), 
             aes(x = collection_month, fill = dengue_virus)) +
        geom_bar(stat = "count", width = 0.5) +
        labs(x = NULL, y = "Cases", title = "Dengue Cases in Vientiane Capital", subtitle = "per month and per district") +
        facet_wrap(~ collection_year + patient_district, scales = "fixed") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle=45, hjust=1, size = 9))
    })
    
    # PLH7: map dengue cases per district
    output$map_dengue_district <- renderLeaflet({
      req(data_available())
      
      leaflet(dengue_data_filt()  %>% 
                filter(patient_province == "Vientiane Capital")) %>% 
        setView(lng = 102.691992723991, lat = 17.9787350677193, zoom = 10) %>%
        addTiles() %>% 
        addMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions())
    })
    
    # PLH8: info on patients gender
    output$text_patients_gender <- renderText({
      req(data_available())
      
      paste0("There are ", dengue_data_filt() %>% filter(patient_gender == "M") %>% nrow(), " males patients and ", 
             dengue_data_filt() %>% filter(patient_gender == "F") %>% nrow(), " female patients.")
    })
    
    # PLH9: info on patients age categories
    output$table_patients_age <- renderTable({
      req(data_available())
      
      table(dengue_data_filt()$age_category)
    })
    
    # PLH9.2: info on patients age categories
    output$plot_patients_age <- renderPlot({
      req(data_available())
      
      ggplot(dengue_data_filt(), aes(patient_age)) +
        geom_histogram(binwidth = 1) +
        geom_vline(xintercept = median(dengue_data_filt()$patient_age))
    })
    
    # PLH10: info on patients province of origin
    output$table_patients_origin_province <- renderTable({
      req(data_available())
      
      table(dengue_data_filt()$patient_province)
    })
    
    # PLH11: info on patients distric of origin (Vientiane only)
    output$table_patients_origin_district <- renderTable({
      req(data_available())
      
      table((dengue_data_filt() %>% filter(patient_province == "Vientiane Capital"))$patient_district)
    })
    
    # PLH12: table of patients, ELISA NS1
    output$table_patients_elisa <- renderTable({
      req(data_available())
      
      table(dengue_data_filt()$elisa_ns1)
    })
    
    # PLH12.2: plot of patients, ELISA NS1
    output$plot_patients_elisa <- renderPlot({
      req(data_available())
      
      ggplot(data = dengue_data_filt(), aes(x = collection_month, fill = elisa_ns1)) +
        geom_bar(stat = "count", width = 0.5, position = "dodge") +
        labs(x = NULL, y = "Tests", title = "ELISA Tests, NS1", subtitle = " per month and result") +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        guides(fill = guide_legend(nrow = 1))
    })
    
    # PLH 13: table of patients, ELISA IgM
    output$table_patients_elisa_igm <- renderTable({
      req(data_available())
      
      table((dengue_data_filt()$elisa_igm))
    })
    
    # PLH13.2: plot of patients, ELISA IgM
    output$plot_patients_elisa_igm <- renderPlot({
      req(data_available())
      
      ggplot(data = dengue_data_filt(), aes(x = collection_month, fill = elisa_igm)) +
        geom_bar(stat = "count", width = 0.5, position = "dodge") +
        labs(x = NULL, y = "Tests", title = "ELISA Tests, IgM", subtitle = " per month and result") +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        guides(fill = guide_legend(nrow = 1))
    })
    
    # PLH14: table of patients, RDT NS1
    output$table_patients_rdt_ns1 <- renderTable({
      req(data_available())
      
      table((dengue_data_filt()$rdt_ns1))
    })
    
    # PLH 15: plot of patients, RDT NS1
    output$plot_patients_rdt_ns1 <- renderPlot({
      req(data_available())
      
      ggplot(data = dengue_data_filt(), aes(x = collection_month, fill = rdt_ns1)) +
        geom_bar(stat = "count", width = 0.5, position = "dodge") +
        labs(x = NULL, y = "Tests", title = "RDT Tests, NS1", subtitle = " per month and result") +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        guides(fill = guide_legend(nrow = 1))
    })
    
    # PLH14: table of patients, RDT IgM
    output$table_patients_rdt_ns1 <- renderTable({
      req(data_available())
      
      table((dengue_data_filt()$rdt_ns1))
    })
    
    # PLH16: plot of patient, RDT IgM
    output$plot_patients_rdt_igm <- renderPlot({
      req(data_available())
      
      ggplot(data = dengue_data_filt(), aes(x = collection_month, fill = rdt_igm)) +
        geom_bar(stat = "count", width = 0.5, position = "dodge") +
        labs(x = NULL, y = "Tests", title = "RDT Tests, IgM", subtitle = " per month and result") +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        guides(fill = guide_legend(nrow = 1))
    })
    
    # PLH17: table of patients, PCR
    output$table_patients_pcr <- renderTable({
      req(data_available())
      
      table((dengue_data_filt()$pcr_serotype))
    })
    
    # PLH 18, plot of patients, PCR serotype
    output$plot_patients_pcr <- renderPlot({
      req(data_available())

      ggplot(data = dengue_data_filt() %>% 
               count(collection_year, collection_month, pcr_serotype) %>% 
               complete(collection_year, collection_month, pcr_serotype), 
             aes(x = collection_month, weight = n, fill = pcr_serotype)) +
        geom_bar(width = 0.8,  position = "dodge") +
        labs(x = NULL, y = "Tests", title = "PCR", subtitle = " per month") +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 16) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        guides(fill = guide_legend(nrow = 1))
      
    })
  }
)