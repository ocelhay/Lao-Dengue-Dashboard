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
      hideTab(inputId = "tabs", target = "info_patients")
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
      showTab(inputId = "tabs", target = "info_patients")
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
             paste0(div(class = "infobox", 
                        h4(icon("upload"), nrow(dengue_data_dl()), " Patients"), 
                        tags$ul( 
                          tags$li("Patients admitted at Mahosot hospital with suspicion of dengue infection, according to WHO criteria (2009)."),
                          tags$li("Source: ", source_data()),
                          tags$li("Generated on the: ", date_generation())
                        ))
             ),
             paste0(div(class = "alert", 
                        icon("exclamation-triangle", "fa-2x"), strong("There is no data to display,"), " please upload a dataset.")
             )
      )
    })
    
    # PLH1bis: information on the number of elements filtered
    output$data_filter <- renderText({
      req(data_available())
      
      ifelse(nrow(dengue_data_filt()) == nrow(dengue_data_dl()),
             
             paste0(div(class = "infobox", 
                        h4(icon("filter"), paste0("All ", nrow(dengue_data_filt()), " patients in the provided dataset are selected"))
             )),
             
             paste0(div(class = "alert", 
                        h4(icon("filter"), nrow(dengue_data_filt()), " Patients selected"), 
                        tags$ul( 
                          tags$li(paste0("original dataset contains ", nrow(dengue_data_dl()), " patients admitted at Mahosot hospital with suspicion of dengue infection, according to WHO criteria (2009).")),
                          tags$li(paste0(nrow(dengue_data_dl()) - nrow(dengue_data_filt()), " patients where filtered out."))
                        )
             )
             )
      )
    })
    
    
    # PLH2: plot dengue cases per week
    output$plot_dengue_week <- renderHighchart({
      req(dengue_data_filt())
      
      dengue_data_filt() %>%
        group_by(collection_week, collection_week_str, dengue_virus) %>% 
        count() %>%
        ungroup() %>%
        complete(dengue_virus, nesting(collection_week, collection_week_str), fill = list(n = 0)) %>%
        hchart("column", hcaes(x = 'collection_week', y = 'n', group = 'dengue_virus', label = 'dengue_virus', week = 'collection_week_str')) %>%
        hc_plotOptions(column = list(stacking = "normal", dataLabels = list())) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_colors(cols) %>%
        hc_title(text = "Patients per week") %>%
        hc_xAxis(title = list(text = "Week of collection")) %>%
        hc_yAxis(title = list(text = "Number of Patients")) %>%
        hc_tooltip(useHTML = TRUE, borderWidth = 4,
                   headerFormat = "",
                   pointFormat = "Week: {point.week} <br> 
                   {point.label}: {point.y}"
        )
    })
    
    # PLH3: table of dengue cases per week
    output$table_dengue_week <- renderDT({
      req(dengue_data_filt())
      
      datatable(dengue_data_filt() %>% 
                  group_by(collection_year, collection_week, collection_week_str) %>% 
                  summarise(`Total Patients` = n()) %>%
                  rename(Year = collection_year, `Week of collection` = collection_week, ' ' = collection_week_str),
                rownames = FALSE)
    })
    
    
    # PLH5: plot dengue cases per month
    output$plot_dengue_month <- renderHighchart({
      req(data_available())
      req(dengue_data_filt())
      
      dengue_data_filt() %>%
        mutate(month = month(collection_date, label = FALSE)) %>%
        mutate(month = ifelse(month < 10, paste0("0", month), as.character(month))) %>%
        mutate(year_month = paste(collection_year, month, sep = "-")) %>%
        group_by(year_month, dengue_virus) %>%
        count() %>%
        ungroup() %>%
        arrange(year_month) %>%
        mutate(year_month = as.factor(year_month)) %>%
        complete(dengue_virus, nesting(year_month), fill = list(n = 0)) %>%
        hchart("column", hcaes(x = 'year_month', y = 'n', group = 'dengue_virus',
                               label = 'dengue_virus', label2 = 'year_month')) %>%
        hc_plotOptions(column = list(stacking = "normal", dataLabels = list())) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_colors(cols) %>%
        hc_title(text = "Patients per month") %>%
        hc_xAxis(title = list(text = "Month of collection")) %>%
        hc_yAxis(title = list(text = "Number of Patients")) %>%
        hc_tooltip(useHTML = TRUE, borderWidth = 4,
                   headerFormat = "",
                   pointFormat = "Month: {point.year_month} <br> 
                   {point.label}: {point.y}")
    })
    
    # PLH7: map dengue cases per district
    output$map_dengue_district <- renderLeaflet({
      req(data_available())
      
      leaflet(dengue_data_filt()) %>% 
        setView(lng = 102.691992723991, lat = 17.9787350677193, zoom = 7) %>%
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
      
      table(dengue_data_filt()$age_category) %>%
        as.data.frame() %>%
        rename(`Age Category` = Var1, Patients = Freq)
    })
    
    # PLH9.2: info on patients age categories
    output$plot_patients_age <- renderPlot({
      req(data_available())
      
      ggplot(dengue_data_filt(), aes(patient_age)) +
        geom_histogram(binwidth = 1) +
        geom_vline(xintercept = median(dengue_data_filt()$patient_age)) +
        geom_label(x = median(dengue_data_filt()$patient_age), y = 0, label = "Median Age") +
        labs(x = "Age (y.o.)", y = "Patients") +
        theme_minimal(base_size = 14)
    })
    
    # PLH10: info on patients province of origin
    output$table_patients_origin_province <- renderTable({
      req(data_available())
      
      table(dengue_data_filt()$patient_province) %>%
        as.data.frame() %>%
        rename(`Province` = Var1, Patients = Freq)
    })
    
    # PLH11: info on patients distric of origin (Vientiane only)
    output$table_patients_origin_district <- renderTable({
      req(data_available())
      
      table((dengue_data_filt() %>% filter(patient_province == "Vientiane Capital"))$patient_district) %>%
        as.data.frame() %>%
        rename(`District of Vientiane` = Var1, Patients = Freq)
    })
    
    # PLH12: table of patients, ELISA NS1
    output$table_patients_elisa <- renderTable({
      req(data_available())
      
      table(dengue_data_filt()$elisa_ns1) %>%
        as.data.frame() %>%
        rename(`ELISA NS1 result` = Var1, Patients = Freq)
    })
    
    # PLH12.2: plot of patients, ELISA NS1
    output$plot_patients_elisa <- renderPlot({
      req(data_available())
      
      dengue_data_filt() %>%
        group_by(collection_year, collection_month, elisa_ns1) %>% 
        count() %>%
        ungroup() %>%
        complete(nesting(collection_year, collection_month, elisa_ns1), fill = list(n = 0)) %>%
        ggplot(aes(x = collection_month, y = n, fill = elisa_ns1)) +
        geom_col() +
        labs(x = NULL, y = "Nb. of Tests", title = "ELISA, NS1") +
        scale_fill_manual(values = cols_elisa_ns1) +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill = guide_legend(nrow = 2))
    })
    
    # PLH 13: table of patients, ELISA IgM
    output$table_patients_elisa_igm <- renderTable({
      req(data_available())
      
      table((dengue_data_filt()$elisa_igm)) %>%
        as.data.frame() %>%
        rename(`ELISA IgM result` = Var1, Patients = Freq)
    })
    
    # PLH13.2: plot of patients, ELISA IgM
    output$plot_patients_elisa_igm <- renderPlot({
      req(data_available())
      
        dengue_data_filt() %>%
        group_by(collection_year, collection_month, elisa_igm) %>% 
        count() %>%
        ungroup() %>%
        complete(nesting(collection_year, collection_month, elisa_igm), fill = list(n = 0)) %>%
        ggplot(aes(x = collection_month, y = n, fill = elisa_igm)) +
        geom_col() +
        labs(x = NULL, y = "Nb. of Tests", title = "ELISA, IgM") +
        scale_fill_manual(values = cols_elisa_igm) +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill = guide_legend(nrow = 2))
    })
    
    # PLH14: table of patients, RDT NS1
    output$table_patients_rdt_ns1 <- renderTable({
      req(data_available())
      
      table((dengue_data_filt()$rdt_ns1)) %>%
        as.data.frame() %>%
        rename(`RDT NS1 result` = Var1, Patients = Freq)
    })
    
    # PLH 15: plot of patients, RDT NS1
    output$plot_patients_rdt_ns1 <- renderPlot({
      req(data_available())
      
      dengue_data_filt() %>%
        group_by(collection_year, collection_month, rdt_ns1) %>% 
        count() %>%
        ungroup() %>%
        complete(nesting(collection_year, collection_month, rdt_ns1), fill = list(n = 0)) %>%
        ggplot(aes(x = collection_month, y = n, fill = rdt_ns1)) +
        geom_col() +
        labs(x = NULL, y = "Nb. of Tests", title = "RDT, NS1") +
        scale_fill_manual(values = cols_rdt) +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill = guide_legend(nrow = 2))
    })
    
    # PLH16: plot of patient, RDT IgM
    output$plot_patients_rdt_igm <- renderPlot({
      req(data_available())
      
      dengue_data_filt() %>%
        group_by(collection_year, collection_month, rdt_igm) %>% 
        count() %>%
        ungroup() %>%
        complete(nesting(collection_year, collection_month, rdt_igm), fill = list(n = 0)) %>%
        ggplot(aes(x = collection_month, y = n, fill = rdt_igm)) +
        geom_col() +
        labs(x = NULL, y = "Nb. of Tests", title = "RDT, IgM") +
        scale_fill_manual(values = cols_rdt) +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill = guide_legend(nrow = 2))
    })
    
    # PLH16-2: table of patients, RDT IgM
    output$table_patients_rdt_igm <- renderTable({
      req(data_available())
      
      table((dengue_data_filt()$rdt_igm)) %>%
        as.data.frame() %>%
        rename(`RDT IgM result` = Var1, Patients = Freq)
    })
    
    
    
    # PLH16-3: plot of patient, RDT IgG
    output$plot_patients_rdt_igg <- renderPlot({
      req(data_available())
      
      dengue_data_filt() %>%
        group_by(collection_year, collection_month, rdt_igg) %>% 
        count() %>%
        ungroup() %>%
        complete(nesting(collection_year, collection_month, rdt_igg), fill = list(n = 0)) %>%
        ggplot(aes(x = collection_month, y = n, fill = rdt_igg)) +
        geom_col() +
        labs(x = NULL, y = "Nb. of Tests", title = "RDT, IgG") +
        scale_fill_manual(values = cols_rdt_igg) +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill = guide_legend(nrow = 2))
    })
    
    # PLH16-4: table of patients, RDT IgG
    output$table_patients_rdt_igg <- renderTable({
      req(data_available())
      
      table((dengue_data_filt()$rdt_igg)) %>%
        as.data.frame() %>%
        rename(`RDT IgG result` = Var1, Patients = Freq)
    })
    
    
    # PLH16-5: table of patients, PCR result
    output$table_patients_pcr_res <- renderTable({
      req(data_available())
      
      table((dengue_data_filt()$pcr_result))%>%
        as.data.frame() %>%
        rename(`PCR Result` = Var1, Patients = Freq)
    })
    
    # PLH 16-6, plot of patients, PCR results
    output$plot_patients_pcr_res <- renderPlot({
      req(data_available())
      
      dengue_data_filt() %>%
        group_by(collection_year, collection_month, pcr_result) %>% 
        count() %>%
        ungroup() %>%
        complete(nesting(collection_year, collection_month, pcr_result), fill = list(n = 0)) %>%
        ggplot(aes(x = collection_month, y = n, fill = pcr_result)) +
        geom_col() +
        labs(x = NULL, y = "Nb. of Tests", title = "PCR Results") +
        scale_fill_manual(values = cols_pcr_result) +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill = guide_legend(nrow = 2))
      
    })
    
    
    # PLH17: table of patients, PCR serotype
    output$table_patients_pcr <- renderTable({
      req(data_available())
      
      table((dengue_data_filt()$pcr_serotype))%>%
        as.data.frame() %>%
        rename(`PCR Serotype` = Var1, Patients = Freq)
    })
    
    # PLH 18, plot of patients, PCR serotype
    output$plot_patients_pcr <- renderPlot({
      req(data_available())
      
      dengue_data_filt() %>%
        filter(!is.na(pcr_serotype)) %>%
        group_by(collection_year, collection_month, pcr_serotype) %>% 
        count() %>%
        ungroup() %>%
        complete(nesting(collection_year, collection_month, pcr_serotype), fill = list(n = 0)) %>%
        ggplot(aes(x = collection_month, y = n, fill = pcr_serotype)) +
        geom_col() +
        labs(x = NULL, y = "Nb. of Tests", title = "PCR Serotype") +
        scale_fill_manual(values = cols_pcr_serotype) +
        facet_wrap(~ collection_year, scales = "free_x") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill = guide_legend(nrow = 2))
      
    })
  }
)