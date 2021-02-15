#- ------------------------------------------
#- PARENT PACKAGE: shiny-external_data_protal 
#- MODULE NAME: showBoarIndexes
#- DESCRIPTION: ui and server to display boar table on the Indexes tab
#- AUTHOR: lg
#- DATE: 2021-01-04
#- ------------------------------------------

showBoarIndexesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2, uiOutput(ns('uiChooseVendor')),
             checkboxInput(ns('chkActive'), label="Show Only Active Boars")),
      column(2, uiOutput(ns('uiChooseFarm'))),
      column(2, uiOutput(ns('uiChooseBreed')))
    ),
    hr(),
    fluidRow(
      column(9,
             DT::dataTableOutput(ns("table"))
      )
    ),
    fluidRow(
      column(2, dnldExcelUI(ns('indexes')))
    ),
    hr(),
    fluidRow(
      column(3,
             sliderInput(ns('sldBinWidth'), label="Bin Width", min=2, max=10, value=5),
             showSimpleStatsUI(ns('indexStats'))
      ),
      column(8,
             plotOutput(ns('idxHist'))
      )
    )
  )
}



showBoarIndexesServer <- function(id, getIndexData) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      #- ==== DYNAMIC UI ====
      output$uiChooseVendor <- renderUI({
        ns <- session$ns
        vendorChoices = c('All', unique(getIndexData()$VendorCode))
        selectInput(ns('chooseVendor'), label = "Vendor",
                    choices = vendorChoices,
                    selected = 'All')
      })
      output$uiChooseFarm <- renderUI({
        ns <- session$ns
        farmChoices = c('All', unique(getIndexData()$SPGStudName))
        selectInput(ns('chooseFarm'), label = 'Farm',
                    choices = farmChoices,
                    selected = 'All')
      })
      
      output$uiChooseBreed <- renderUI({
        ns <- session$ns
        breedChoices = c('All', unique(getIndexData()$Breed))
        selectInput(ns('chooseBreed'), label = 'Breed',
                    choices = breedChoices,
                    selected = 'All')
      })
      
      #- ==== REACTIVE DATA SETS
      filterIndexes <- reactive({
        req(input$chooseBreed)
        req(input$chooseFarm)
        
        filtered <- getIndexData()
    
        if ( input$chooseVendor != 'All' ) 
          filtered <- filtered %>% filter(VendorCode == input$chooseVendor)

        if ( input$chooseBreed != 'All' ) 
          filtered <- filtered %>% filter(Breed == input$chooseBreed)
        
        if ( input$chooseFarm != 'All' )
          filtered <- filtered %>% filter(SPGStudName == input$chooseFarm) 
        
        if ( input$chkActive )
          filtered <- filtered %>% filter(is.na(RemovalDate))
        
        filtered
      })
      
      #- ==== OUTPUTS
      output$table <- DT::renderDataTable({
        prettyData <- filterIndexes() %>% mutate(BirthDate = format(BirthDate, format="%Y-%m-%d"),
                                                 RemovalDate = format(RemovalDate, format="%Y-%m-%d"),
                                                 IndexDate = format(IndexDate, format="%Y-%m-%d"),
                                                 Active = ifelse(!is.na(RemovalDate), "NO", "Active")
                                                 ) %>%
          select(SPG_ID, VendorCode, VisualID, Breed, BirthDate, SPGStudName, Active, IndexDate, idx)
        
        DT::datatable(prettyData) %>% formatStyle(
          'Active',
          target = 'row',
          color = styleEqual('NO', 'lightgray')
        )
        
      })
      
      filename <- reactive(paste(input$chooseFarm,'_boar_indexes', Sys.Date(), '.xlsx'))
      dnldExcelServer('indexes', reactive(filterIndexes()), reactive(filename()))
      
      output$idxHist <- renderPlot({
        #browser()
        req(filterIndexes())
        req(input$sldBinWidth)
        
        #- we only need indexes so we'll make a df with one column
        justIdx <- filterIndexes() %>% select(idx)
        
        ggplot(justIdx, aes(idx, fill = cut(idx, 100))) +
          geom_histogram(binwidth = input$sldBinWidth,
                         show.legend = FALSE) +
          scale_fill_discrete(h = c(80, 1), c = 120, l=70) +
          theme_minimal() +
          labs(x="Index", y = "Count") +
          ggtitle("Histogram of Selected Indexes")
      })
      
      showShowSimpleStatsServer('indexStats', reactive(filterIndexes()[,c('idx')]))
      
    
      
    }
  )
}
