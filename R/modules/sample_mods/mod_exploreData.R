#- ------------------------------------------
#- PARENT PACKAGE: Race Analysis Project
#- MODULE NAME: mod_exploreData
#- DESCRIPTION: ui and server to display athlete data 
#- AUTHOR: lg
#- DATE: 2021-01-04
#- ------------------------------------------

exploreData_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(1),
      column(10,
             fluidRow(
               uiOutput(ns('chkYears')),
               br(),
               column(2, checkboxGroupInput(ns('genders'), "Select Genders",
                                            choices = c("Male" = "M", "Female" = "F"),
                                            selected = 'M',
                                            inline = FALSE)),
               column(3, uiOutput(ns('chkSports'))),
               column(3, uiOutput(ns('chkDistances')))
             ),
             fluidRow(
               column(3, uiOutput(ns('selEvents'))),
               column(3, uiOutput(ns("selCategories"))),
               column(5, uiOutput(ns('selAgeRange')))
             ),
             hr(),
             fluidRow(
               tabsetPanel(
                 tabPanel('Data Table',
                          column(8,DT::dataTableOutput(ns("dataSample")))
                 ),
                 tabPanel('Factors',
                          summaryTab_ui(ns('sumTab'))
                 )
               )
             )
      )
    )
  )
}

exploreData_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #- ==== DYNAMIC UI ====
      output$chkYears <- renderUI({
        ns <- session$ns
        checkboxGroupInput(ns('years'), 'Choose Year(s)', 
                           choices = yearList,
                           selected = max(yearList),
                           inline = TRUE
        )
      })
      output$chkSports <- renderUI({
        ns <- session$ns
        checkboxGroupInput(ns("sports"), "Select Sport(s)",
                           choices = sportList,
                           selected = 'overall',
                           inline = TRUE)
      })
      output$chkDistances <- renderUI({
        ns <- session$ns
        checkboxGroupInput(ns("distances"), "Select Distance(s)",
                           choices = getDistanceList(),
                           selected = getDistanceList()[1],
                           inline = TRUE)
      })
      output$selEvents <- renderUI({
        ns <- session$ns        
        selectInput(ns("events"), "Select Events",
                    choices = getEventList(), 
                    multiple = FALSE)
      })
      output$selCategories <- renderUI({
        ns <- session$ns        
        selectInput(ns("categories"), "Select Categories",
                    choices = getCategoryList(),
                    multiple = FALSE)
      })
      output$selAgeRange <- renderUI({
        ns <- session$ns
        sliderInput(ns("ages"), "Select Age Range",
                    min = minAge,
                    max = maxAge,
                    value = initAgeRange)
      })
      
      #- ==== REACTIVE ELEMENTS
      getYearData <- reactive({
        req(input$years)
        filter(races, ryear %in% input$years)        
      })
      
      getDistanceList <- reactive({
        req(input$years)
        sort(unique(getYearData()$dist))
        
      })
      
      getEventList <- reactive({
        req(input$years)
        c('All Events', sort(unique(getYearData()$event)))
      })
      
      getCategoryList <- reactive({
        req(input$years)
        c('All Categories', sort(levels(getYearData()$rcat)))
      })
      
      filterData <- reactive({
        # req(input$ages)
        # browser()
        req(input$events)
        req(input$categories)
        getYearData() %>%
          filter(
            sex %in% input$genders,
            dist %in% input$distances,
            sport %in% input$sports,
            input$events == 'All Events' | event %in% input$events,
            input$categories == 'All Categories' | rcat %in% input$categories,
            between(age, input$ages[1], input$ages[2])
          )
      })
      
      #- ==== OUTPUT ELEMENTS
      output$dataSample <- DT::renderDataTable({
        req(filterData())
        filterData() %>%
          select(-venue, -cat, -ag) %>%
          DT::datatable(options = list(
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"),
            searchHighlight = TRUE
          ))
      })
      #browser()
      x <- summaryTab_server('sumTab', reactive(filterData()))
 
      
    }
  )
}
