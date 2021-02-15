#- ------------------------------------------
#- PARENT PACKAGE: Race Analysis Project
#- MODULE NAME: mod_selectVars1
#- DESCRIPTION: ui and server to display athlete data 
#- AUTHOR: lg
#- DATE: 2021-01-04
#- ------------------------------------------

mod_dataChooser_ui <- function(id) {
  ns <- NS(id)
  tagList(
      uiOutput(ns('selSport')),
      selectInput(ns('genders'), "Select Genders",
                  choices = c("Male" = "M", "Female" = "F")),
      uiOutput(ns('selDistance')),
      uiOutput(ns('selRaceYear')),
      uiOutput(ns('selGroupSize')),
      hr(),
      DT::dataTableOutput(ns("dataSample"))
  )
}

mod_dataChooser_server <- function(id, getRaceData) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #- ==== DYNAMIC UI ====
      output$selSport <- renderUI({
        selectInput("sports", "Select Sports",
                    choices = sportList,
                    multiple = TRUE)
      })
      output$selDistance <- renderUI({
        selectInput("distances", "Select Distance",
                    choices = distanceList,
                    multiple = TRUE)
      })
      output$selAges <- renderUI({
        sliderInput("ages", "Select Age Range",
                    min = minAge,
                    max = maxAge,
                    value = initAgeRange, 
                    format = '##')
      })
      output$selRaceYear <- renderUI({
        sliderInput("raceyears", "Select Date Range",
                    min = min(getRaceData()$ryear),
                    max = max(getRaceData()$ryear),
                    value = c(2010, 2018), 
                    format = 'yyyy')
      })
      output$selGroupSize <- renderUI({
        
        sliderInput("groupsize", "Select Min Group Size",
                    min = 5,
                    max)
      })
      
      #- ==== REACTIVE ELEMENTS
      filterData <- reactive({
        races %>%
          filter(
            is.null(input$events) | event %in% input$events,
            is.null(input$distances) | dist %in% input$distances,
            is.null(input$sports) | sport %in% input$sports,
            is.null(input$categories) | rcat %in% input$categories,
            between(rdate, input$dates[1], input$dates[2]),
            between(age, input$ages[1], input$ages[2])
          )
      })
        
      #- ==== OUTPUT ELEMENTS
      output$dataSample <- DT::renderDataTable({
        req(filterData())
        DT::datatable(filterData())
      })
#       
# selectVarsServer <- function(id, getVars) {
#   moduleServer(
#     id, 
#     function(input, output, session) {
# 
#         req(getVars(), 'No Data To Display')      
#       #- ==== DYNAMIC UI ====
#       output$uiChooseDistance <- renderUI({
#         ns <- session$ns
#         distChoices <- unique(getVars()$dist)
#         selectInput(ns('dist'), label = 'Race Distance',
#                     choices = distChoices,
#                     selected = distChoices[1])
#       })
#       
#       # output$uiChooseDates <- renderUI({
#       #   ns <- session$ns
#       # })
#       # 
#       # output$uiChooseSport <- renderUI({
#       #   ns <- session$ns
#       #   selectInput(ns('sport'), label = 'Discipline', 
#       #               choices = ,
#       #               selected = )
#       # })
#       # 
#       # output$uiChoosePopulation <- renderUI({
#       #   ns <- session$ns
#       #   selectInput(ns('population'), label = 'Population',
#       #               choices = ,
#       #               selected = )
#       # })
#       # 
#       # output$uiChooseOutlierLevel <- renderUI({
#       #   ns <- session$ns
#       #   
#       # })
#       # 
#       # output$uiChooseVenue <- renderUI({
#       #   ns <- session$ns
#       #   selectInput(ns('venue'), label= 'Race Venue',
#       #               choices = ,
#       #               selected = )
#       # })
#       # 
#       
#       return (
#         list (
#           dist = reactive({input$dist})
#           # ,
#           # dates = reactive({input$dates}),
#           # sport = reactive({input$sport}),
#           # popn = reactive({input$popuplation}),
#           # vobs = reactive({input$validObs}),
#           # venue = reactive({input$venue})
#         )
#       )

    }
  )
}
