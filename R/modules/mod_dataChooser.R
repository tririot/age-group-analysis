#- ------------------------------------------
#- PARENT PACKAGE: Race Analysis Project
#- MODULE NAME: mod_dataChooser
#- DESCRIPTION: ui and server to select data from the races dataframe
#- AUTHOR: lg
#- DATE: 2021-01-04
#- ------------------------------------------

mod_dataChooser_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('selSport')),
    checkboxGroupInput(ns('genders'), "Select Gender(s)",
                       choices = c("Male" = "M", "Female" = "F"),
                       selected = "M",
                       inline = TRUE
    ),
    uiOutput(ns('selDistance')),
    uiOutput(ns('selRaceYear')),
    uiOutput(ns('selAges')),
    uiOutput(ns('selGroupSize')),
    checkboxInput(ns('zeroTimes'), "Include Zero Times",
                  value = FALSE),
    checkboxInput(ns('others'), "Include Other Categories",
                  value = FALSE),
    hr(),
    textOutput(ns('datasize'))
  )
}

mod_dataChooser_server <- function(id, getRaceData) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #- ==== DYNAMIC UI ====
      output$selSport <- renderUI({
        ns <- session$ns
        sportList <- unique(getRaceData()$sport)
        selectInput(ns("sports"), "Select Sport",
                    choices = sportList,
                    selected = 'overall',
                    multiple = FALSE)
      })
      output$selDistance <- renderUI({
        ns <- session$ns
        distanceList <- unique(getRaceData()$dist)
        selectInput(ns("distances"), "Select Distance",
                    choices = distanceList,
                    selected = 'SPRINT',
                    multiple = FALSE)
      })
      output$selAges <- renderUI({
        ns <- session$ns
        sliderInput(ns("ages"), "Select Age Range",
                    min = min(getRaceData()$age),
                    max = max(getRaceData()$age),
                    step = 5,
                    value = c(30,60)
        )
      })
      output$selRaceYear <- renderUI({
        ns <- session$ns
        sliderInput(ns("raceyears"), "Select Date Range",
                    min = min(getRaceData()$ryear),
                    max = max(getRaceData()$ryear),
                    value = c(2010, 2018)
        )
      })
      output$selGroupSize <- renderUI({
        ns <- session$ns
        maxGroupSize <- max(getRaceData()$g.count)
        sliderInput(ns("groupsize"), "Select Min Group Size",
                    min = 5,
                    max = maxGroupSize,
                    value = 25
        )
      })
      
      #- ==== REACTIVE ELEMENTS
      filterData <- reactive({
        validate(
          need(getRaceData(), 'No Data To Display'),
          need(input$genders, 'Choose a gender'),
          need(input$sports, 'Choose a sport'),
          need(input$distances, 'Choose a distance'),
          need(input$ages, 'Choose an age range'),
          need(input$raceyears, 'Choose a year range'),
          need(input$groupsize, 'Choose a group size range')
        )
        getRaceData() %>%
          filter(
            (is.null(input$sports) | sport %in% input$sports), 
            (is.null(input$genders) | sex %in% input$genders),
            (is.null(input$distances) | dist %in% input$distances),
            is.null(input$ages) | (age >= input$ages[1] & age <= input$ages[2]),
            between(ryear, input$raceyears[1], input$raceyears[2]),
            between(g.count, input$groupsize[1], input$groupsize[2]),
            (input$zeroTimes | stime != 0),
            (input$others | grepl("^[MF][0-9][0-9]", .data$rcat, perl=TRUE))
          )
      })
      
      #- ==== OUTPUT ELEMENTS
      output$datasize <- renderText({
        req(filterData())
        recordCount <- nrow(filterData())
        sprintf("%-20s: %s\n", "Record Count", recordCount)
      })
      
      return (
        list (
          df = reactive({filterData()})
        )
      )
    }
  )
}
