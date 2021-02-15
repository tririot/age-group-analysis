#- ------------------------------------------
#- PARENT PACKAGE: shiny-external_data_protal 
#- MODULE NAME: server.R
#- AUTHOR: lg
#- DATE: 2020-12-29
#- ------------------------------------------


server <- function(input, output, session) {
  races <- getData('/Users/gould/LX/src/repositories/Race-Analysis-Project/data/raw.Rdata')
  races <- cleanCategories(races)
  races <- cleanGenders(races)
  races <- cleanTimes(races)
  
  racesOverall <- races %>% 
    filter(sport == 'overall',
           stime != 0)
  
  dataFactors <- c('Event'= 'event', 
                   'Gender' = 'sex',
                   'Distance' =  'dist', 
                   'Venue' = 'venue', 
                   'Race Category' = 'rcat',
                   'Race Year' = 'ryear')
  
  #- ==== DYNAMIC UI ELEMENTS
  output$chooseFactor <- renderUI({
    selectInput('selFactor', 'Select Grouping Factor',
                choices = dataFactors)
  })
  
  output$chooseXAxis <- renderUI({
    req(input$selFactor)
    remainingFactors <- dataFactors[dataFactors != input$selFactor]
    selectInput('selX', 'Select Factor for X Axis',
                choices = remainingFactors)
  })
  
  #- ==== REACTIVE DATA SETS
  getFactorLabel <- reactive({
    req(input$selFactor)
    x <- names(dataFactors)
    x[which(dataFactors == input$selFactor)]
  })
  
  getFactorLevels <- reactive({
    req(racesOverall)
    req(input$selFactor)
    
    racesOverall %>% 
      group_by_at(input$selFactor) %>%
      summarise(record_count = n(),
                mean_time = mean(stime),
                sd_time = sd(stime)) %>%
      arrange_at(input$selFactor)
  })
  
  getLevelDetails <- reactive({
    req(getFactorLevels())
    req(input$selX)
    rows <- input$factorLevelsTable_rows_selected
    validate(need(length(rows) > 0, "No rows have been selected for display"))

    levelsSelected = as.character(pull(getFactorLevels()[rows,1]))
    colName <- as.character(input$selFactor)
#browser()
    x <- racesOverall %>%
      filter_at(colName, all_vars(. %in% levelsSelected)) %>%
      group_by_at(vars(input$selX, colName)) %>%
      summarize(n = n(),
                m_stime = mean(stime))
    names(x)[2] = 'factor.level'
    x
  })
  
  #- ==== OUTPUT OBJECTS  
  output$factorTableTitle <- renderText({
    req(getFactorLabel())
    label <- plural(getFactorLabel())
    paste("Summary Data For", label)
  })
  
  output$factorLevelsTable <- DT::renderDataTable({
    getFactorLevels()
  })
  
  output$selectedLevelsTable <- DT::renderDataTable({
    getLevelDetails()
  })
  
  output$sumData <- renderTable({
    validate(need(input$selFactor, "Choose a grouping factor from the dropdown"))
    validate(need(getFactorLabel(), "Choose a grouping factor from the dropdown"))
    getSummaryTable(racesOverall, input$selFactor, getFactorLabel())
  }, rownames=TRUE)
  
  output$factorDetailsTimeTrend <- renderPlot({
    req(getLevelDetails())
    req(input$selX)
    #browser()
    x.var <- input$selX
    y.var <- 'n'
    
    ggplot(getLevelDetails(), 
           aes(.data[[x.var]], .data[[y.var]])) +
      geom_point() +
      geom_line(aes(color = factor.level))
    
    
  })
}
