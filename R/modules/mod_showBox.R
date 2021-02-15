#- ------------------------------------------
#- PARENT PACKAGE: Race Analysis Project
#- MODULE NAME: mod_showBox
#- DESCRIPTION: ui and server to display a boxplot
#- AUTHOR: lg
#- DATE: 2021-01-26
#- ------------------------------------------

mod_showBox_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns('boxPlot'))
  )
}

mod_showBox_server <- function(id, getData, getTitle) {
  moduleServer(
    id,
    function(input, output, session) {
#      browser()
      output$boxPlot <- renderPlot({
        req(getData())
        colName <- names(getData())
        ggplot(getData(), aes(.data[[colName[1]]], .data[[colName[2]]])) +
          geom_boxplot(aes(fill = .data[[colName[1]]]),
                     show.legend = FALSE) +
          labs(title = getTitle())
          
      })
    }
  )
}
