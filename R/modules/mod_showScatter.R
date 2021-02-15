#- ------------------------------------------
#- PARENT PACKAGE: Race Analysis Project
#- MODULE NAME: mod_showscatter
#- DESCRIPTION: ui and server to display a count of items in a table
#- AUTHOR: lg
#- DATE: 2021-01-26
#- ------------------------------------------

mod_showScatter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns('scatPlot'))
  )
}

mod_showScatter_server <- function(id, getData, getTitle) {
  moduleServer(
    id,
    function(input, output, session) {
#      browser()
      output$scatPlot <- renderPlot({
        req(getData())
        colName <- names(getData())
        ggplot(getData(), aes(.data[[colName[1]]], .data[[colName[2]]])) +
          geom_point(aes(color = .data[[colName[3]]]),
                     show.legend = FALSE) +
          labs(title = getTitle())
          
      })
    }
  )
}
