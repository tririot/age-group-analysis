#- ------------------------------------------
#- PARENT PACKAGE: Race Analysis Project
#- MODULE NAME: mod_showCountTable
#- DESCRIPTION: ui and server to display a count of items in a table
#- AUTHOR: lg
#- DATE: 2021-01-26
#- ------------------------------------------

showCountTable_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tableOutput(ns('countTable'))
  )
}

showCountTable_server <- function(id, getTable) {
  #- getTable is a dataframe. Only the first column is used
  #- getScrollLen is the page length
  moduleServer(
    id,
    function(input, output, session) {
      output$countTable <- renderTable({
        colName <- names(getTable())
        getTable() %>% 
          group_by(.data[[colName[1]]]) %>% 
          tally()
      },
      options = list(pageLength = 5)
      )
    }
  )
}
