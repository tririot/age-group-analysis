#- ------------------------------------------
#- PARENT PACKAGE: Race Analysis Project
#- MODULE NAME: mod_summaryTab
#- DESCRIPTION: ui and server to display athlete data 
#- AUTHOR: lg
#- DATE: 2021-01-26
#- ------------------------------------------

summaryTab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(1),
      column(3, showCountTable_ui(ns('years'))),
      column(3, showCountTable_ui(ns('genders')),
             showCountTable_ui(ns('distances'))
      ),
      column(3, showCountTable_ui(ns('categories')))
    )
  )
}

summaryTab_server <- function(id, getDataFrame) {
  moduleServer(
    id,
    function(input, output, session) {
      
      showCountTable_server('years', reactive(select(getDataFrame(), ryear)))
      showCountTable_server('genders', reactive(select(getDataFrame(), sex)))
      showCountTable_server('distances', reactive(select(getDataFrame(), dist)))
      showCountTable_server('catyehories', reactive(select(getDataFrame(), rcat)))
      
      return(reactive({
        NA
      }))
    }
  )
}
