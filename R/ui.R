#- ------------------------------------------
#- PARENT PACKAGE: shiny-external_data_protal 
#- MODULE NAME: ui.R
#- AUTHOR: lg
#- DATE: 2020-12-29
#- ------------------------------------------

ui <- fluidPage(
  headerPanel('Analysis of Age and Performance',
              windowTitle = 'TriRiot Analysis'),
  sidebarLayout(
    sidebarPanel(width = 3,
      h2("Filter The Data"), 
      hr(),
      mod_dataChooser_ui('main')
    ),
    mainPanel(
      fluidRow(
        column(6,
               mod_showScatter_ui('scat1')
        ),
        column(6,
               mod_showBox_ui('box1')
        )
      ),
      fluidRow (
        column(6,
               mod_showKmeans_ui('k1')
        ),
        column(6,
               mod_showBox_ui('top3box')
        )
        
      )
      
    )
  )
)