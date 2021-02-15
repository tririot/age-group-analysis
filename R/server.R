#- ------------------------------------------
#- PARENT PACKAGE: shiny-external_data_protal 
#- MODULE NAME: server.R
#- AUTHOR: lg
#- DATE: 2020-12-29
#- ------------------------------------------


server <- function(input, output, session) {
  data <- reactive({
    races
  })
  
  returnList <- mod_dataChooser_server('main', reactive(data()))
  
  title1 = paste0("Scatter plot of time on age")
  mod_showScatter_server('scat1', 
                         reactive(returnList$df()[,c('age', 'stime', 'rcat')]),
                         reactive(title1)
  )
  title2 = paste0("Box plot of time on age group")
  mod_showBox_server('box1',
                     reactive(returnList$df()[,c('rcat', 'stime')]),
                     reactive(title2))
  
  mod_showKmeans_server('k1',
                        reactive(returnList$df()))
  
  top3 <- reactive({
    req(returnList$df())
    returnList$df() %>%
      group_by (event, rcat) %>%
      mutate(r = dense_rank(stime)) %>%
      filter(r <= 3 )
  })
  
  title4 = paste0("Box plot of time on age group for top 3 finishers")
  mod_showBox_server('top3box',
                     reactive({top3()[,c('rcat', 'stime')]}),
                     reactive({title4}))
}
