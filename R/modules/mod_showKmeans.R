#- ------------------------------------------
#- PARENT PACKAGE: Race Analysis Project
#- MODULE NAME: mod_showKmeans
#- DESCRIPTION: ui and server to display the kmeans of time by age group
#- AUTHOR: lg
#- DATE: 2021-01-26
#- ------------------------------------------

mod_showKmeans_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns('kPlot'))
  )
}

mod_showKmeans_server <- function(id, getData, getTitle) {
  moduleServer(
    id,
    function(input, output, session) {

      getClust <- reactive({
        req(getData())
        #- The number of groups is equal to the number
        #- of categories presented in getData()
        grpCount <- length(unique(getData()$rcat))
        set.seed(1234)  #- we'll use a constant seed for reproducability
        kmeans(getData()$stime, grpCount)
      })
      
      output$kPlot <- renderPlot({
        validate(
          need(getData(), 'No dataset selected yet'),
          need(getClust(), 'No cluster data yet')
        )
        
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        #- get cluster data
        mainPlotData <- getData() %>%
          add_column(kGrps = getClust()$cluster)
        clustCent <- data.frame(clust = seq(1, length(getClust()$centers)),
                                cent = getClust()$centers
        )
                                               
#browser()
        ggplot(mainPlotData, aes(x = kGrps, y = stime)) +
          geom_point(aes(color = rcat),
                     show.legend = FALSE) +
          #- mark the cluster centers on the plot
          geom_point(data=clustCent, 
                     aes(x = clust, y = cent,
                         size=4),
                     color = 'red',
                     show.legend = FALSE) +
          labs(x = 'Cluster Group', y = 'Race Time',
               title = "Race time divided into clusters")
       })
      
    }
  )
}
