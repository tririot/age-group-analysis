library(shiny)

fluidPage(
  
  # Application title
  titlePanel("Fitness Tracker"),
  
  div(plotOutput('withinAgeGroup', 
                 hover=hoverOpts(id='plot1_hover',
                                 delay = 100, 
                                 delayType = "debounce")
                 ),
      uiOutput('hover_info'),
      style = "width: 90%; height: 90%"
   ),
  hr(),
  fluidRow(
    column(3, 
           h4("Input Controls"),
           uiOutput('selectDist'),
           #uiOutput('selectEvent'),
           uiOutput('selectDateRange'),
           textOutput('dateRangeText')
    ),
    column(2,
           uiOutput('radioSport')
           ),
    column(7,
           tabsetPanel(
             tabPanel("Athlete Data",
                      div(DT::dataTableOutput('dataTable'),
                          style = "font-size: 65%; width: 65%"
                      )
             ),
             uiOutput('selectRace'),
             tabPanel("Transitions",
                      div(DT::dataTableOutput('transitionTable'),
                          style = "font-size: 65%; width: 65%"
                      )
             )
           )
    )
  )
)