
race_trend <- function(inputAthleteName) {

  library(shiny)
  
  shinyApp(
    ui = fluidPage(
      
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
               uiOutput('selectDateRange')
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
                 tabPanel("Race Info", h4("Another Table")
                          # div(DT::dataTableOutput('raceTable'),
                          #     style = "font-size: 65%; width: 65%"
                          # )
                 )
               )
        )
      )
    ),
  
  server = function(input, output) {
    library(data.table)
    library(tidyverse)
    library(lubridate)
    
    setwd('/Users/gould/LX/src/rap2')
    
    #- Read data from the main RAP database
    db <- read.table(file="clean_data/temp.db", sep="|",
                     header=T,
                     #row.names=1,
                     na.strings="0",
                     quote="")
    db$DATE <- mdy(db$DATE)
    
    #- make db tidy
    db.tidy <- db %>% 
      select(DATE, GENDER, PLACE, AGE, NAME, SWIM, T1, BIKE, T2, RUN, TIME, VENUE, DISTANCE, EVENT, AG) %>%
      gather(sport, sport.time, SWIM:TIME)
    
    inputAthleteName = 'LOWELL GOULD'
    #inputAthleteName = 'KENT GRAY'
    sportList = c('SWIM', 'T1', 'BIKE', 'T2', 'RUN', 'TIME')
    distanceList <- levels(db$DISTANCE)
    athleteGender <- unique(db[db$NAME == inputAthleteName, 'GENDER'])
    
    #- ===============================================
    #- Set up the dynamic UI elements
    
    #- Race distance selector
    output$selectDist <- renderUI({
      selectInput("dist", 
                  label = "Race Distance",
                  choices = as.list(distanceList),
                  selected = distanceList[-1])
    })
    
    
    #- Swim, Bike, Run, Overall, etc
    output$radioSport <- renderUI({
      radioButtons('sport', 'Sports',
                   choices = as.list(sportList),
                   selected = nrow(sportList)
      )
    })
    #- Set up date range selector
    output$selectDateRange <- renderUI ({
      df.date <- db
      
      minDate <- min(df.date$DATE)
      maxDate <- max(df.date$DATE)
      sliderInput('dates', 'Date Range',
                  min = minDate,
                  max = maxDate,
                  value=c(minDate, maxDate),
                  dragRange=TRUE,
                  step=365,  #- units are days. we want yearly step
                  timeFormat = "%Y")
    })
    
    #- ===============================================
    
    #- Reduce the data frame to a manageable size
    df <- reactive({
      #- Decide which population to use.  It will either be age group
      #- or base.
      #   if ( input$pop == 'ag' ) {
      event.list <- db %>%
        mutate (event.ag = paste0(EVENT,'_', AG)) %>%
        filter(NAME == inputAthleteName &
                 DISTANCE == input$dist &
                 between(DATE, input$dates[1], input$dates[2])
        ) %>%
        select(event.ag)
      
      db.tidy %>% 
        mutate(event.ag = paste0(EVENT,'_', AG)) %>%
        filter(event.ag %in% event.list$event.ag & sport == input$sport) %>%
        select(NAME, EVENT, AG, sport.time, AGE, DISTANCE, DATE, VENUE, event.ag) %>%
        mutate(rtime = sport.time / 60,
               rvenue = paste0(year(DATE), sprintf("%03d",yday(DATE)), "_", VENUE)
        )
    })
    
    #- Calculate mean and SD for race time by venue and ag
    plot.df <- reactive({
      
      times.df <- df()
      min.date = min(times.df$DATE)
      
      times.df %>%
        group_by(event.ag) %>%
        summarize(m.time = mean(rtime, na.rm=T),
                  s.time = sd(rtime, na.rm=T),
                  min.time = min(rtime),
                  n = n()
        ) %>%
        inner_join(times.df) %>%
        mutate(centered = rtime-m.time,
               std.time = centered/s.time,
               behind = rtime - min.time,
               event.seq = (DATE-min.date) + 1) %>%
        filter(n >=20 & 
                 std.time < 3) %>%
        select (NAME, EVENT, VENUE, DATE, event.seq, n, rtime, std.time) %>%
        arrange(event.seq)
    })
    
    athleteData <- reactive({
      plot.df() %>%
        filter(NAME == inputAthleteName) %>%
        select(EVENT, VENUE, DATE, event.seq, rtime, std.time)
    })
    
    
    #- ========================================================
    #- Athlete data table definition
    output$dataTable <- DT::renderDataTable (DT::datatable({
      athlete.data <- athleteData()
      
      athlete.data %>%
        mutate(rtime=format(rtime, width=7, digits=2),
               std.time = format(std.time, width=7, digits=2)
        ) %>%
        select(VENUE, DATE, event.seq, rtime, std.time)
    },
    extensions = 'Scroller',
    selection = 'single',
    colnames = c('Race', 'Date', 'Days', 'Time(m)', 'Time(sd)'),
    options=list(paging=FALSE,
                 searching = FALSE,
                 scrollY = 400,
                 scrollX = FALSE,
                 #scroller = TRUE,
                 deferRender = TRUE
    )
    ))
    #- ========================================================
    #- Main Plot
    output$withinAgeGroup <- renderPlot({ 
      if ( is.null(plot.df()) ) {return}
      
      plot.data <- plot.df()
      athlete.data <- athleteData()
      
      ggplot( plot.data ) +
        geom_point(aes(event.seq, std.time), shape = 21, fill = "white") +
        geom_hline(aes(event.seq, std.time), yintercept = 0) +
        geom_point(data = athlete.data, aes(event.seq, std.time),
                   color = 'red', size = 3) +
        geom_smooth(data = athlete.data, method='lm', se=FALSE,
                    aes(event.seq, std.time)) +
        ggtitle("Relative Fitness Plot", subtitle = "Joe Athlete") +
        ylab("Standardized Race Time") +
        scale_x_continuous("Event", breaks=athlete.data$event.seq, 
                           labels = athlete.data$VENUE) +
        theme(axis.text.x=element_text(angle = -60, hjust = 0))
    })
    
    #- Render the popup info on the plot
    output$hover_info <- renderUI({
      hover <- input$plot1_hover
      point <- nearPoints(athleteData(), hover, threshold = 10, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        p(HTML(paste0("<b>Event: </b>", point$VENUE, "<br/>",
                      "<b>Date: </b>", point$DATE, "<br/>",
                      "<b>Deviation: </b>", sprintf("%4.2f",point$std.time), "<br/>"
        )
        )
        )
      )
    })
  },
  options = list(height = 500)
  )
}