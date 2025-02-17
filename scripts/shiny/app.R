if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  here, htmltools, leaflet, lubridate, scales, shiny, shinydashboard, sp, tidyverse) #Make sure all packages required are installed
here::i_am("scripts/shiny/app.R")

fxns_r <- here("scripts/shiny/spatial-page/page_functions.R") #Loading in the script keeps app.R updated without having to run it separately
bottle_rda <- here("data/processed/bottle.RData") #The binary file containing variables and functions required to run the app
stopifnot(file.exists(fxns_r))
source(fxns_r)
load(bottle_rda)

# UI ----

ui <- navbarPage(
  "CalCOFI", id="nav",
  tabPanel(
    "Home",
    includeHTML("about.html")
  ),
  #* Spatial tab ----
  tabPanel(
    "Spatial Trends",
    div(
      class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("style.css"),),
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map1", width="100%", height="100%"),
      
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 700, bottom = "auto",
        width = 330, height = "auto",
        
        h2("Inputs"),
        numericInput(
          'yr', # selection gets stored as `input$yr`
          'Year', 
          min = min(year(bottle$date), na.rm = T),
          max = max(year(bottle$date), na.rm = T),
          value = median(year(bottle$date), na.rm = T),
          step = 1),
        numericInput(
          'qr', # selection gets stored as `input$qr`
          'Quarter',
          min = 1,
          max = 4,
          step = 1,
          value = 1),
        selectInput( #Used for kriging
          'dpth',
          'Depth layer',
          depths),
        selectInput( #Used for updating the plots
          'lin',
          'Transect (Line ID)',
          lines,),
        selectInput( #Used for updating the plots
          'sta',
          'Station ID',
          stations,),
        h4("Select Parameters"),
        radioButtons('param', 'Parameters', 
                     choices = 
                       c("Oxygen" = "oxy",
                       "Temperature" = "temp",
                       "Salinity" = "sal",
                       "Chlorophyll" = "chlorophyll"),
                     selected = "oxy",),
        checkboxInput('show_krig',
                      "Show Smooth (Oxygen) Data", 
                      value = TRUE),
      ),
      #Displaying plots
      column(
        4,
        absolutePanel(
          id = "controls",class = "panel panel-default",
          fixed = TRUE,
          draggable = FALSE, top = 50, left = "auto", right = 0, bottom = "auto",
          width = 600, height = 10000, 
          h2('Profiles'),
          tabsetPanel(
            #Depth profile plot
            tabPanel(
              title = 'Depth profiles',
              width = "100%",
              height = "100%",
              status = 'primary',
              solidHeader = T,
              plotOutput("profile", width = "100%", height = "800px",)),
            #Transect profile plot
            tabPanel(
              title = "Transect Profile",
              width = "100%",
              height = "200%",
              plotOutput('stationline', width = "100%", height = "800px",)),
          ),
        )),
      tags$div(
        id="cite",
        'Data compiled for ', tags$em('CalCOFI'), ' by CCDSP Fellows'),
      actionButton("show", label = NULL, icon = icon("info")),
      ),
  ),
#* temporal tab ----  
  tabPanel(
    "Temporal Trends",
    div(
      class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("style.css"),
        includeScript("gomap.js")),
      leafletOutput("map2", width="100%", height="100%"),
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 700, bottom = "auto",
        width = 330, height = "auto",
        
        h2("Inputs"),
        dateRangeInput( #Used in time series plot, depth average plot, and updating slider for animation
          "animation",
          "Date Range",
          start = "1990-06-14",
          end = "2010-01-26",
          min = as.Date(as.Date(min((bottle$date), na.rm = T), "%Y-%m-%d"), "%Y-%m-%d"),
          max = as.Date(as.Date(max((bottle$date), na.rm = T), "%Y-%m-%d"), "%Y-%m-%d"),
          format = "yyyy-mm-dd",
          startview = "month",
          weekstart = 0,
          language = "en",
          separator = " to ",
          width = NULL,
          autoclose = TRUE
        ),
        sliderInput( #Used only for map animation
          "times",
          "Press Play to Animate",
          min = as.Date(as.Date(min((bottle$date), na.rm = T), "%Y-%m-%d"), "%Y-%m-%d"),
          max = as.Date(as.Date(max((bottle$date), na.rm = T), "%Y-%m-%d"), "%Y-%m-%d"),
          value = as.Date(as.Date(min((bottle$date), na.rm = T), "%Y-%m-%d"), "%Y-%m-%d"),
          animate = animationOptions(interval = 400, loop = TRUE),
          step = 30,
          timeFormat = "%b %Y",
        ),
        selectInput( #Used to update map
          'qr2',
          'Quarter',
          1:4,
          selected = 1),
        numericInput( #Used to update map
          'yr2', # selection gets stored as `input$yr`
          'Year', 
          min = min(year(bottle$date), na.rm = T),
          max = max(year(bottle$date), na.rm = T),
          value = 1992,
          step = 1),
        selectInput( #Used for the time series plot
          'num_depths',
          'Number of Depth Bins',
          2:10,
          selected = 5),
        selectInput( #Used to update depth_avg_plots
          'lin2',
          'Line ID',
          lines,),
        selectInput( #Used to update depth_avg_plots
          'sta2',
          'Station ID',
          stations,),
        radioButtons('param2', 
                     'Parameters', 
             choices = 
               c("Oxygen" = "oxy2",
                 "Temperature" = "temp2",
                 "Salinity" = "sal2",
                 "Chlorophyll" = "chlorophyll2"),
             selected = "oxy2",),
      ),
      column( #Display plots
        4,
        absolutePanel(
          id = "controls",class = "panel panel-default",
          fixed = TRUE,
          draggable = FALSE, top = 50, left = "auto", right = 0, bottom = "auto",
          width = 600, height = 1000000, 
          h2('Plots'),
          tabsetPanel(
            tabPanel(
              title = 'Time Series Plots',
              plotOutput('time_series', width = "100%", height = "800px"),
              width = "100%",
              height = "100%",
              status = 'primary',
              solidHeader = T,
            ),
        #     tabPanel(
        #       title = "Depth Average Plots",
        #       plotOutput('depthavg', width = "100%", height = "800px",),
        #       width = "100%",
        #       height = "200%",),
        ),
        ),
      ),
      tags$div(
        id="cite2",
        'Data compiled for ', tags$em('CalCOFI'), ' by CCDSP Fellows'),
      actionButton("show2", label = NULL, icon = icon("info")),
    ),
  ),
)

# SERVER ----
server <- function(input, output, session) {
  
  # map ----
  # user retrieve data by year to make the basemap and place the markers indicating station locations
  map_data <- reactive({get_map_data(input$yr, input$qr)})
  map_data2 <- reactive({get_map_data(input$yr2, input$qr2)})
  kriging_data <- reactive({
    if(input$show_krig == TRUE){
      get_kriging_data(input$yr, input$qr, input$dpth)}
    else{
      NULL}
  })
  
  # base map layer (will show default year 2000)
  output$map1 <- renderLeaflet({make_basemap()})
  output$map2 <- renderLeaflet({make_basemap()})
  
  # * update_basemap() ----
  # plot user's selection
  observe({
    input$nav
    tab1 <- leafletProxy('map1') %>%
      update_basemap(map_data(), kriging_data())
    tab2 <- leafletProxy('map2') %>%
      update_basemap(map_data2(), NULL)
  })
  
  # * map1_marker_click ----
  # When marker is clicked, update Station and Line ID selectors
  observe({
    event <- input$map1_marker_click
    if (is.null(event))
      return()
    updateSelectInput(session, "sta", selected=strsplit(event$id," ")[[1]][[2]])
  })
  observe({
    event <- input$map1_marker_click
    if (is.null(event))
      return()
    updateSelectInput(session, "lin", selected=strsplit(event$id," ")[[1]][[1]])
  })
  observe({
    event <- input$map2_marker_click
    if (is.null(event))
      return()
    updateSelectInput(session, "sta2", selected=strsplit(event$id," ")[[1]][[2]])
  })
  observe({
    event <- input$map2_marker_click
    if (is.null(event))
      return()
    updateSelectInput(session, "lin2", selected=strsplit(event$id," ")[[1]][[1]])
  })
  #For temporal tab inputs
  #Date Range input updates endpoints of the slider inputs
  observe({
   val <- input$animation
   if (is.null(val))
     return()
   updateSliderInput(session, "times", value = as.Date(val[1], "%Y-%m-%d"),
                     min = as.Date(val[1],"%Y-%m-%d"), max = as.Date(val[2], "%Y-%m-%d"), 
                     timeFormat = "%Y-%m-%d")
  })
  #Slider updates the quarter and year values to update the map
  observe({
    quarter_val <- tibble(
      date = format(input$times, "%Y-%m-%d") %>% as.Date()) %>% 
      mutate(quarter = lubridate::quarter(date))
    updateSelectInput(session, "qr2", selected = as.character(quarter_val))
  })
  observe({
    year_val <- format(input$times, "%Y")
    updateNumericInput(session, "yr2", value = year_val)
  })
  
  ## PROFILE PANEL
  # user retrieve data by year/quarter and parameter for profile plots
  profile_plot <- reactive({
    make_profile(input$yr, input$lin, input$param)
  })
  
 #Station line plots use if else to determine which plot to show based on which parameter
  station_line_plot <- reactive({
    if(input$param == "oxy"){
      make_station_line(input$yr, input$lin)
    }else{
      if(input$param == "temp"){
        make_station_line_temp(input$yr, input$lin)
      }else{
        if(input$param == "sal"){
          make_station_line_salinity(input$yr, input$lin)
        }
        else{
          make_station_line_chlor(input$yr, input$lin)
        }
      }
    }
  })
  #*---- Intro to CalCOFI modal
  output[["image"]] <- renderImage({
    list(src = "ims/calcofi_header.png",
         alt = "This is alternate text",
         width = "100%",
         height = "75px")}, deleteFile = FALSE)
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "How to use this app",
      imageOutput("image", height = "100px"),
      HTML(
      "The purpose of this page is to show the variation of the sampling patterns and parameters with space. 
      On this tab are two plots, the Depth Profile Plots and the Transect Profile Plots.
      <h4> Depth Profile Plots </h4>
      These &quot;ponytail&quot; plots show the dissolved oxygen concentration by depth for the individual stations, 
      shown as the &quot;strands&quot; of the &quot;ponytails&quot;. Most of the &quot;strands&quot; follow the 
      same path of constant dissolved oxygen (DO) concentration at the surface before quickly declining in DO and 
      decreasing with depth. We are particularly interested in the stations or &quot;strands&quot; that 
      quickly decline in DO at shallower depths than other stations during seasons that should be experiencing higher
      oxygen concentration, Summer and Fall, because this could indicate hypoxia. 
      <h4> Transect Profile Plots </h4>
      These profiles show the average value of dissolved oxygen concentration, salinity, temperature, or
      chlorophyll concentration by depth and distance from shore for each transect line. Click on the desired
      parameter in the inputs tab to see each parameter's plot. Chlorophyll is a good indicator of nutrient
      levels in the ocean, so look out for high chlorophyll levels! <br>
      When viewing the plots of average oxygen level, note that at deeper depths it is normal for these bottom
      waters to have very little oxygen and low temperatures. We are interested in the graphs where the red and
      black colors, indicating oxygen levels at and below the hypoxic threshold, invade shallower depths, especially
      in the Summer and Fall when seasonal cycles allow for higher oxygen at the surface.
      <h4>Spatial Interpolation </h4>
      This plot shows continuous data created from discrete data for oxygen concentration by depth. There is a selectable
      range of depths to view from 0-50, 50-100, 100-250, 250-500. The plot displays a light blue to black to red color gradient, 
      with black and red indicating hypoxic zones. We are interested in the appearance of black and red colors at the surface during
      Summer and Fall."),
      easyClose = FALSE,
    ),
    )
  })
  #*---- Temporal Tab modal
  output[["image2"]] <- renderImage({
    list(src = "ims/calcofi_header.png",
         alt = "This is alternate text",
         width = "100%",
         height = "75px")}, deleteFile = FALSE)
  observeEvent(input$show2, {
    showModal(modalDialog(
      title = "The Temporal Tab",
      imageOutput("image2", height = "100px"),
      HTML(
        "The purpose of this page is to show the variation of the sampling patterns and parameters with time. 
        On this tab are two plots: the Time Series Plot and the Depth Average Plot. <br>
      <h4> Time Series Plots </h4>
      The time series plot shows you how a parameter (oxygen, temperature)
      changes over a given time range, at various depth ranges (0-50m, 50-100m, 200-500m). 
      The output plot is the average value for all of the stations at a given time.
        <br>
        For the time series plot:
        <ol>
          <li>Select the date range input to select a time range you want to plot.</li>
          <li>Select the depth ranges you want to plot.</li>
          <li>Select the parameter of interest.</li>
          <li>Refer to the legend to see that each point shape indicates what quarter that was sampled for.</li>
          <li>Refer to the legend to see what depth range you are looking at.</li>
        </ol>
        <h4> Depth Average Plot </h4>
        The depth average plot shows you how oxygen 
        changes over depth ranges, as well as the value of that parameter on a given date, 
        and the minimum and maximum values over a date range. This data is for one station.
        <br>
        For the depth average plot:
        <ol>
          <li>Select the date range using the Date Range Input for the range you want to plot.</li>
          <li>Click on a station <strong> on the map </strong> to update the station and line input.</li>
          <li>Select the parameter of interest</li>
          <li>Press play on the slider to animate the current value of the parameter on the plot.</li>
        </ol>
        This page also has the capability to animate through the station and line sampling over the date 
        range selected. To do so simply:
        <ol>
          <li>Select a date range using the Date Range Input</li>
          <li>Press play on the slider to show the sampling patterns over the years.</li>
        </ol>"),
      #size = "l",
      easyClose = FALSE,
    ),
    )
  })
  
  #Temporal tab plots
  
  depth_av_plot <- reactive({
    range_select <- input$animation
    depth_avg_plot(as.Date(range_select[1], "%Y-%m-%d"), as.Date(range_select[2], "%Y-%m-%d"), 
                   input$times, input$sta2, input$lin2)
  })
  ts_plot <- reactive({
    daterange <- input$animation
    if(input$param2 == "oxy2"){
    oxy_ts_plot(as.integer(input$num_depths), as.character(daterange[1]), as.character(daterange[2]))
    }
    else{
      if(input$param2 == "temp2"){
        temp_ts_plot(as.integer(input$num_depths), as.character(daterange[1]), as.character(daterange[2]))
      }else{
        if(input$param2 == "sal2"){
          sal_ts_plot(as.integer(input$num_depths), as.character(daterange[1]), as.character(daterange[2]))
        }
        else{
          cho_ts_plot(as.integer(input$num_depths), as.character(daterange[1]), as.character(daterange[2]))
        }
      }
    }
  })
  # plot outputs
  output$profile <- renderPlot({profile_plot()})
  output$stationline <- renderPlot({station_line_plot()})
  output$depthavg <- renderPlot({depth_av_plot()})
  output$time_series <- renderPlot({ts_plot()})
  
} 

## ---------------
## DEPLOY

shinyApp(ui,server)
