# Author: Suvansh Vaid
# Description - This is a narrative visualization aimed at enlightening the users 
# with the past few year trends of the road crashes in Victoria, their effect on 
# different road populations and investigating factors (such as road conditions)
# that may have had an effect on the crashes. 

# Libraries used
require("shiny")
require("shinythemes")
require("shinydashboard")
require("tidyverse")
require("leaflet")
require("plotly")
require("highcharter") 
require("shinycustomloader")
require("rgdal")

# Sourcing the preprocess file
source("preprocess.R", local = T)

# ---------------------------------- SHINY UI ----------------------------------#

# Creating a shinyUI function for the User Interface
shinyUI <-  dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "Road crash Analysis - Victoria",
    titleWidth = 1250
  ),
  
  # Creating a sidebar for the shiny dashboard 
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("th")),
      menuItem("Regional trends across Victoria", tabName = "regional", icon = icon("th")),
      menuItem("Population analysis", tabName = "population", icon = icon("th")),
      menuItem("Day-Time Accident trends", tabName = "daytime", icon = icon("th")),
      menuItem("About", tabName = "about", icon = icon("th"))
    )
  ),
  dashboardBody(
    
    # creating an onclick script for the sidebar items
    tags$script(
      HTML(
        "
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      "
      )
    ),
    
    # Creating tabs for the narrative visualization
    tabItems(
      # ----------------------The landing page (Introduction tab)--------------#
      tabItem(
        tabName = "introduction",
        
        fluidPage(
          # Adding a logo image
          HTML('<center><img src="vicroads.png" width="300"></center>'),
          tags$head(
            tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@500&display=swap');
      
      # Setting CSS style scripts for h1, h4 and h5 tag elements used
      h1 {
        font-family: 'Space Grotesk', sans-serif;
        color: black;
      }
      
      h4 {
        font-family: 'Space Grotesk', sans-serif;
        color: black;
        text-align: justify;
      } 
      
      h5 {
        font-family: 'Space Grotesk', sans-serif;
        color: black;
        text-align: justify;
      }
    "))
          ),
          
          titlePanel(
            h1("Welcome to the narrative visualization!", align = "center")
          ),
          
          fluidRow(
            column(10,offset = 1, tags$div(
              
              HTML("<h4>Road accidents are a major problem as they cause financial and health 
      damage both to the society and the individuals involved. This narrative 
      project would help you understand the road crash trends in the state of Victoria since the 
      last 5 years and also investigate the factors affecting your safety on the road.</h4><br>
           ")
            )
            )
          ),
          fluidRow(
            column(10,offset = 1, tags$div(
              
              HTML("<h4>There are three different views created for 3 different types of audiences.</h4><br>")
            )
            )
          ),
          
          # Displaying 3 Info boxes for easier navigation
          fluidRow(
            br(),
            width = 12,
            infoBoxOutput("tab1"),
            tags$style("#tab1 {width:1000px;}")),
          
          fluidRow(
            br(),
            width = 12,
            infoBoxOutput("tab2"),
            tags$style("#tab2 {width:1000px;}")),
          
          fluidRow(
            br(),
            width = 12,
            infoBoxOutput("tab3"),
            tags$style("#tab3 {width:1000px;}"))
          
          # fluidRow(
          #   tags$div(HTML("<p><a href = 'https://discover.data.vic.gov.au/dataset/crashes-last-five-years1'> Data Source : Vic Roads</p>"))
          )
        ),
      #-------------------- Regional analysis (First Tab) ---------------------#
      tabItem(
        tabName = "regional",
        fluidRow(
          infoBoxOutput("tab4"),
          tags$style("#tab4 {width:1000px;}")
        ),
        fluidRow(
          column(12,
                 tags$div(
                   HTML("<br><h5>The following chart shows you the accident trend across Victoria from 2013 to 2018 for different categories.</h5><h5>You can compare the trends between two different regions by clicking on the 'Compare' button</h5><h5>Click on a particular year to see the topmost affected regions by category.</h5><br>")
                 )
                 )
        ),

        fluidRow(
          
          column(4,
                 
                 radioButtons(
                   "accident_category",
                   "Choose metric",
                   c("Total Accidents", "Fatalities", "Serious Injuries"),
                   selected = "Total Accidents",  
                   inline = TRUE
                 ),
                 
                 selectInput("region", 
                             "Select region", 
                             regions, 
                             selected = "ALL"),
                 
                 actionButton("back", "Single Region"),
                 
                 HTML('&nbsp; &nbsp; &nbsp; &nbsp; &nbsp;'),
                 
                 actionButton("compare", "Compare regions"),
                 
                 HTML("<br> <br>"),
                 
                 selectInput("region1", 
                             "Select other region", 
                             regions, 
                             selected = "MELBOURNE"),
                 
                 textOutput("text")
                 
          ),
          
          column(8,
                 highchartOutput("plot1")
          )

        ),
        fluidRow(
          column(12,
                 tags$div(
                   HTML("<br><h5>Most of the regions have an increasing trend till the year 2016 after which the accidents seem to drop. </h5> <h5>We suggest you to play around the charts. Compare the accidents in Melbourne to those in Yarra Ranges. Do you see any differences?</h5> <h5>Also, click on 2016 to see which regions witnessed most of the fatal accidents!</h5><br>")
                 )
                 )
        ),
        fluidRow(
          
          withLoader(highchartOutput("plot_top10"), 
                     loader = "pacman")
        )
    ),
    # -------------------  Population analysis (Second Tab) -------------------#
      tabItem(
        tabName = "population",
        
        fluidRow(infoBoxOutput("tab5"),
                 tags$style("#tab5 {width:1000px;}")),
        
        fluidRow(
          
          box(
            width = 12,
            column(12,
                   tags$div(
                     HTML("<h5>The following map shows you the accident info for different accident types across all the regions (LGAs) in Victoria.</h5><h5>The more darker regions have comparatively more number of accidents in the selected category. Click on a particular region to see how that region has affected the driver/pedestrian population.</h5>")
                   ),
                   radioButtons("accident_type",
                                "Choose metric",
                                c("Total Accidents", 
                                  "Alcohol related", 
                                  "Hit and Run", 
                                  "Off Road"),
                                inline = TRUE)
            ))),
        
        fluidRow(
          
          box(
            width = 12,
            column(7,
                   withLoader(leafletOutput("plot2"), 
                              loader = "pacman")),
            column(5, 
                   withLoader(highchartOutput("plot3"),
                              type = "html",
                              loader = "loader3")
          )
          )
        )
          
      ),
    # -------------------------- Day/time and other factor analysis ----------------------------#
    tabItem(
      tabName = "daytime",
      fluidRow(infoBoxOutput("tab6"),
               tags$style("#tab6 {width:1000px;}")),
      fluidRow(
        column(12,
               tags$div(
                 HTML("<h5>The following is a heat map that shows you how the accidents and their types vary with the day of week and time of the day across all the regions (LGAs) in Victoria. Try checking out the heatmap for Alcohol related accidents and hit-run accidents, do you see any patterns?</h5><h5><br><br>")
               )
               )
        
    ),
      fluidRow(
        
        column(3, 
               radioButtons(
                 "accident_heat",
                 "Choose metric",
                 c("Total Accidents", 
                 "Alcohol related", 
                 "Hit and Run", 
                 "Off Road"),
                 selected = "Total Accidents"
               )),
        column(
          9, 
          highchartOutput("plot4")
        )
      ),
    fluidRow(
      column(12,
             tags$div(
               HTML("<h5>You can click on anywhere on the heatmap to get a more detailed view on the accident count for the factor selected below at the selected time and day of the week.</h5>")
             )
      )
    ),
    fluidRow(
      column(12, 
             tags$div(
               HTML("<h5> You can select different factors to analyse. For example, try checking out the accident types for Friday nights for the alcohol related accidents.</h5>")
             ))
    ),
    fluidRow(
      column(3,
             radioButtons("condition",
                          "Choose a condition",
                          c("Speed Limit",
                            "Collision Type",
                            "Lighting Conditions")
          )
        ),
      column(8,
             withLoader(plotlyOutput("factor_donut"),
                        type = "html",
                        loader = "loader2"))
      )
    ),
    
    tabItem(
      tabName = "about",
      fluidRow(column(12, 
                      tags$div(
                        HTML("<h5> This project is developed under the Monash University guidelines and is still in development stage with frequent updates.</h5> <h5> Developed by: Suvansh Vaid (Master of Data Science, Monash University) </h5> <h5> <a href = 'https://youtu.be/Nku5KifWgf8'> Check out the User guide Video </a> </h5> <h5> Data Source: <a href= 'https://discover.data.vic.gov.au/dataset/crashes-last-five-years1'>Vic Roads</a></h5>")
                      )
                      ))
    )
    
  )
)
)

# -------------------------------- SHINY SERVER --------------------------------#

# Creating a shinyServer function for the output
shinyServer <- function(input, output, session) {
  
  # Rendering the logo image
  output$logo <- renderImage({
    return(list(src = "vicroads.png",contentType = "image/png"))
  }, deleteFile = FALSE)
  
  
  # Creating a reactive data element for the bar plot showing yearly trends.This data changes every time user selects a different input from the accident category radio button group.
  
  plot1_data <- reactive({
    
    if (input$accident_category == 'Total Accidents') {
      
        crash_data %>% 
          filter(LGA_NAME == input$region) %>% 
          group_by(ACCIDENT_YEAR) %>% 
          summarise(Total = n()) %>% 
          ungroup()
      }
      
    
    else if  (input$accident_category == 'Serious Injuries') {
    
          crash_data %>% 
            group_by(ACCIDENT_YEAR) %>% 
            summarise(Total = sum(SERIOUSINJURY)) %>% 
            ungroup()
    }
    
    else {
        
        if (input$accident_category == 'Fatalities'){
          crash_data %>% 
            filter(LGA_NAME == input$region) %>% 
            group_by(ACCIDENT_YEAR) %>% 
            summarise(Total = sum(FATALITY)) %>% 
            ungroup()
        }
    }
      
  })
  
  # Creating data for the comparison of regional trends between two LGAs for different  accident categories.
  plot1compare_data <- reactive({
    
    region_compare <- c(input$region, input$region1)
    
    if (input$accident_category == 'Total Accidents') {
      
      crash_data %>% 
        filter(SUBURB %in% region_compare) %>% 
        group_by(ACCIDENT_YEAR, SUBURB) %>% 
        summarise(Total = n()) %>% 
        ungroup()
    }
    
    
    else if  (input$accident_category == 'Serious Injuries') {
      
      crash_data %>% 
        filter(SUBURB %in% region_compare) %>% 
        group_by(ACCIDENT_YEAR, SUBURB) %>% 
        summarise(Total = sum(SERIOUSINJURY)) %>% 
        ungroup()
    }
    
    else {
      
      if (input$accident_category == 'Fatalities'){
        crash_data %>% 
          filter(SUBURB %in% region_compare) %>%  
          group_by(ACCIDENT_YEAR, SUBURB) %>% 
          summarise(Total = sum(FATALITY)) %>% 
          ungroup()
      }
    }
    
  })
  
  
  # Creating a highcharter bar plot with an onclick functionality. 
  highchart_single <- reactive({
    
    hcClickFunction <- JS("function(event) {Shiny.onInputChange('hcClicked', event.point.category);}")
    
    plot1_data() %>%
      hchart('column', hcaes(x = ACCIDENT_YEAR, y = Total, color = ACCIDENT_YEAR)) %>% 
      hc_title(text = paste("Yearly trend for", input$accident_category),
               style = list(fontWeight = "bold", fontSize = "20px"),
               align = "center") %>% 
      hc_subtitle(text = input$region, 
                  style = list(fontWeight = "bold"),
                  align = "center") %>% 
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = input$accident_category)) %>% 
      hc_plotOptions(series = list(events = list(click = hcClickFunction)))
    
  })                  
  
  # Rendering the highchart plot created above.
  output$plot1 <- renderHighchart({
    
    highchart_single()
    
  })
  
  
  # Creating a reactive data for showing the top 10 accident prone regions 
  top10_data <- reactive({

    if (input$accident_category == 'Total Accidents') {
      crash_data %>%
        mutate(total = 1)
      }


    else if  (input$accident_category == 'Serious Injuries') {

      crash_data %>%
        filter(SERIOUSINJURY >= 1) %>%
        mutate(total = SERIOUSINJURY)

    }

    else {

      if (input$accident_category == 'Fatalities'){

        crash_data %>%
          filter(FATALITY >= 1) %>%
          mutate(total = FATALITY)}

    }

  })

  
  # Creating an observe event for the highcharter click which creates another highcharter bar plot showing the top 10 regions for the year clicked, in terms of the category selected.
  observeEvent(input$hcClicked, 
               {
    output$plot_top10 <- renderHighchart({
      
     data <-  top10_data() %>% 
        filter(ACCIDENT_YEAR == input$hcClicked) %>% 
        group_by(SUBURB) %>% 
        summarise(Total = sum(total)) %>% 
        arrange(desc(Total)) %>% 
        head(10)
      
      data %>% hchart(
        'bar', hcaes(x = SUBURB, y = Total, color = SUBURB), 
        pointWidth = 15
      ) %>% 
        hc_title(text = paste("Regions with topmost", input$accident_category, "in", input$hcClicked),
                 style = list(fontWeight = "bold", fontSize = "20px"),
                 align = "center") 
    })
    
    
  })
  
  # Creating an observe event which renders a new highchart with multiple groups once the user clicks the compare button. 
  observeEvent(input$compare, {

        output$plot1 <- renderHighchart({
          
          plot1compare_data() %>%
            hchart('column', hcaes(x = ACCIDENT_YEAR, y = Total, group = SUBURB)) %>%
            hc_colors(c("#0073C2FF", "#EFC000FF")) %>%
            hc_title(text = paste("Comparison of ", input$accident_category),
                     style = list(fontWeight = "bold", fontSize = "20px"),
                     align = "center") %>%
            hc_subtitle(text = paste(input$region, "vs", input$region1),
                        style = list(fontWeight = "bold"),
                        align = "center")
        })
        
  })
      
  
  # Creating an observe event which directs the user back to the single region highchart when the user clicks on the back button. 
  observeEvent(input$back,{
    
    output$plot1 <- renderHighchart({
      
      highchart_single()
      
    })
    
  })
  

  # Creating a reactive data which stores the choropleth map prepared in the preprocess file accoriding to the accident type selected from the radio button group. 
  choropleth <- reactive({
    
    if (input$accident_type == "Alcohol related"){
      
      choropleth_alcohol
      
    }else if (input$accident_type == "Hit and Run"){
      
      choropleth_hitrun
      
    }else if (input$accident_type == "Off Road"){
      
      choropleth_offroad
      
    }else {
      
      choropleth_accidents
      
    }
    
  })
  
  
  # Rendering the choropleth map
  output$plot2 <- renderLeaflet({
    
    choropleth()
    
  })
  

  # Creating an observe event which gets invoked when the user clicks on a particular region in the choropleth map and displays the driver/pedestrian population wise breakdown of the region using a radial pie chart from highcharter. 
  observe(
    {  
      click = input$plot2_shape_click
      if(is.null(click))
        return()
      
      if (input$accident_type == "Alcohol related"){
        
        pop_data <- crash_data %>% 
          filter(ALCOHOL_RELATED == 'Yes')
        
      }else if (input$accident_type == "Hit and Run"){
        
        pop_data <- crash_data %>% 
          filter(HIT_RUN_FLAG == 'Yes')
        
      }else if (input$accident_type == "Off Road"){
        
        pop_data <- crash_data %>% 
          filter(RUN_OFFROAD == 'Yes')
        
      }else {
        
        pop_data <- crash_data
      }
      
      # Creating a dataframe that counts the drivers/pedestrains affected for selected region and accident type. 
      population_data <- pop_data %>% 
        filter(SUBURB == click$id) %>% 
        group_by(SUBURB) %>% 
        summarise(young_drivers = sum(YOUNG_DRIVER), 
                  old_drivers = sum(OLD_DRIVER), 
                  pedestrians = sum(PEDESTRIAN),
                  motorists = sum(MOTORIST))
      
      # Converting data from a wide form to a long form using gather.
      population_data <- gather(population_data, population, number, young_drivers:motorists, factor_key=TRUE)
      
      # Rendering a radial pie chart using highcharter for all population categories
      output$plot3 <- renderHighchart({
        
        highchart() %>%
          hc_chart(type = "column", polar = TRUE) %>% 
          hc_xAxis(categories = population_data$population) %>% 
          hc_title(text = paste("Population wise breakdown for ", click$id),
                   style = list(fontWeight = "bold", fontSize = "15px"),
                   align = "center") %>% 
          hc_series(list(
            name = "Accidents",
            data = population_data$number,
            colorByPoint = TRUE,
            type = "column",
            colors = c("#d35400", "#2980b9", "#2ecc71", "#f1c40f", "#2c3e50", "#d3d3d3"),
            showInLegend = FALSE
          )
          )
        
      })
      
    })
  
  
  # Creating a reactive data for the heatmap which changes according to the accident_heat radio button group input. 
  plot4_data <- reactive({
    
    if (input$accident_heat == "Alcohol related"){
      
      heatmap_data %>% 
        
        select(hour = ACCIDENT_HOUR, day = DAY_OF_WEEK, Total = alcohol_accidents) %>% 
        mutate(color = "green")
      
    }else if (input$accident_heat == "Hit and Run"){
      
      heatmap_data %>% 
        select(hour = ACCIDENT_HOUR, day = DAY_OF_WEEK, Total = hit_run) %>% 
        mutate(color = "blue")
      
    }else if (input$accident_heat == "Off Road"){
      
      heatmap_data %>% 
        select(hour = ACCIDENT_HOUR, day = DAY_OF_WEEK, Total = off_road) %>% 
        mutate(color = "")
      
    }else if (input$accident_heat == "Total Accidents") {
      
      heatmap_data %>% 
        select(hour = ACCIDENT_HOUR, day = DAY_OF_WEEK, Total = accidents)
      
    }
    
    
  })
  
  # Rendering a heatmap from the data  created above using highcharter. 
  output$plot4 <- renderHighchart({
    
    #hcdaytimeFunction <- JS("function(event) {Shiny.onInputChange('hcClicked', c(event.point.x, event.point.y));}")
    
    hchart(plot4_data(),
           type = "heatmap", 
           hcaes(x = hour, y = day, value = Total),
           events = list(
             click = JS("function(event) {
                      
                         var xstr = event.point.series.xAxis.categories[event.point.x].toString();
                         var ystr = event.point.series.yAxis.categories[event.point.y].toString();
  
                         var data = {
                            x: xstr,
                            y: ystr,
                            nonce: Math.random()
                          };
  
                         Shiny.onInputChange('matrix_click', data);

                       }"))) %>% 
      hc_title(text = paste("Time of Day vs Day of the Week"),
               style = list(fontWeight = "bold", fontSize = "15px"),
               align = "center")
    
  })
  
  observeEvent(input$matrix_click, {
    
    filter_data <- crash_data %>% 
      filter(ACCIDENT_HOUR == input$matrix_click$x) %>% 
      filter(DAY_OF_WEEK == input$matrix_click$y) 
    
    speed_data <- reactive({
      
      if (input$accident_heat == "Alcohol related"){
        
        filter_data %>% 
          filter(ALCOHOL_RELATED == 'Yes')
          
      }else if (input$accident_heat == "Hit and Run"){
        
       filter_data %>% 
          filter(HIT_RUN_FLAG == 'Yes')
          
      }else if (input$accident_heat == "Off Road"){
        
        filter_data %>% 
          filter(RUN_OFFROAD == 'Yes')
          
      }else if (input$accident_heat == "Total Accidents") {
        
        filter_data
      }
    })
    
    factor_data <- reactive({
      
      if (input$condition == 'Speed Limit'){
        
        speed_data() %>% 
          group_by(SPEED_ZONE) %>% 
          summarise(Total = n()) %>% 
          mutate(label = SPEED_ZONE)
        
      } else if (input$condition == 'Lighting Conditions'){
        
        speed_data() %>% 
          group_by(LIGHT_CONDITION) %>% 
          summarise(Total = n()) %>% 
          mutate(label = LIGHT_CONDITION)
        
      } else 
      {
        speed_data() %>% 
          group_by(ACCIDENT_TYPE) %>% 
          summarise(Total = n()) %>% 
          mutate(label = ACCIDENT_TYPE)
      }
    })
    
    output$factor_donut <- renderPlotly(
      
      factor_data() %>%
        plot_ly(labels = ~label, values = ~Total) %>% 
        add_pie(hole = 0.6) %>% 
        layout(title = paste('Categorizing', input$condition, 'for', input$accident_heat, '(Time:', input$matrix_click$x, 'Day:', input$matrix_click$y, ')'),  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               fig_bgcolor   = "rgba(0, 0, 0, 0)"
               )
      
    ) 
    
  })
  
  
  # Creating output info boxes for naviagtion
  output$tab1 <- renderInfoBox({
    infoBox(
      "Regional Trends",
      a(
        "If you are an offical who wants to see how well the regions in Victoria have been able to control the accidents and compare regional trends, click this option!",
        onclick = "openTab('regional')",
        href = "#"
      ),
      color = "green",
      width = 12
    )
  })
  
  output$tab2 <- renderInfoBox({
    infoBox(
      "Population analysis",
      a(
        "If you are a driver/pedestrian who wants to see how your population is involved in accidents in a particular region, click this option!",
        onclick = "openTab('population')",
        href = "#"
      ),
      color = "blue"
    )
  })
  
  output$tab3 <- renderInfoBox({
    infoBox(
      "Day/Time Analysis",
      a(
        "Click this option if you are a commuter who would like to analyse what time and day are prone to what kind of accidents!",
        onclick = "openTab('daytime')",
        href = "#"
      ),
      color = "blue"
    )
  })
  
  output$tab4 <- renderInfoBox({
    infoBox(
      "Introduction",
      a(
        "Click to move back to Introduction",
        onclick = "openTab('introduction')",
        href = "#"
      ),
      color = "blue"
    )
  })
  
  output$tab5 <- renderInfoBox({
    infoBox(
      "Introduction",
      a(
        "Click to move back to Introduction",
        onclick = "openTab('introduction')",
        href = "#"
      ),
      color = "blue"
    )
  })
  
  output$tab6 <- renderInfoBox({
    infoBox(
      "Introduction",
      a(
        "Click to move back to Introduction",
        onclick = "openTab('introduction')",
        href = "#"
      ),
      color = "blue",
      width = 12
    )})
  
}


# Running my Shiny Server and Shiny UI
shinyApp(ui = shinyUI, server = shinyServer) 





