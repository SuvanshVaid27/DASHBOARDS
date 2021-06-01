server <- function(input, output) {
  
  # output$plot_cumulative <- renderPlot({
  #   ggplot(confirmed_aus, mapping = aes(x = Date, y = count)) + 
  #     geom_bar(stat = "identity", aes(fill = State), width = 1) +  
  #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #           panel.background = element_blank(), axis.line = element_line(colour = "black"))
  # })
  
  output$plot_cumulative <- renderHighchart(
    confirmed_aus %>% 
      hchart('areaspline', hcaes(x = Date, y = count, group = State))
  )

  
  output$last_updated <- renderText({
    paste("Last Updated: ", format(Sys.Date()-1,"%m/%d/%Y"))
  })
  
  output$Confirmed <- renderValueBox({
    valueBox(
      total_confirmed, 
      subtitle = "Confirmed",
      color = "aqua",
      icon = icon("file-medical")
    )
  })
  
  output$Recovered <- renderValueBox({
    valueBox(
      total_recovered, 
      subtitle = "Recovered",
      color = "green",
      icon = icon("heart")
    )
  })
  
  output$Deaths <- renderValueBox({
    valueBox(
      total_deaths, 
      subtitle = "Deaths",
      color = "red",
      icon = icon("heartbeat")
    )
  })
  
  output$Active <- renderValueBox({
    valueBox(
      total_active, 
      subtitle = "Active",
      color = "yellow",
      icon = icon("refresh")
    )
  })
  
  
  confirmed_state <- reactive({
     confirmed_aus <- confirmed_aus %>% filter(State == input$province1)
  })
  
  recovered_state <- reactive({
    recovered_aus <- recovered_aus %>% filter(State == input$province1)
  })
  
  deaths_state <- reactive({
    deaths_aus <- deaths_aus %>% filter(State == input$province1)
  })
  
  data2 <- reactive({
    confirmed_aus <- confirmed_aus %>% filter(State == input$province2)
  })
  
  addLabel <- function(map_data) {
    map_data$label <- paste0(
      '<b>', map_data$State, '</b><br>
    <table style="width:150px;">
    <tr><td>Confirmed:</td><td align="right">', map_data$Confirmed, '</td></tr>
    <tr><td>Deceased:</td><td align="right">', map_data$Deaths, '</td></tr>
    <tr><td>Estimated Recoveries:</td><td align="right">', map_data$Recovered, '</td></tr>
    <tr><td>Increased Cases:</td><td align="right">', map_data$Confirmed_new, '</td></tr>
    </table>'
    )
    map_data$label <- lapply(map_data$label, HTML)
    
    return(map_data)
  }
  
  map <- leaflet(addLabel(final_data), options = leafletOptions(minZoom = 3)) %>%
    setMaxBounds(100.427001,-30.145127,158.825343, -28.921631) %>%
    setView(134.489563,-25.734968, zoom = 3) %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
    addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
    addLayersControl(
      baseGroups    = c("Light", "Satellite")
    ) %>%
    addEasyButton(easyButton(
      icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
      onClick = JS("function(btn, map){ map.setView([134.489563,-25.734968], 3); }"))) %>%
    addCircleMarkers(lng = ~Long, lat = ~Lat, label = ~label, radius = ~log(Confirmed))
  
  output$overview_map <- renderLeaflet(map)
  

  output$plot_confirmed <- renderPlotly({
    ggplotly(
  
        ggplot(data = confirmed_aus) +
        geom_smooth(mapping = aes(x = Date, y = count, color = State), se = FALSE) +  
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
    )
    
  })
  
  
  output$plot_recovered <- renderPlotly({
    
    ggplotly(
      
        ggplot(data = recovered_aus) + 
        geom_smooth(mapping = aes(x = Date, y = count, color = State), se = FALSE) +  
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    )
  })
  
  output$plot_deaths <- renderPlotly({
    
    ggplotly(
      
      ggplot(data = deaths_aus) +
        geom_smooth(mapping = aes(x = Date, y = count, color = State), se = FALSE) +  
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    )
  })
  
  output$overview <- renderText({
    
    paste("Coronavirus disease 2019 (COVID-19) is a contagious disease caused by severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2). The first case was identified in Wuhan, China, in December 2019. It has since spread worldwide, leading to an ongoing pandemic.", "This dashboard is a created to enlighten the Australian citizens of the daily covid -19 cases and how effective various states have been in controlling the spread of this disease.")
  })
  
  output$aboutus <- renderText({
    
    paste("We are a team of data scientists and economists studying a Master's degree at Monash University. We love analysing data and creating dashboards in R shiny.", "We will be soon updating this dashboard with more statistical analysis of how the covid-19 has affected the Australian economy.")
    
  })
  
}
  