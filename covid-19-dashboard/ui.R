
ui <- dashboardPage(
  dashboardHeader(title = "Covid-19 Australia"),
  dashboardSidebar( sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("About", tabName = "about", icon = icon("th"))
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                width = 12, 
                column(
                valueBoxOutput("Confirmed", width = 3),
                valueBoxOutput("Recovered", width = 3),
                valueBoxOutput("Deaths", width = 3),
                valueBoxOutput("Active", width = 3),
                style = "margin-left: -10px",
                width = 12
              ),
              div("  Last Updated: ", Sys.Date() - 1),
              div("  Data Source: John Hopkins Univesity"))),
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(
                  title = "Cumulative cases in Australia",
                  highchartOutput("plot_cumulative")
                ),
                box(
                  title = "Map of cases",
                  leafletOutput("overview_map")
                )),
              fluidRow(
                box(
                  title = "State wise Figures",
                  width = 12, 
                mainPanel(
                  width = 12,
                  tabsetPanel(type = "tabs",
                              tabPanel("Confirmed Cases", plotlyOutput("plot_confirmed")),
                              tabPanel("Recovered Cases", plotlyOutput("plot_recovered")),
                              tabPanel("Deaths", plotlyOutput("plot_deaths"))
                  )
                )
              )
            )
      ),
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12,
            title = "Overview",
            textOutput("overview")
          )),
          fluidRow(
            box(
              width = 12,
              title = "About us",
              textOutput("aboutus")
            )
          )
        )
      )
)
)

