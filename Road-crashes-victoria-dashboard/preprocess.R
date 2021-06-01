# Student Name: Suvansh Vaid
# Student Id: 31182968
# Lab: 02-P1
# Tutor: Xiaojiao Du
# Description - This file contains all the pre processing data related to the narrative visualization project. The original data Crashes_Last_Five_Years.csv is first cleaned and another column named SUBURB is added which contains the LGA name as in the shape file.

# Note that the initial data was transformed using a Python script to extract the LGAs for each accident and store in a new column named 'SUBURB'. This was quite a challenging task as it involved using the geopandas functionlities in depth and also took a long time to run. 

# Loading clean data after merging with the spatial data
crash_data <- read_csv('crash_data.csv')

# Reading Shape file
vic_regions <- readOGR('vic_lga_polygon_shp_gda2020/VIC_LGA_POLYGON_SHP_GDA2020.shp')


vic_regions <- spTransform(vic_regions, CRS("+proj=longlat +ellps=GRS80"))


# Removing unwanted attributes from the data
crash_data <-  subset(crash_data, select = -c(ACCIDENT_STATUS, NODE_ID, VICGRID_X, VICGRID_Y, UNKNOWN, SRNS_ALL, RMA_ALL, DIVIDED_ALL, DEG_URBAN_ALL, REGION_NAME_ALL, LGA_NAME_ALL))


# Extracting the hour of accident time 
crash_data <- crash_data %>%
  separate(ACCIDENT_TIME,
           c("ACCIDENT_HOUR"),
           extra='drop',
           remove = FALSE)

crash_data$ACCIDENT_HOUR <- as.integer(crash_data$ACCIDENT_HOUR) 


# Creating a new column for year 
crash_data <- crash_data %>% 
  mutate(ACCIDENT_YEAR = substr(ACCIDENT_DATE, nchar(ACCIDENT_DATE)-3, nchar(ACCIDENT_DATE)))

crash_data$ACCIDENT_YEAR <- as.numeric(crash_data$ACCIDENT_YEAR)

# Removing the 2019 accidents from the analysis
crash_data <- crash_data %>% 
  filter (ACCIDENT_YEAR < 2019)

years <- unique(crash_data$ACCIDENT_YEAR)

# Creating a list of all the LGAs
regions <- unique(crash_data$SUBURB)

# -------------- Preparing data for the Choropleth map (Tab 2) --------------#

crash_data$ABB_NAME <- crash_data$SUBURB

# Creating a data frame for counting accidents by categories for each region 
map_data <- crash_data %>% 
  group_by(ABB_NAME) %>% 
  summarise(alcohol = sum(ALCOHOL_RELATED == 'Yes'), hit_run = sum(HIT_RUN_FLAG == 'Yes'), off_road = sum(RUN_OFFROAD == 'Yes'), accidents = n()) %>% 
  ungroup()


# Merging the shape file data with the map data created above
vic_regions@data <- inner_join(vic_regions@data, map_data, by = 'ABB_NAME')

# Replacing the 0 values in each category to NA for easier choropleth interpretation
vic_regions@data$alcohol[ which(vic_regions@data$alcohol == 0)] = NA
vic_regions@data$hit_run[ which(vic_regions@data$hit_run == 0)] = NA
vic_regions@data$off_road[ which(vic_regions@data$off_road == 0)] = NA

# Creating color palettes for each category
palette_accidents <- colorNumeric(palette= "Reds", domain= vic_regions@data$accidents, na.color="white")
palette_hitrun <- colorNumeric(palette= "Greens", domain= vic_regions@data$hit_run, na.color="white")
palette_offroad <- colorNumeric(palette= "Blues", domain= vic_regions@data$off_road, na.color="white")
palette_alcohol <- colorNumeric(palette= "Purples", domain= vic_regions@data$alcohol, na.color="white")

# Creating tooltip text content
mytext <- paste(
  "LGA: ", vic_regions@data$ABB_NAME,"<br/>", 
  "Total Accidents: ", vic_regions@data$accidents,"<br/>", 
  "Alcohol related accidents: ", vic_regions@data$alcohol,"<br/>",
  "Hit and run accidents: ", vic_regions@data$hit_run,"<br/>",
  "Off road crashes: ", vic_regions@data$off_road,"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

# Creating a uniform title template
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 40%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 15px;
  }
"))

# Creating title texts for different accident types
title_accidents <- tags$div(
  tag.map.title, HTML("All the accidents across Victoria")
)  

title_alcohol <- tags$div(
  tag.map.title, HTML("Alcohol related accidents across Victoria")
)  

title_hitrun <- tags$div(
  tag.map.title, HTML("Hit and Run accidents across Victoria")
)  

title_offroad <- tags$div(
  tag.map.title, HTML("Off road accidents across Victoria")
)  


# Storing choropleth leaflet plots for each category 
choropleth_accidents <- leaflet(vic_regions) %>%
                            addTiles(group = "OSM (default)") %>%
                            addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
                            addProviderTiles("Stamen.Watercolor", group = "Water Color") %>%
                            setView(lng = 145.02,lat = -36.4713, zoom = 6) %>%
                            addEasyButton(easyButton(
                              icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
                              onClick = JS("function(btn, map){ map.setView([-36.4713, 145.02], 6); }"))) %>%
                            addPolygons(fillColor = ~palette_accidents(accidents),
                                        weight = 1,
                                        smoothFactor = 0.2,
                                        fillOpacity = 1,
                                        highlightOptions = highlightOptions(color = "white",
                                                                            weight = 2,
                                                                            bringToFront = TRUE),
                                        label = mytext,
                                        labelOptions = labelOptions(
                                          style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "11px",
                                          direction = "auto"
                                        ),
                                        layerId = ~ABB_NAME
                            ) %>% 
  addControl(title_accidents, position = "topleft", className="map-title") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner Lite", "Water Color"),
    options = layersControlOptions(collapsed = FALSE)
  )

choropleth_hitrun <- leaflet(vic_regions) %>%
  addProviderTiles("Stamen.Watercolor") %>%
  setView(lng = 145.02,lat = -36.4713, zoom = 6) %>%
  addEasyButton(easyButton(
    icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
    onClick = JS("function(btn, map){ map.setView([-36.4713, 145.02], 6); }"))) %>%
  addPolygons(fillColor = ~palette_hitrun(hit_run),
              weight = 1,
              smoothFactor = 0.2,
              fillOpacity = 1,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              label = mytext,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "11px",
                direction = "auto"
              ),
              layerId = ~ABB_NAME
  ) %>% 
  addControl(title_hitrun, position = "topleft", className="map-title") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner Lite", "Water Color"),
    options = layersControlOptions(collapsed = FALSE)
  )
  

choropleth_offroad <- leaflet(vic_regions) %>%
  addProviderTiles("Stamen.Watercolor") %>%
  setView(lng = 145.02,lat = -36.4713, zoom = 6) %>%
  addEasyButton(easyButton(
    icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
    onClick = JS("function(btn, map){ map.setView([-36.4713, 145.02], 6); }"))) %>%
  addPolygons(fillColor = ~palette_offroad(off_road),
              weight = 1,
              smoothFactor = 0.2,
              fillOpacity = 1,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              label = mytext,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "11px",
                direction = "auto"
              ),
              layerId = ~ABB_NAME
  ) %>% 
  addControl(title_offroad, position = "topleft", className="map-title") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner Lite", "Water Color"),
    options = layersControlOptions(collapsed = FALSE)
  )

choropleth_alcohol <- leaflet(vic_regions) %>%
  addProviderTiles("Stamen.Watercolor") %>%
  setView(lng = 145.02,lat = -36.4713, zoom = 6) %>%
  addEasyButton(easyButton(
    icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
    onClick = JS("function(btn, map){ map.setView([-36.4713, 145.02], 6); }"))) %>%
  addPolygons(fillColor = ~palette_alcohol(alcohol),
              weight = 1,
              smoothFactor = 0.2,
              fillOpacity = 1,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              label = mytext,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "11px",
                direction = "auto"
              ),
              layerId = ~ABB_NAME
  ) %>% 
  addControl(title_alcohol, position = "topleft", className="map-title") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner Lite", "Water Color"),
    options = layersControlOptions(collapsed = FALSE)
  )

#---------------- Preparing data for the Heat map (Tab3) ----------------------#

# Creating a new data frame for heatmap 
heatmap_data <- crash_data %>% 
  group_by(ACCIDENT_HOUR, DAY_OF_WEEK) %>% 
  summarise(accidents = n(), 
            alcohol_accidents = sum(ALCOHOL_RELATED == 'Yes'),
            hit_run = sum(HIT_RUN_FLAG == 'Yes'), 
            off_road = sum(RUN_OFFROAD == 'Yes'))

# Converting hour and week into factors for maintaining order
heatmap_data$ACCIDENT_HOUR <- as.factor(heatmap_data$ACCIDENT_HOUR)

heatmap_data$DAY_OF_WEEK <- as.factor(heatmap_data$DAY_OF_WEEK)

levels(heatmap_data$DAY_OF_WEEK) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")





