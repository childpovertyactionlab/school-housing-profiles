##### Libraries #####
library(shiny)
library(tidyverse)
library(cpaltemplates)
library(sf)
library(leaflet)
library(DT)
library(highcharter)

##### Data #####
#import data for each campus
wd <- "/Users/erose/CPAL Dropbox/Housing/04_Projects/School Housing Profiles/shiny/disd-profiles/"
#wd <- "C:/Users/Michael/CPAL Dropbox/Housing/04_Projects/School Housing Profiles/shiny/disd-profiles/"
# wd <- ""

disdevictions <- rio::import(paste0(wd, "data/DISD Evictions by Boundary.csv")) %>%
  filter(SchoolYear != "2024-2025")
disdprofiles <- rio::import(paste0(wd, "data/DISD Demographic Campus Profiles.csv"))
disdpoints <- st_read(paste0(wd, "data/DISD Campus Point Locations.geojson")) %>%
  st_transform(crs = 4326)
disdfeeder <- st_read(paste0(wd, "data/DISD Campus Feeder Patterns.geojson")) %>%
  st_transform(crs = 4326)
disdloc <- st_read(paste0(wd, 'data/Campus_Locations/Campus_Locations.shp'))%>%
  st_transform(crs = 4326)
hs_moy <- readxl::read_xlsx(paste0(wd, "data/2023-2024 MOYM Analysis.xlsx"), sheet = 6)%>%
  filter(!is.na(Campus))%>%
  rename("moymPerGrkt5_24" = "% MOY Moves (Grades Kn-5)",
         "moymGrkt5_24" = '# MOY Moves (Grades Kn-5)',
         "name"= "Campus")%>%
  mutate(name = paste0(name, "  High"))
hs_moy <- hs_moy[-1,]
disdprofiles <- disdprofiles %>%
  left_join(hs_moy, by = "name") %>%
  mutate(
    moymGrkt5_24 = ifelse(is.na(moymGrkt5_24.x), moymGrkt5_24.y, moymGrkt5_24.x), 
    moymPerGrkt5_24 = ifelse(is.na(moymPerGrkt5_24.x), moymPerGrkt5_24.y, moymPerGrkt5_24.x)
  ) %>%
  select(-moymGrkt5_24.x, -moymGrkt5_24.y, -moymPerGrkt5_24.x, -moymPerGrkt5_24.y)
hs_moy23 <- readxl::read_xlsx(paste0(wd, "data/2022-2023 MOYM Analysis.xlsx"), sheet = 2)%>%
  rename("moymPerGrkt5_23" = "% MOY Moves (Grades Kn-5)",
         "moymGrkt5_23" = '# MOY Moves (Grades Kn-5)',
         "name"= "HIGH")%>%
  mutate(name = paste0(name, "  High"))
disdprofiles <- disdprofiles %>%
  left_join(hs_moy23, by = "name") %>%
  mutate(
    moymGrkt5_23 = ifelse(is.na(moymGrkt5_23.x), moymGrkt5_23.y, moymGrkt5_23.x),  
    moymPerGrkt5_23 = ifelse(is.na(moymPerGrkt5_23.x), moymPerGrkt5_23.y, moymPerGrkt5_23.x)
  ) %>%
  select(-moymGrkt5_23.x, -moymGrkt5_23.y, -moymPerGrkt5_23.x, -moymPerGrkt5_23.y)

title <- tags$a(
  div(
    tags$img(src = "images/CPAL_Logo_White.png",
             height = "30", style = "vertical-align: middle; margin-right: 40px;"), # Add margin to the right of the image
    strong("Campus Housing Profiles", style = "font-weight: bold; vertical-align: middle;"), # Ensure text is bold
    style = "display: flex; align-items: center;" # Use flexbox to align items centrally
  )
)

#### UI #####
ui <- navbarPage(
  title = span(title, style = "display: inline-block;"), # Ensure title is inline to allow other elements next to it
  theme = cpaltemplates::cpal_shiny(),
  
  tags$head(tags$style(
    HTML(
      "
            .dataTables_wrapper {
                border: 4px solid #042d33; /* Example color for $teal-900 */
                border-radius: 10px 10px 5px 5px;
            }

            .dataTable {
                overflow: hidden;
            }

            .dataTable .last-row {
                background-color: #004D40;
            }

            .dataTable tbody tr:hover {
                background-color: rgba(4, 45, 51, 0.1);
            }

            .dataTable thead th:hover {
                color: #FFF;
                cursor: pointer;
            }

            .dataTable th,
            .dataTable td {
                font-family: 'Poppins', sans-serif;
                text-align: left;
            }

            .dataTable td {
                font-size: 15px;
                padding-left: 15px;
            }

            .dataTable th {
                background-color: #042d33;
                color: #FFFFFF;
                line-height: 1.2;
                font-style: normal;
                font-variant: small-caps;
            }
            "
    )
  )),
  
  # Main layout with sidebar and main panel
  tabPanel(
    "DISD",
    sidebarLayout(
      position = "left",
      
      # Sidebar panel contents
      sidebarPanel(
        width = 5,
        h3(strong("Campus Attendance Zone")),
        radioButtons(
          inputId = "schoolType",
          label = strong("Choose a School Type"),
          choices = c("Elementary School", "Middle School", "High School"),
          selected = "Elementary School",
          inline = TRUE
        ),
        selectInput(
          inputId = "campus",
          label = strong("Choose a Dallas ISD Campus"),
          choices = NULL
        ),
        # Choices will be populated from the server
        leafletOutput("map"),
        br(),
        h3(strong("Campus Housing Info")),
        p(
          "The following information includes data on all residents within a school attendance boundary."
        ),
        dataTableOutput("table"),
        br(),
        conditionalPanel( 
          condition = "input.schoolType == 'High School'", 
          p(style = "font-size: 12px; color: gray;", 
          "*NOTE: MOYM data for high schools reflects the elementary schools within that high school's attendance zone."
          )
          ),
        p(style = "font-size: 12px; color: gray;", 
          "*NOTE: Market Asking Rent data is sourced from CoStar and averages the two bedroom asking rent price for large multi-family apartment complexes."
        ),
        p(style = "font-size: 12px; color: gray;", 
          "*NOTE: This tool is designed to present a range of housing-related metrics specific to each campus attendance zone within the Dallas Independent School District (DISD). Utilizing the comprehensive data from the 2022 5-Year American Community Survey, this tool offers users a detailed visualization of various key metrics. These metrics have been sourced from the American Community Survey, Dallas County, and CoStar, and have been estimated across attendance zones by the Child Poverty Action Lab (CPAL) to accurately reflect the unique characteristics of each attendance zone."
        )
        
      ),
      
      # Main panel contents
      mainPanel(
        width = 7,
        selectInput(
          inputId = "yearType",
          label = strong("Choose Year Type"),
          choices = c("School Year" = "School Year", "Calendar Year" = "Calendar Year"),
          selected = "school"
        ),
        highchartOutput("barChart"),
        br(),
        highchartOutput("lineChart"),
        br(),
        highchartOutput("amtChart"),
      )
    )
  )
)

##### Server #####
server <- function(input, output, session) {
  
  # Create a modal dialog
  myModal <- modalDialog(
    title = "About this Tool",
    htmltools::HTML(paste0("Welcome to the Dallas ISD Housing Profiles Tool! <br><br>",
                           "This tool is designed to present a range of housing-related metrics specific to each campus attendance zone within the Dallas Independent School District (DISD). Utilizing the comprehensive data from the 2022 5-Year American Community Survey, this tool offers users a detailed visualization of various key metrics. These metrics have been sourced from the American Community Survey, Dallas County, and CoStar, and have been estimated across attendance zones by the Child Poverty Action Lab (CPAL) to accurately reflect the unique characteristics of each attendance zone.<br><br>",
                           "One of the notable features of this tool is its ability to present annual figures aligned with school years rather than calendar years, providing a more relevant and context-specific understanding of the data. Included in this tool is eviction filing rate, calculated as the number of evictions filed in a school attendance zone during a school year per 1,000 renter-occupied households. This rate offers valuable insights into the housing stability and challenges faced within each zone.<br><br>",
                           "Property data sourced from CoStar may be incomplete and only includes large multi-family apartment complexes within attendance zone.<br><br>",
                           "Overall, the Dallas ISD Campus Housing Profiles tool is a strong resource for those seeking to understand the housing landscape within specific Dallas ISD attendance zones.<br><br>", "This tool was last updated <strong>February 18th, 2025</strong>.")),
    easyClose = TRUE,
    size = "l",
    footer = tagList(modalButton("Dismiss"))
  )
  
  # Show the modal when the app starts
  observe({
    showModal(myModal)
  })
  
  observeEvent(input$schoolType, {
    # Filter the campuses based on the selected school type
    filtered_campuses <- disdpoints %>%
      filter(schoolType == input$schoolType) %>%
      arrange(name)
    
    # Update the campus select input
    updateSelectInput(session, "campus", choices = unique(filtered_campuses$name))
  })
  
  # generate filtered datasets for data views
  reactive_disdfeeder <- reactive({
    req(input$campus)  # Ensure campus input is available
    filter(disdfeeder, name == input$campus)
  })
  
  reactive_disdevictions <- reactive({
    req(input$campus, input$yearType)  # Ensure inputs are available
    data <- disdevictions %>% filter(name == input$campus | name == "Dallas ISD")
    
    if (input$yearType == "Calendar Year") {
      data <- data %>% mutate(chartYear = Year)  
    } else {
      data <- data %>% mutate(chartYear = SchoolYear)  
    }
    
    data
  })
  
  
  reactive_disdprofiles <- reactive({
    req(input$campus)  # Ensure campus input is available
    filter(disdprofiles, name == input$campus)
  })
  
  
  # School Housing Metric Table
  output$table <- renderDataTable({
      disd_tbl <- reactive_disdprofiles() %>%
        mutate(moymPerGrkt5_24 = as.numeric(moymPerGrkt5_24)) %>% 
        transmute(
          SLN = SLN,          
          School = name,
          'Total Population' = prettyNum(round(tot_pop, digits = 0), big.mark = ",", format = "d"),
          'Percent Below Poverty' = paste0(round(bp_pop * 100, digits = 1), "%"),
          'Children Below Poverty' = prettyNum(round(bp_u18, digits = 0), big.mark = ",", format = "d"),
          'Renter-Occupied Households' = prettyNum(round(rohh, digits = 0), big.mark = ",", format = "d"),
          'Median Household Income' = paste0("$", prettyNum(round(mhi, digits = 0), big.mark = ",", format = "d")),
          'Multi-Family Complexes' = tot_mf,
          'Multi-Family Units' = tot_units,
          'Market Asking Rent' = paste0("$", prettyNum(round(MAR, digits = 0), big.mark = ",", format = "d")),
          'Est Income to Afford Market Asking Rent' = paste0("$", prettyNum(round(IncomeNeed, digits = 0), big.mark = ",", format = "d")),
          'Middle of Year Moves K-5 (2022-2023)' = ifelse(
            grepl("High", name, ignore.case = TRUE), # Check if "High" is in the name
            paste0(ifelse(is.na(moymPerGrkt5_23), NA, paste0(moymGrkt5_23, " (", round(moymPerGrkt5_23 * 100, digits = 1), "%)")), "*"), # Add asterisk for high schools
            ifelse(is.na(moymPerGrkt5_23), NA, paste0(moymGrkt5_23, " (", round(moymPerGrkt5_23 * 100, digits = 1), "%)")) # No asterisk for others
          ),
          'Middle of Year Moves K-5 (2023-2024)' = ifelse(
            grepl("High", name, ignore.case = TRUE),
            paste0(ifelse(is.na(moymPerGrkt5_24), NA, paste0(moymGrkt5_24, " (", round(moymPerGrkt5_24 * 100, digits = 1), "%)")), "*"),
            ifelse(is.na(moymPerGrkt5_24), NA, paste0(moymGrkt5_24, " (", round(moymPerGrkt5_24 * 100, digits = 1), "%)"))
          )
        ) %>%
        t() %>%
        as.data.frame() %>%
        rename(Metric = V1)
      
      # Create datatable with formatted column and removed features
      datatable(disd_tbl, options = list(pageLength = 10, 
                                         searching = FALSE,
                                         lengthChange = FALSE,
                                         info = FALSE,
                                         paging = FALSE), 
                rownames = TRUE
      )
  })
  
  # Total Evictions by School Year Line Graph
  output$barChart <- renderHighchart({
    
    # Assuming 'disdevictions' is your data frame
    data_to_plot <- reactive_disdevictions() %>%
      filter(name != "Dallas ISD")
    
    hchart(data_to_plot, type = "column", hcaes(x = chartYear, y = TotEvictions)) %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = TRUE))) %>%
      hc_xAxis(title = list(text = ""), categories = data_to_plot$chartYear) %>%
      hc_yAxis(title = list(text = "")) %>%
      hc_title(text = paste0(input$campus, " Attendance Zone<br><sup>Total Eviction Filings by ", input$yearType, "</sup>")) %>%
      hc_tooltip(headerFormat = "<span style=\"font-size: 10px\">{point.key}</span><br/>",
                 pointFormat = "Total Evictions: {point.y}") %>%
      hc_colors(palette_cpal_main)
  })
    
  # Eviction Filing Rate by School Year Bar Graph
  output$lineChart <- renderHighchart({
    
    # Assuming 'disdevictions' is your data frame
    data_to_plot <- reactive_disdevictions() %>%
      filter(name == input$campus | name == "Dallas ISD") %>%
      mutate(name = relevel(as.factor(name), "Dallas ISD"))
    
    hchart(data_to_plot, "line", hcaes(x = chartYear, y = evicRate, group = name)) %>%
      hc_xAxis(categories = unique(data_to_plot$chartYear)) %>%
      hc_yAxis(title = list(text = "")) %>%
      hc_title(text = paste0(input$campus, " Attendance Zone<br><sup>Eviction Filing Rate by ", input$yearType, "</sup>")) %>%
      hc_tooltip(pointFormat = "Eviction Filing Rate: {point.y:.2f}") %>%
      hc_colors(palette_cpal_main) %>%
      hc_legend(align = 'center', verticalAlign = 'bottom', layout = 'horizontal') %>%
      hc_plotOptions(series = list(
        lineWidth = 4,  # Setting the line width to twice the default size
        marker = list(
          enabled = TRUE,
          symbol = "circle",  # Ensuring markers are circles
          radius = 6,         # Adjust the size of the marker if needed
          fillColor = NULL,   # Setting the color of markers to match the line
          lineWidth = 2,
          lineColor = NULL    # Line color of marker (border) to match the line
        )
      ))
  })
  
  # Total Evictions by School Year Line Graph
  output$amtChart <- renderHighchart({
    
    # Assuming 'disdevictions' is your data frame
    data_to_plot <- reactive_disdevictions() %>%
      filter(name != "Dallas ISD")
    
    hchart(data_to_plot, type = "column", hcaes(x = chartYear, y = EvicFilAmt)) %>%
      hc_plotOptions(column = list(
        dataLabels = list(
          enabled = TRUE,
          formatter = JS("function() { return '$' + Highcharts.numberFormat(this.y, 2); }")
        )
      )) %>%
      hc_xAxis(title = list(text = ""), categories = data_to_plot$SchoolYear) %>%
      hc_yAxis(title = list(text = ""),
               labels = list(formatter = JS("function() { return '$' + Highcharts.numberFormat(this.value, 2); }"))) %>%
      hc_title(text = paste0(input$campus, " Attendance Zone<br><sup>Average Eviction Filing Amount by ", input$yearType, "</sup>")) %>%
      hc_tooltip(headerFormat = "<span style=\"font-size: 10px\">{point.key}</span><br/>",
                 pointFormat = "Total Evictions: ${point.y:.2f}") %>%
      hc_colors(palette_cpal_main)
  })
  
  # School Feeder Pattern Leaflet Map
  # Generate bounding box for map render
  output$map <- renderLeaflet({
    bbox <- reactive_disdfeeder() %>%
      st_bbox(.) %>%
      as.vector()
    disdfeeder$SLN <- as.character(disdfeeder$SLN)
    disdloc$SLN <- as.character(disdloc$SLN)
    
    feeder_data <- reactive_disdfeeder()
    
    feeder_schools <- disdloc %>%
      filter(!LEVEL_ %in% c("Choice", "Magnet"))%>%
      st_filter(feeder_data, .predicate = st_within)%>%
      mutate(color = case_when(
        LEVEL_ == "Elementary" ~ "#ed683f",
        LEVEL_ == "Middle" ~ "#ed018c",
        LEVEL_ == 'High' ~ "#008097",
        TRUE ~ "gray" 
      ))
    
    
    feeder_data %>%
      leaflet(options = leafletOptions(zoomControl = FALSE, dragging = FALSE)) %>%
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
      addTiles(urlTemplate = cpal_mapbox_color, attribution = cpal_leaflet) %>%
      addPolygons(weight = 1, opacity = 0.5, color = "#686158", fillOpacity = 0.2) %>%
      addCircleMarkers(
        data = filter(disdpoints, name == input$campus),
        stroke = FALSE,
        radius = 7,
        fillOpacity = 0.8,
        color = "#008097"
      ) %>%
      {
        if (input$schoolType == "High School" && !is.null(feeder_schools)) { 
          addCircleMarkers(
            .,  
            data = feeder_schools,  
            radius = 5, 
            color = ~color,
            fillOpacity = 0.8
          )%>%
            addLegend(
              position = "bottomright",
              colors = c("#ed683f", "#ed018c", "#008097"), 
              opacity = 0.7,
              labels = c("Elementary School", "Middle School", "High School"),
              title = "Feeder School Type"
            ) 
        } else {
          .
        }
      }
      
  })
}

# Run the application 
shinyApp(ui = ui, 
         server = server)

