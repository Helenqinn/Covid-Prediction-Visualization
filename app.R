
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(sf)
library(pROC)
library(haven)
library(memisc)
library(lubridate)
library(shinyjs)

# Page 1: weekly prediction
prediction <- read.csv("predictions_with_geodata.csv")
county=prediction[,c('state','NAME','geometry','Prediction')]
county_sf <- st_as_sf(county, wkt = "geometry", crs = 4326)

# Page 2: past prediction
csv_files <- list.files("Viz-COVID19-main/Weekly_data", pattern = "\\.csv$", full.names = TRUE)
list_of_dataframes <- lapply(csv_files, function(file) {
  df <- read.csv(file)
  df$week <- as.numeric(gsub("\\D", "", basename(file)))  
  return(df)
})
history <- do.call(rbind, list_of_dataframes)

# slider bar
start_date <- ymd("2020-08-19")
num_weeks <- 175
week_dates <- seq(start_date, by = "week", length.out = num_weeks)


# create website
ui <- fluidPage(
  navbarPage("Forecasting local surges in COVID-19 hospitalizations through adaptive decision tree classifiers",
             
             # Page 1
             tabsetPanel(
               tabPanel("Weekly Prediction",
                        h2("Risk of COVID-19 hospitalizations exceeding the hospital capacity of 15 per 100,000 population over the next 3 weeks"),
                        fluidPage(
                          leafletOutput("map"),
                          uiOutput("legend")
                        )
               ),
               
               # Page 2
               tabPanel("Past Predictions",
                        fluidRow(
                          # Add titles above the maps
                          column(6, tags$h3("Predicted risk of surges in COVID-19 hospitalizations above the threshold 15 per 100,000 population")),
                          column(6, tags$h3("Whether COVID-19 hospitalizations exceeded the threshold 15 per 100,000 population"))
                        ),
                        fluidRow(
                          # Create two maps side by side
                          column(6,leafletOutput("map1")),
                          column(6,leafletOutput("map2"))
                          ),
                        uiOutput("legend1"),
                        fluidRow(
                          column(12, tags$h3("Decision tree:")),
                        ),
                        uiOutput("image_list",style = "text-align: center;"),
                        fluidRow(
                          column(12, align = "center", 
                                 tags$div(style = "margin-top: 20px; font-size: 20px;", 
                                          textOutput("roc_sentence")))
                        ),
                        fluidRow(
                          column(12, align = "center", 
                                 sliderInput("week",
                                             "",
                                             min = as.Date(start_date,"%Y-%m-%d"),
                                             max = as.Date(week_dates[175],"%Y-%m-%d"),
                                             value=as.Date(start_date), step=7, width = "100%"))
                        )),
               # Page 3
               tabPanel("About",
                        tags$style(".text-content {
                                    position: relative;
                                    font-size: 14px;
                                    }"
                        ),
                        div(class = "text-content",
                            br(),
                            p("This tool predicts whether hospitalizations due to COVID-19 are expected to surpass a certain capacity threshold (e.g., 15 per 100,000 population members) within the next 3 weeks."),
                            br(),
                            p("The predictions are produced based on the reported number of COVID-19-related hospital admissions and hospital and ICU beds occupied by COVID-19 patients (",
                            tags$a("HealthData.gov", href="https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/anag-cw7u/about_data"),
                            ")."),
                            br(),
                            p("The details of the methodology can be found on medRxiv [link to be added].")),
                            br(),
                            p("Contributors to this project include: Rachel Murray-Watson, Raihan Qin, ", 
                            tags$a("Alyssa Bilinski", href ="https://vivo.brown.edu/display/abilinsk"), " and ",
                            tags$a("Reza Yaesoubi", href="https://ysph.yale.edu/profile/reza-yaesoubi/"), "."),
                            br(),
                            p("This project is supported by the National Institute of Allergy and Infectious Diseases (NIAID) of the National Institutes of Health (NIH) under Award Number R21AI173746 and by Council of State and Territorial Epidemiologists (CSTE) under Award Number ???."),
                            br(),
                            p("This work is solely the responsibility of contributors and does not necessarily represent the official view of NIH or CSTE."),
                            br(),
                            p("The computational supported is provided by the ",
                            tags$a("Yale Center for Research Computing", href="https://research.computing.yale.edu/"), "."),
                            br(),
                            tags$img(src = "about.png", width="400px", align="left")
                        )
             )
  )
)

# Shiny app server
server <- function(input, output, session) {
  # Page 1
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        data = county_sf,
        fillColor = ~ifelse(Prediction == 1, "tomato", "forestgreen"),
        fillOpacity = 0.7,
        color = "white",
        weight = 1
      )%>%
      setView(lng = -95.7128, lat = 37.0901, zoom = 4)  # Set initial view
  })
  
  observe({
    output$legend <- renderUI({
      tags$div(
        class = "info legend",
        style = "padding: 6px 8px; font-size: 14px; background-color: rgba(255, 255, 255, 0.7);",
        tags$div(
          style = "background-color: tomato; width: 20px; height: 20px; display: inline-block; margin-right: 5px;"
        ),
        "High Risk",
        tags$div(
          style = "background-color: forestgreen; width: 20px; height: 20px; display: inline-block; margin-right: 5px;"
        ),
        "Low Risk"
      )
    })
  })
  
  # Page 2
  observeEvent(input$week, {
    # Filter data for the selected week
    week_input <- match(input$week, week_dates)
    week_data <- st_as_sf(subset(history, week == week_input), wkt = "geometry", crs = 4326)
  
    # Create a map
    output$map1 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        addPolygons(data = week_data,
                    fillColor = ~ifelse(Prediction_15 == 1, "tomato", "forestgreen"),
                    color = "white", fillOpacity = 0.7, weight = 1)%>%
        setView(lng = -95.7128, lat = 37.0901, zoom = 4)
    })
    
    output$map2 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        addPolygons(data = week_data,
                    fillColor = ~ifelse(Outcome_15 == 1, "tomato", "forestgreen"),
                    color = "white", fillOpacity = 0.7, weight = 1)%>%
        setView(lng = -95.7128, lat = 37.0901, zoom = 4)
    })
    
    observe({
      output$legend1 <- renderUI({
        tags$div(
          class = "info legend",
          style = "padding: 6px 8px; font-size: 14px; background-color: rgba(255, 255, 255, 0.7);",
          tags$div(
            style = "background-color: tomato; width: 20px; height: 20px; display: inline-block; margin-right: 5px;"
          ),
          "High Risk",
          tags$div(
            style = "background-color: forestgreen; width: 20px; height: 20px; display: inline-block; margin-right: 5px;"
          ),
          "Low Risk"
        )
      })
    })
    
    output$image_list <- renderUI({
      week <- week_input
      file_path <- paste0("Classifier_week_", week, ".png")
      tags$img(src = file_path, style = "max-width: 100%; height: auto;", align = "center")
    })
    
    # auROC
    auroc <- round(auc(roc(week_data$Outcome_15,week_data$Predict_proba_15)),2)
    output$roc_sentence <- renderText({
      paste("The auROC of the week of", format(ymd(input$week), "%B %d, %Y"), "is:", auroc)
  })
  })
  
}

# Run the Shiny app
shinyApp(ui, server)




