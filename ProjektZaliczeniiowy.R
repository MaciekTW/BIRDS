library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(shinydashboard)

#DataDF <- read.csv("occurrence.txt",header=TRUE,sep='\t')
#DataDF <- select(DataDF, -c(typeStatus, coordinateUncertaintyInMeters,higherGeography,footprintSRS,occurrenceStatus,geodeticDatum,individualCount,identifiedBy,occurrenceRemarks,eventRemarks,recordedBy,basisOfRecord,occurrenceID,bibliographicCitation,taxonConceptID,taxonID)) 
#DataDF <- DataDF[complete.cases(DataDF), ]
#DataDF$year <- as.numeric(substr(DataDF$eventDate, 1, 4))
#DataDF$sex[DataDF$sex == ""] <- "nie określona"


tooMuchDataFlag <- FALSE

bins <- c(0, 10, 50, 100, 200, 500, 1000, 2500,5000,8000,10000,15000, Inf)



ui <- dashboardPage(
  dashboardHeader(title = "BIRDS analysis"),
  dashboardSidebar(    
    h3("Filtry"),
    sliderInput("yearInput", "Wybierz zakres dat obserwacji:",
                min = 1880, max = 2023,
                value = c(1880, 2023), step = 1),
    textInput("searchInput", "Nazwa ptaka:", ""),
    
    fluidRow(selectInput("countryInput", "Wybierz kraje pochodzenia", multiple = TRUE, choices = unique(DataDF$country))),
    radioButtons("sexInput", "Wybierz płeć ptaka:",
                 choices = c("wszystkie",unique(DataDF$sex)),
                 selected = "wszystkie"),
    selectInput("taxonRankInput", "Wybierz taxon rank:",
                choices = c("wszystkie",unique(DataDF$taxonRank)),
                selected = "wszystkie")
    
  ),
  dashboardBody(
  fluidPage(
  titlePanel(""),
      h2("Sekcja Map"),
      uiOutput("relativeGUI")
  )
))

server <- function(input, output) {
  
  filteredData <- eventReactive(c(input$yearInput, input$searchInput,input$countryInput,input$sexInput,input$taxonRankInput), {
    # Filtrowanie danych na podstawie wyborów użytkownika
    sex <- input$sexInput
    yearRange <- input$yearInput[1]:input$yearInput[2]
    nameToSearch <- input$searchInput
    taxonInput <- input$taxonRankInput

    filtered <- DataDF

  
    countries <- input$countryInput
    
    if (sex != "wszystkie") {
      filtered <- filtered[filtered$sex == sex, ]
    }
    
    if (taxonInput != "wszystkie") {
      filtered <- filtered[filtered$taxonRank == taxonInput, ]
    }
    
    if (length(countries) > 0) {
      filtered <- filtered[filtered$country %in% countries, ]
    }
    
    filtered <- filtered[filtered$year %in% yearRange, ]
    
    
    if (nchar(input$searchInput) > 0) {
      filtered <- filtered[grep(tolower(input$searchInput), tolower(filtered$scientificName)), ]
    }
    
    if (nrow(filtered) > 3000) {
      
      filteredData_sampled <- filtered %>%
        sample_n(3000, replace = FALSE)
      
      filtered <- filteredData_sampled
      
      showModal(modalDialog(
        title = "Zbyt dużo danych",
        "Przekroczono limit 3000 próbek. Wykonuję losowe próbkowanie...",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    filtered
  })
  
  filteredData2 <- eventReactive(c(input$yearInput, input$searchInput,input$countryInput,input$sexInput,input$taxonRankInput), {
    
    x<-filteredData()
  
    liczebnosc_posortowana <- table(x$country)
    
    ramka_danych <- data.frame(
      country = names(liczebnosc_posortowana),
      observations = as.numeric(liczebnosc_posortowana)
    )
    
    ramka_danych
  })
  
  
  output$relativeGUI <- renderUI({
    
    # Sprawdzenie, czy dataframe jest pusty
    if (nrow(filteredData()) == 0) {
      # Wyświetlanie pola tekstowego "Brak danych"
      textOutput("noDataText")
    } else {
      # Wyświetlanie histogramu
      tagList(
      tabsetPanel(
        tabPanel("Mapa",leafletOutput("observationMap")),
        tabPanel("Mapa Polski", leafletOutput("observationMap2"))),
      
      h2("Analiza całego zbioru danych"),
      fluidRow(
        infoBox("Ilość obserwacji", nrow(filteredData()), icon = icon("glyphicon glyphicon-search", lib="glyphicon"), fill = TRUE),
        infoBoxOutput("progressBox2"),
        infoBoxOutput("approvalBox2")
      ),
      fluidRow(
         box(
           title = "Rozkład płci", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           plotlyOutput("sexHistogram")
         ),
         box(
           title = "Rozkład lat", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           plotlyOutput("yearHistogram")
         ),
         box(
           title = "10 najpopularniejszych krajów", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           plotlyOutput("countryHistogram")
         ),
         box(
           title = "10 najpopularniejszych gatunków", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           plotlyOutput("spiecesHistogram")
         )
      ),
      verbatimTextOutput("meanRating"))
    }
  })
  
  

  output$sexHistogram <- renderPlotly({
    plot <- plot_ly(
      x = filteredData()$sex,
      type = "histogram")
    
    plot <- plot %>% layout(xaxis = list(title = "Płeć"), yaxis = list(title = "Liczebność"))
    
  })

  output$yearHistogram <- renderPlotly({
    plot <-plot_ly(
      x = filteredData()$year,
      xbins = list(size = 10),
      type = "histogram")
    plot <- plot %>% layout(xaxis = list(title = "Rok"), yaxis = list(title = "Liczebność"))
    
  })
    

  
  output$countryHistogram <- renderPlotly({
    dane <- filteredData() %>% group_by(country) %>% summarise(catalogNumber = n()) %>% arrange(desc(catalogNumber), .by_group = TRUE)
    najliczniejsze_kraje <- head(dane, 10)
    
    
    fig <-plot_ly(
      x = najliczniejsze_kraje$catalogNumber,
      
      y = najliczniejsze_kraje$country,
      type = "bar"
      
    )
    
    layout <- list(yaxis = list(categoryorder = 'total ascending'))
    
    fig <- fig %>% layout(yaxis = layout$yaxis, xaxis = list(title = "Liczebność"))
    
    fig
  })

  output$spiecesHistogram <- renderPlotly({
    dane <- filteredData() %>% group_by(scientificName) %>% summarise(catalogNumber = n()) %>% arrange(desc(catalogNumber), .by_group = TRUE)
    najliczniejsze_gatunki <- head(dane, 10)
    
    
    fig <-plot_ly(
      x = najliczniejsze_gatunki$catalogNumber,
      
      y = najliczniejsze_gatunki$scientificName,
      type = "bar"
      
    )
    
    layout <- list(yaxis = list(categoryorder = 'total ascending'))
    
    fig <- fig %>% layout(yaxis = layout$yaxis, xaxis = list(title = "Liczebność"))
    
    fig
  })
  
    
    output$observationMap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = filteredData(), lng = ~decimalLongitude, lat = ~decimalLatitude, label = ~scientificName)
    })
    
    output$noDataText <- renderText({
      "Brak danych"
    })

    output$progressBox2 <- renderInfoBox({
      infoBox(
        "Ilość państw z obserwacjami", length(unique(filteredData()$country)), icon = icon("glyphicon glyphicon-globe", lib="glyphicon"),
        color = "purple", fill = TRUE
      )
    })
    output$approvalBox2 <- renderInfoBox({
      infoBox(
        "Ilość gatunków", length(unique(filteredData()$scientificName)),icon = icon("glyphicon glyphicon-camera", lib = "glyphicon"),
        color = "yellow", fill = TRUE
      )
    })



  
  
}


shinyApp(ui, server)