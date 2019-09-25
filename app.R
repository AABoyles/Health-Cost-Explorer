#!/usr/bin/env R

library("shiny")
library("dplyr")
library("stringr")
library("leaflet")
library("DT")
library("plotly")

load("data/Medicare_Data.rdata")

shinyApp(
  navbarPage("Medicare Cost Explorer", theme = "cerulean.min.css",
    tabPanel("Data",
      fluidRow(
        column(2, offset = 1, id = "controls",
        	selectInput("year",  "Year", 2011:2017, selected = 2014),
          selectInput("state", "State", as.character(StateCentroids$Code), selected = "VA"),
          selectInput("code",  "Procedure", list("Outpatient" = OutpatientCodes$procedure, "Inpatient" = InpatientCodes$Procedure))),
        column(8,
        	tabsetPanel(
        		tabPanel("Map", leafletOutput("mymap")),
        		tabPanel("Table", dataTableOutput("mytable")),
        		tabPanel("Timeline", plotlyOutput("myPlot"))
        	)
        )
      )
    ),
    tabPanel("About", fluidRow(column(6, offset = 3,
      includeMarkdown("Readme.md"),
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "main.css"))
    )))
  ),

  shinyServer(function(input, output, session) {
   	thisData <- reactive({
  		if(input$code %in% InpatientCodes$Procedure){
  			coder <- InpatientCodes %>%
  				filter(Procedure==input$code)
        InpatientData %>%
        	filter(Procedure==coder$Procedure) ->
  				data
  		} else {
  			coder <- OutpatientCodes %>%
  				filter(procedure==input$code)
  			OutpatientData %>%
        	filter(code==coder$code) ->
  				data
  		}
  		Providers %>%
        filter(`Provider State`==input$state) %>%
  			inner_join(data)
  	})
   	
   	thisYear <- reactive({
   		thisData() %>%
   			filter(year==input$year)
   	})
  	
    output$mymap <- renderLeaflet({
      state <- StateCentroids %>% filter(Code==input$state)
      data <- thisYear()
      if(nrow(data)>0){
        leaflet(data) %>%
          setView(lng = state$Longitude[1], lat = state$Latitude[1], zoom = 8) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addCircleMarkers(
            lat = ~latitude,
            lng = ~longitude,
            popup = ~paste0(
              "<b>",str_to_title(`Provider Name`),"</b><br />",
              "Average Total Cost: $", format(round(`Average Total Payments`,2), decimal.mark=".", big.mark=","), "<br />",
              "Treatments Performed: ", performed))
      } else {
        leaflet() %>%
          setView(lng = state$Longitude[1], lat = state$Latitude[1], zoom = 8) %>%
          addProviderTiles("CartoDB.Positron")
      }
    })
    
    output$mytable <- renderDataTable({
			thisYear() %>%
        mutate(Location = str_to_title(`Provider City`)) %>%
        select(`Provider Name`, Location, `Treatments Performed`=performed, `Average Total Payments`) %>%
        datatable(options=list(paging=FALSE, searching=FALSE, responsive=TRUE), rownames = FALSE, escape = FALSE) %>%
    		formatCurrency(~`Average Total Payments`)
    })
    
    output$myPlot <- renderPlotly({
			thisData() %>%
    		plot_ly(type = 'scatter', mode = 'lines',
    			x = ~year,
    			y = ~`Average Total Payments`,
    			color = ~`Provider Name`
    		) %>%
        layout(xaxis = list(dtick = 1))
    })
  })
)
