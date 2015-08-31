library(shinythemes)

#TODO: This is horrible and must be destroyed.
options <- list()
for(i in 1:nrow(Procedures)){
  options[[Procedures[i,]$procedure]]<-Procedures[i,]$code
}

shinyUI(
  navbarPage("Health Cost Explorer", theme=shinytheme("cerulean"),
    tabPanel("Data",
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "main.css")),
      tags$head(tags$script(src="geolocate.js")),
      fluidRow(
        column(2, offset = 1, id="controls",
          selectInput("code", "Procedure", options, selected="0604", selectize=FALSE),
          selectInput("state", "State", as.character(StateCentroids$Code), selected="MA", selectize=FALSE),
          selectInput("year", "Year", c(2011,2012,2013), selected="2013", selectize=FALSE),
          div(id="latitude-wrapper", textInput("latitude",  "Latitude",     value = "")),
          div(id="longitude-wrapper", textInput("longitude", "Longitude",    value = "")),
          div(id="address-wrapper", textInput("address", "Your Address", value = "")),
          actionButton("geolocate", "Calculate Distances", icon = NULL)),
        column(8,
          leafletOutput("mymap"),
          dataTableOutput("mytable")
        )
      )
    ),
    tabPanel("About",fluidRow(column(6, offset = 3,
      includeMarkdown("Readme.md")
    )))
  )
)
