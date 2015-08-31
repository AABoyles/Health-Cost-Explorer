library(RPostgreSQL)
library(jsonlite)
library(dplyr)
library(magrittr)
library(stringr)
library(geosphere)
library(markdown)

source("creds.R")

con <- src_postgres(
  dbname="postgres",
  host=server,
  port=port,
  user=username,
  password=password
)

gGeoCode <- function(address) {
  url <- URLencode(paste0("http://maps.google.com/maps/api/geocode/json?address=", address, "&sensor=false"))
  out <- fromJSON(url, simplifyVector = FALSE)
  if(out$status=="OK") {
    return(c(out$results[[1]]$geometry$location$lng, out$results[[1]]$geometry$location$lat))
  }
  return(NULL)
}

shinyServer(function(input, output, session) {
  
  latitude <- NULL
  longitude <- NULL

  observeEvent(input$geolocate, {
    if(is.null(latitude)){
      if(is.null(input$latitude)){
        userLocation <- gGeoCode(input$address)
        if(!is.null(userLocation)){
          longitude <- userLocation[1]
          latitude  <- userLocation[2]
        }
      }
    }
  })
  
  output$mymap <- renderLeaflet({
    state <- StateCentroids %>% filter(Code==input$state)
    isp <- tbl(con, "Providers") %>%
      filter(Provider.State==input$state)
    thisData <- tbl(con, "MedicareData") %>%
      filter(year==input$year, code==input$code) %>%
      inner_join(isp, by="pid") %>%
      collect
    if(nrow(thisData)>0){
      leaflet(thisData) %>%
        setView(lng = state$Longitude[1], lat = state$Latitude[1], zoom = 8) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(
          lat = ~latitude,
          lng = ~longitude,
          popup = ~paste0(
            "<b>",str_to_title(Provider.Name),"</b><br />",
            "Average Total Cost: $", format(round(atp,2), decimal.mark=".", big.mark=","),"<br />",
            "Treatments Performed: ", performed))
    } else {
      leaflet() %>%
        setView(lng = state$Longitude[1], lat = state$Latitude[1], zoom = 8) %>%
        addProviderTiles("CartoDB.Positron")
    }
  })

  output$mytable <- renderDataTable({
    isp <- tbl(con, "Providers") %>%
      filter(Provider.State==input$state)
    thisData <- tbl(con, "MedicareData") %>%
      filter(year==input$year, code==input$code) %>%
      inner_join(isp, by="pid") %>%
      collect %>%
      mutate(Location = str_to_title(Provider.City))
    if(input$address!=""){
      thisData %<>% mutate(Distance = 0)
      userLocation <- gGeoCode(input$address)
      for(i in 1:nrow(thisData)){
        thisData[i,]$Distance <- userLocation %>%
          distGeo(c(thisData[i,]$longitude,thisData[i,]$latitude))/1609.344 %>%
          round(1)
      }
      thisData %<>%
        select(Hospital=hospital, Location, `Treatments Performed`=performed, `Average Total Payments`=atp, `Distance from You (in miles)`=Distance)
    } else {
      thisData %<>%
        select(Hospital=hospital, Location, `Treatments Performed`=performed, `Average Total Payments`=atp)
    }
    datatable(thisData, options=list(paging=FALSE, searching=FALSE, responsive=TRUE), rownames = FALSE, escape = FALSE) %>%
      formatCurrency(c("Average Total Payments"))
  })
})
