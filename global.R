library(shiny)
library(DT)
library(leaflet)
library(readr)

Procedures <- read_csv("data/Procedures2.csv", col_types = "cc")
StateCentroids <- read_csv("data/StateCentroids.csv")
