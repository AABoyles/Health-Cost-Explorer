#!/usr/bin/env R

libs <- c("magrittr", "readr", "dplyr", "stringr", "shiny", "DT", "leaflet", "shinythemes")
notInstalled <- setdiff(libs, as.vector(installed.packages()[, "Package"]))
if(length(notInstalled > 0)){
	install.packages(notInstalled)
}
lapply(libs, library, character.only = TRUE)
