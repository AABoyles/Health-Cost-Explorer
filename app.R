#!/usr/bin/env R

library("shiny")
library("dplyr")
library("stringr")
library("leaflet")
library("DT")
library("plotly")
library("readr")
library("scales")

load("data/Medicare_Data.rdata")

# Load Medicaid data if available (produced by build.R)
MedicaidData <- if (file.exists("data/MedicaidData.csv")) {
  read_csv("data/MedicaidData.csv", show_col_types = FALSE)
} else {
  NULL
}

medicaid_years <- if (!is.null(MedicaidData)) sort(unique(MedicaidData$year), decreasing = TRUE) else integer(0)
has_medicaid    <- length(medicaid_years) > 0

shinyApp(
  navbarPage("Health Cost Explorer", theme = "cerulean.min.css",

    ## ── Medicare tab ────────────────────────────────────────────────────────
    tabPanel("Medicare",
      fluidRow(
        column(2, offset = 1, id = "controls",
          selectInput("year", "Year",
                      seq(min(c(InpatientData$year, OutpatientData$year), na.rm = TRUE),
                          max(c(InpatientData$year, OutpatientData$year), na.rm = TRUE)),
                      selected = max(c(InpatientData$year, OutpatientData$year), na.rm = TRUE)),
          selectInput("state", "State",
                      as.character(StateCentroids$Code), selected = "VA"),
          selectInput("code", "Procedure",
                      list("Outpatient" = OutpatientCodes$procedure,
                           "Inpatient"  = InpatientCodes$Procedure))),
        column(8,
          tabsetPanel(
            tabPanel("Map",      leafletOutput("mymap")),
            tabPanel("Table",    dataTableOutput("mytable")),
            tabPanel("Timeline", plotlyOutput("myPlot"))
          )
        )
      )
    ),

    ## ── Medicaid tab ─────────────────────────────────────────────────────────
    tabPanel("Medicaid",
      if (!has_medicaid) {
        fluidRow(column(8, offset = 2,
          wellPanel(
            h3("Medicaid Data Not Yet Built"),
            p("Run ", code("make build"), " to download and aggregate the HHS Medicaid",
              " Provider Spending dataset (2018–2024). The build step uses DuckDB to",
              " aggregate 227 million provider-level records into a compact summary",
              " without downloading the full 2.94 GB parquet file."),
            p("Source: ",
              a("HHS Open Data – Medicaid Provider Spending",
                href = "https://opendata.hhs.gov/datasets/medicaid-provider-spending/",
                target = "_blank"))
          )
        ))
      } else {
        fluidRow(
          column(2, offset = 1,
            selectInput("mdcd_year", "Year",
                        medicaid_years, selected = medicaid_years[1]),
            numericInput("mdcd_top_n", "Top N procedures", 25, min = 5, max = 100, step = 5),
            hr(),
            p(em("Data: HHS Medicaid Provider Spending, fee-for-service + managed care + CHIP,",
                 " aggregated nationally by HCPCS code."),
              style = "font-size:11px; color:#777")
          ),
          column(8,
            tabsetPanel(
              tabPanel("Top Procedures",
                plotlyOutput("mdcdBar", height = "600px")
              ),
              tabPanel("Trend",
                selectInput("mdcd_hcpcs", "HCPCS Code",
                            sort(unique(MedicaidData$hcpcs_code))),
                plotlyOutput("mdcdTrend")
              ),
              tabPanel("Summary Table",
                dataTableOutput("mdcdTable")
              )
            )
          )
        )
      }
    ),

    ## ── About tab ────────────────────────────────────────────────────────────
    tabPanel("About", fluidRow(column(6, offset = 3,
      includeMarkdown("Readme.md"),
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "main.css"))
    )))
  ),

  ## ── Server ─────────────────────────────────────────────────────────────────
  shinyServer(function(input, output, session) {

    ## Medicare reactive data
    thisData <- reactive({
      if (input$code %in% InpatientCodes$Procedure) {
        coder <- InpatientCodes %>% filter(Procedure == input$code)
        data  <- InpatientData  %>% filter(Procedure == coder$Procedure)
      } else {
        coder <- OutpatientCodes %>% filter(procedure == input$code)
        data  <- OutpatientData  %>% filter(code      == coder$code)
      }
      Providers %>%
        filter(`Provider State` == input$state) %>%
        inner_join(data, by = "Provider Id")
    })

    thisYear <- reactive({
      thisData() %>% filter(year == input$year)
    })

    ## Medicare: Map
    output$mymap <- renderLeaflet({
      state <- StateCentroids %>% filter(Code == input$state)
      data  <- thisYear()
      base_map <- leaflet() %>%
        setView(lng = state$Longitude[1], lat = state$Latitude[1], zoom = 8) %>%
        addProviderTiles("CartoDB.Positron")
      if (nrow(data) > 0) {
        base_map %>%
          addCircleMarkers(
            data = data,
            lat  = ~latitude, lng = ~longitude,
            popup = ~paste0(
              "<b>", str_to_title(`Provider Name`), "</b><br/>",
              "Average Total Cost: $",
              format(round(`Average Total Payments`, 2),
                     big.mark = ",", decimal.mark = "."), "<br/>",
              "Treatments Performed: ", performed))
      } else {
        base_map
      }
    })

    ## Medicare: Table
    output$mytable <- renderDataTable({
      thisYear() %>%
        mutate(Location = str_to_title(`Provider City`)) %>%
        select(`Provider Name`, Location,
               `Treatments Performed` = performed,
               `Average Total Payments`) %>%
        datatable(options = list(paging = FALSE, searching = FALSE,
                                 responsive = TRUE),
                  rownames = FALSE, escape = FALSE) %>%
        formatCurrency(~`Average Total Payments`)
    })

    ## Medicare: Timeline
    output$myPlot <- renderPlotly({
      thisData() %>%
        plot_ly(type = "scatter", mode = "lines",
                x = ~year, y = ~`Average Total Payments`,
                color = ~`Provider Name`) %>%
        layout(xaxis = list(dtick = 1))
    })

    ## Medicaid: Top Procedures bar chart
    if (has_medicaid) {
      output$mdcdBar <- renderPlotly({
        df <- MedicaidData %>%
          filter(year == as.integer(input$mdcd_year)) %>%
          arrange(desc(total_paid)) %>%
          slice_head(n = input$mdcd_top_n) %>%
          mutate(
            hcpcs_code = factor(hcpcs_code, levels = rev(hcpcs_code)),
            label = paste0("$", format(round(total_paid / 1e6, 1), big.mark = ","), "M")
          )

        plot_ly(df,
                x = ~total_paid, y = ~hcpcs_code,
                type = "bar", orientation = "h",
                hovertemplate = paste0(
                  "<b>%{y}</b><br>",
                  "Total Paid: $%{x:,.0f}<br>",
                  "Claims: %{customdata[0]:,}<br>",
                  "Beneficiaries: %{customdata[1]:,}<extra></extra>"),
                customdata = ~cbind(total_claims, total_beneficiaries)
        ) %>%
          layout(
            title  = paste0("Top ", input$mdcd_top_n,
                            " Medicaid Procedures by Total Paid (", input$mdcd_year, ")"),
            xaxis  = list(title = "Total Paid", tickformat = "$,.0f"),
            yaxis  = list(title = "HCPCS Code"),
            margin = list(l = 80)
          )
      })

      ## Medicaid: Trend line
      output$mdcdTrend <- renderPlotly({
        df <- MedicaidData %>%
          filter(hcpcs_code == input$mdcd_hcpcs) %>%
          arrange(year)

        plot_ly(df,
                x = ~year, y = ~total_paid,
                type = "scatter", mode = "lines+markers",
                name = "Total Paid",
                hovertemplate = paste0(
                  "Year: %{x}<br>",
                  "Total Paid: $%{y:,.0f}<br>",
                  "<extra></extra>")
        ) %>%
          layout(
            title  = paste0("Medicaid Spending Trend: HCPCS ", input$mdcd_hcpcs),
            xaxis  = list(title = "Year", dtick = 1),
            yaxis  = list(title = "Total Paid ($)", tickformat = "$,.0f")
          )
      })

      ## Medicaid: Summary table
      output$mdcdTable <- renderDataTable({
        MedicaidData %>%
          filter(year == as.integer(input$mdcd_year)) %>%
          arrange(desc(total_paid)) %>%
          select(
            `HCPCS Code`      = hcpcs_code,
            `Total Paid`      = total_paid,
            `Total Claims`    = total_claims,
            `Beneficiaries`   = total_beneficiaries
          ) %>%
          datatable(options = list(pageLength = 25, responsive = TRUE),
                    rownames = FALSE) %>%
          formatCurrency("Total Paid") %>%
          formatRound(c("Total Claims", "Beneficiaries"), digits = 0)
      })
    }
  })
)
