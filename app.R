
suppressPackageStartupMessages({
  library(shiny)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  library(plotly)
  
  library(bigelowshinytheme)
})

file <- "odj-obis-haedat_2023-11-8.tsv"

x <- suppressMessages(read_tsv(file)) 

axis_vars <- c(
  "Coastline Length (km)" = "coastline (world factbook km)",
  "EEZ Size km2" = "EEZ km2",
  "GDP 2020" = "GDP 2020",
  "HAB Forecast Scholar Hits" = "HAB forecast",
  "OBIS Records" = "obis_n",
  "HAEDAT Records" = "haedat_n",
  "Algaebase Records" = "AlgaeBase hits [marine]"
)

# lookup table for switching between log10(x) and log10(x+1)
log_add <- c(
  "coastline (world factbook km)"=0,
  "EEZ km2"=0,
  "GDP 2020"=0,
  "HAB forecast"=0,
  "obis_n"=1,
  "haedat_n"=1,
  "AlgaeBase hits [marine]" = 1
)

ui = fluidPage(
  theme = bigelowshinytheme::bigelow_theme(),
  includeCSS("www/additionalStyles.css"),
  bigelowshinytheme::bigelow_header("Harmful Algae Forecasting through an Ocean Data Justice Lens"),
  bigelowshinytheme::bigelow_main_body(
    #h3("Harmful Algae Forecasting through an Ocean Data Justice Lens"),
    p("Nicholas R. Record, Benjamin Tupper, Kenny Douyon, Lauren Drakopulos, Lourdes Vera, Johnathan Evanilla"),
    p("Forecasting systems for harmful algal blooms (HABs) are becoming more common as HAB monitoring is increasingly networked and aggregated at national and global scales. 
    Ocean forecasting programs in other fields, however, have been seen to have unintended consequences and out-of-scope uses. 
    The field of Data Justice provides a framework for understanding unintended harms caused by the application of data technologies generally and is now starting to be applied to environmental fields. 
    With the proliferation of artificial intelligence algorithms and widespread environmental surveillance, it is timely to turn the Data Justice lens toward environmental applications, such as the prediction of HABs. 
    We surveyed three global data repositories underpinning HAB monitoring and prediction efforts: the Ocean Biodiversity Information System, 
    the Harmful Algae Event Database, and AlgaeBase, as well as a literature corpus and the ocean forecasting literature. 
    The patterns we found reflect and potentially reinforce the existing economic and political relations that underpin global ocean stresses, with monitoring and knowledge generally concentrated 
    in high-GDP, northern North Atlantic nations, and biases toward visibility of taxa relevant to those regions. 
    Principles from Data Justice research, such as from design justice and algorithmic accountability, provide guidance for centering equity and access while building global data and forecast systems for HABs."),
    sidebarLayout(
      sidebarPanel = sidebarPanel("Plot options:",
                                  selectInput("x", "X:", axis_vars, selected="GDP 2020"),
                                  checkboxInput("x_per_capita", "X per capita", TRUE),
                                  checkboxInput("log_x", "Log X", TRUE),
                                  selectInput("y", "Y:", axis_vars, selected="obis_n"),
                                  checkboxInput("y_per_capita", "Y per capita", FALSE),
                                  checkboxInput("log_y", "Log Y", TRUE)),
      mainPanel = mainPanel(bigelowshinytheme::bigelow_card(plotlyOutput("dynamic_plot")))
    ),
    br(),
    #h4("Data"),
    bigelowshinytheme::bigelow_card(headerContent = "Complete Dataset", DT::dataTableOutput("table")),
    br(),
    downloadButton("downloadData", "Download data"),
    actionButton(inputId='ab1', 
                 label="Download full text", 
                 icon = icon("th"), 
                 onclick ="window.open('https://pubs.acs.org/doi/10.1021/acs.est.4c10838', '_blank')"),
    br(),
  ),
  bigelowshinytheme::bigelow_footer("Tandy Center for Ocean Forecasting")
)

server = function(input, output) {
  
  plotdata <- reactive({
    
    pdata = mutate(x, 
                   `SIDS (1-car, 2-pac, 3-aims)` = ifelse(`SIDS (1-car, 2-pac, 3-aims)` %in% c(1,2,3), TRUE, FALSE),
                   LDC = ifelse(LDC == 1, TRUE, FALSE))
    #x variable
    if (input$log_x & input$x_per_capita) {
      pdata<- pdata|>
        dplyr::mutate(!!input$x := log10((.data[[input$x]]/x$`Population 2020`)+as.numeric(log_add[[input$x]])))
    } else if (input$x_per_capita) {
      pdata<- pdata|>
        dplyr::mutate(!!input$x := .data[[input$x]]/x$`Population 2020`)
    } else if (input$log_x) {
      pdata<- pdata|>
        dplyr::mutate(!!input$x := log10(.data[[input$x]]+as.numeric(log_add[[input$x]])))
    } else {
      x
    }
    
    #y variable
    if (input$log_y & input$y_per_capita) {
      pdata<- pdata|>
        dplyr::mutate(!!input$y := log10((.data[[input$y]]/x$`Population 2020`)+as.numeric(log_add[[input$y]])))
    } else if (input$y_per_capita) {
      pdata<- pdata|>
        dplyr::mutate(!!input$y := .data[[input$y]]/x$`Population 2020`)
    } else if (input$log_y) {
      pdata<- pdata|>
        dplyr::mutate(!!input$y := log10(.data[[input$y]]+as.numeric(log_add[[input$y]])))
    } else {
      x
    }
  })
  
  output$dynamic_plot <- renderPlotly({
    
    xvar_name <- names(axis_vars)[axis_vars == input$x]
    yvar_name <- names(axis_vars)[axis_vars == input$y]
    
    #z <- plotdata()
    #ct <- cor(x = z[[input$x]], y = z[[input$y]])
    
    plot_ly(data = plotdata(), 
            x = ~get(input$x), 
            y = ~get(input$y),
            name = "",
            type = "scatter",
            mode = "markers",
            color = ~`SIDS (1-car, 2-pac, 3-aims)`,
            colors = c("gray", "yellow"),
            stroke = ~LDC,
            strokes = c("gray", "blue"),
            marker = list(size = 12),
            text = ~paste0("Country: ", `Country Name`, sep=" ")) |>
      layout(xaxis = list(title = xvar_name),
             yaxis = list(title = yvar_name),
             #legend=list(title=list(text='<b> Small Island Developing State </b>')),
             showlegend=FALSE)
  })
  
  output$table <- DT::renderDataTable(
    DT::datatable({
      data <- x
      data
    })
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("hab_odj_data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(x, file)
    }
  )
}

shinyApp(ui, server)