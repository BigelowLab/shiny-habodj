## Shiny app designed to explore HAB Ocean Data Justice research results

suppressPackageStartupMessages({
  library(shiny)
  
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  library(plotly)
  
  library(markdown)
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

ui <- navbarPage(
  title = "HAB ODJ",
  tabPanel("Home",
           includeMarkdown("README.md")
           ),
  tabPanel("Interactive Plot",
           sidebarLayout(
             sidebarPanel("Axis Options",
               selectInput("x", "X:", axis_vars, selected="GDP 2020"),
               checkboxInput("x_per_capita", "X per capita", TRUE),
               checkboxInput("log_x", "Log X", TRUE),
               selectInput("y", "Y:", axis_vars, selected="obis_n"),
               checkboxInput("y_per_capita", "Y per capita", FALSE),
               checkboxInput("log_y", "Log Y", TRUE)
             ),
             mainPanel(
               plotlyOutput("dynamic_plot"),
               tags$h5(paste("yellow point", "= small island developing state (SIDS)")),
               h5(paste("blue circle", "= least developed country (LDC)")),
               h5(paste("Logged database record counts are log10(n+1); all others are log10(n)")),
               tags$br(),
               tags$h5("Figure 2 selections"),
               tableOutput("figure_2"),
               tags$h5("FIgure 3 selections"),
               tableOutput("figure_3")
             )
           )),
  #tabPanel("HAB Record"),
  tabPanel("Data Table",
           DT::dataTableOutput("table"))
)

server <- function(input, output) {
  
    output$gdp_obis <- renderPlot({
      ggplot(data=x, aes(x=log_gdp, y=log_obis)) +
        geom_point(shape=21, aes(size=1, colour = factor(LDC), fill=factor(`SIDS (1-car, 2-pac, 3-aims)`))) +
        scale_fill_manual("SID", values = c("gray", "yellow", "yellow", "yellow")) +
        scale_color_manual(values = c("gray", "blue")) +
        xlab("GDP per capita (log)") +
        ylab("OBIS hab records (log)") +
        stat_cor() +
        theme_classic() +
        guides(color=FALSE, size=FALSE, fill=FALSE) 
    })
    
    output$gdp_algaebase <- renderPlot({
      ggplot(data=x, aes(x=log_gdp, y=log_ab)) +
        geom_point(shape=21, aes(size=4, colour = factor(LDC), fill=factor(`SIDS (1-car, 2-pac, 3-aims)`))) +
        scale_fill_manual("SID", values = c("gray", "yellow", "yellow", "yellow")) +
        scale_color_manual(values = c("gray", "blue")) +
        xlab("GDP per capita (log)") +
        ylab("AlgaeBase records (log)") +
        stat_cor() +
        theme_classic() +
        guides(color=FALSE, size=FALSE, fill=FALSE) 
    })
    
    output$gdp_haedat <- renderPlot({
      ggplot(data=x, aes(x=log_gdp, y=log_hd)) +
        geom_point(shape=21, aes(size=4, colour = factor(LDC), fill=factor(`SIDS (1-car, 2-pac, 3-aims)`))) +
        scale_fill_manual("SID", values = c("gray", "yellow", "yellow", "yellow")) +
        scale_color_manual(values = c("gray", "blue")) +
        xlab("GDP per capita (log)") +
        ylab("HAEDAT records (log)") +
        stat_cor() +
        theme_classic() +
        guides(color=FALSE, size=FALSE, fill=FALSE)
    })
    
    ## !!input$x := blah blah blah
    ## xname = input$
    ## !!xname := blah
    ## vignette("programming", package = "dplyr")
    ## {{ input$x }} := blah
    
    plotdata <- reactive({
      #x variable
      if (input$log_x & input$x_per_capita) {
        x <- x |>
          dplyr::mutate(!!input$x := log10((.data[[input$x]]/x$`Population 2020`)+as.numeric(log_add[[input$x]])))
      } else if (input$x_per_capita) {
        x <- x |>
          dplyr::mutate(!!input$x := .data[[input$x]]/x$`Population 2020`)
      } else if (input$log_x) {
        x <- x |>
          dplyr::mutate(!!input$x := log10(.data[[input$x]]+as.numeric(log_add[[input$x]])))
      } else {
        x
      }
      
      #y variable
      if (input$log_y & input$y_per_capita) {
        x <- x |>
          dplyr::mutate(!!input$y := log10((.data[[input$y]]/x$`Population 2020`)+as.numeric(log_add[[input$y]])))
      } else if (input$y_per_capita) {
        x <- x |>
          dplyr::mutate(!!input$y := .data[[input$y]]/x$`Population 2020`)
      } else if (input$log_y) {
        x <- x |>
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

      p <- ggplot(data=plotdata(), aes(x = get(input$x), y = get(input$y), text=paste0("Country:", `Country Name`, sep=" "))) +
        geom_point(shape=21, aes(size=1, colour = factor(`LDC`), fill=factor(`SIDS (1-car, 2-pac, 3-aims)`))) +
        scale_fill_manual("SID", values = c("gray", "yellow", "yellow", "yellow")) +
        scale_color_manual(values = c("gray", "blue")) +
        xlab(xvar_name) +
        ylab(yvar_name) +
        stat_cor(na.rm=TRUE, label.x.npc = "center", output.type = "text") +
        theme_classic() +
        guides(color=FALSE, size=FALSE, fill=FALSE, scale="none")

      ggplotly(p, tooltip = "text")
      
    })
    
    output$figure_2 <- renderTable({
      tibble(x = c("GDP per capita (log)", "GDP per capita (log)", "GDP per capita (log)"),
             y = c("OBIS HAB records (log)", "Algaebase records (log)", "HAEDAT records (log)"))
    })
    
    output$figure_3 <- renderTable({
      tibble(x = c("Coastline length per capita (log)", "EEZ size per capita (log)", "GDP per capita (log)"),
             y = c("HAB forecast scholar hits per capita (log)", "HAB forecast scholar hits per capita (log)", "HAB forecast scholar hits per capita (log)"))
    })
    
    
    output$table <- DT::renderDataTable(
      DT::datatable({
        data <- x
        data
      })
    )
}

shinyApp(ui = ui, server = server)
