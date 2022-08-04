library(metricsgraphics)
library(DT)
library(shiny)
library(markdown)

fluidPage(
  sidebarLayout(
    sidebarPanel(
      includeMarkdown("instructions.md"),
      fluidRow(
        column(6,
               uiOutput("seed_input"),
               uiOutput("set_seed_button"),
               uiOutput("randomize_button")),
        column(6,
               uiOutput("discount"),
               actionButton("step", "Step"),
               downloadButton("download", "Download"))),
      span(textOutput("msg"), style='color:red')
    ),
    mainPanel(fluidRow(column(6, metricsgraphicsOutput("inventory_plot")),
                       column(6, metricsgraphicsOutput("revenue_plot"))),
              DTOutput("tbl"))))
