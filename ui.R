library(metricsgraphics)
library(DT)
library(shiny)

fluidPage(
  sidebarLayout(
    sidebarPanel(
      includeMarkdown("instructions.md"),
      textOutput("seed"),
      span(textOutput("msg"), style='color:red'),
      tabsetPanel(
        type='tabs',
        tabPanel("Actions",
                 uiOutput("discount"),
                 actionButton("reset", "Reset"),
                 actionButton("step", "Step"),
                 downloadButton("download", "Download")),
        tabPanel("Config",
                 textInput("seed", "Seed", value=NULL)))),
    mainPanel(fluidRow(column(6, metricsgraphicsOutput("inventory_plot")),
                       column(6, metricsgraphicsOutput("revenue_plot"))),
              DTOutput("tbl"))))
