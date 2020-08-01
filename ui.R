library(metricsgraphics)
library(DT)
library(shiny)

fluidPage(
  sidebarLayout(
    sidebarPanel(
      includeMarkdown("instructions.md"),
      span(textOutput("msg"), style='color:red'),
      tabsetPanel(
        type='tabs',
        tabPanel("Actions",
                 uiOutput("discount"),
                 actionButton("reset", "Reset"),
                 actionButton("step", "Step"),
                 downloadButton("download", "Download")),
        tabPanel("Config",
                 textInput("seed", "Seed", value=sample(1:1e4, 1))))),
    mainPanel(fluidRow(column(6, metricsgraphicsOutput("inventory_plot")),
                       column(6, metricsgraphicsOutput("revenue_plot"))),
              DTOutput("tbl"))))
