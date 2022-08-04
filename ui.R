library(metricsgraphics)
library(DT)
library(shiny)

fluidPage(
  sidebarLayout(
    sidebarPanel(
      includeMarkdown("instructions.md"),
      textOutput("seed"),
      span(textOutput("msg"), style='color:red'),
      uiOutput("discount"),
      actionButton("reset", "Reset"),
      actionButton("step", "Step"),
      downloadButton("download", "Download"),
      textInput("seed", "Seed", value=NULL),
      uiOutput("set_seed_button")
    ),
    mainPanel(fluidRow(column(6, metricsgraphicsOutput("inventory_plot")),
                       column(6, metricsgraphicsOutput("revenue_plot"))),
              DTOutput("tbl"))))
