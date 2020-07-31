library(shiny)

fluidPage(
  sidebarLayout(sidebarPanel(
    textOutput("msg"),
    textInput("seed", "Seed", value=sample(1:1e4, 1)),
    uiOutput("discount"),
    actionButton("reset", "Reset"),
    actionButton("step", "Step"),
    downloadButton("download", "Download")),
    mainPanel(fluidRow(column(6, plotOutput("inventory_plot")),
                       column(6, plotOutput("revenue_plot"))),
              dataTableOutput("tbl"))))
