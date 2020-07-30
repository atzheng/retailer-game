library(shiny)

fluidPage(
  sidebarLayout(sidebarPanel(
    ## uiOutput("discount"),
    textOutput("msg"),
    textInput("seed", "Seed", value=sample(1:1e4, 1)),
    radioButtons("price", "Discount", choices=c(100, 90, 80, 60)),
    ##            choiceValues=c(100, 90, 80, 60),
    ##            choiceNames=c(0., 0.1, 0.2, 0.4)),
    actionButton("reset", "Reset"),
    actionButton("step", "Step")),
    mainPanel(fluidRow(column(6, plotOutput("inventory_plot")),
                       column(6, plotOutput("revenue_plot"))),
              dataTableOutput("tbl"))))
