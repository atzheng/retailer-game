library(shiny)
source("functions.R")

# Parameters
server <- function(input, output){
  ## Initialization
  ## ------------------------------------
  state <- do.call(reactiveValues, init_state(sample(1:1e4, 1)))
  observeEvent(input $ reset, {
#    shinyjs::enable("step")
    new_state <- init_state(as.integer(input $ seed))
    copy_list(new_state, state)
  })

  output $ discount <- renderUI({
    last_price <- state $ price_history [length(state $ price_history)]
    valid_prices <- keep(~ .x < last_price)
    discount_pct <- max(prices) - valid_prices / max(prices)
    radioButtons("price", "Discount", choices=valid_prices)
                 ## choiceValues=valid_prices,
                 ## choiceNames=discount_pct)
  })

  ## Event loop
  ## -------------------------------------
  history <- reactive(summarise_state(state))
  season_over <- reactive(is_season_over(state))

  observeEvent(input $ step, {
    if(!season_over()){
      state $ price_history <- c(state $ price_history,
                                 as.numeric(input $ price))
      if(season_over()){
        complete_state(state)
      }
    }
  })

  ## Output
  ## --------------------------------------
  output $ msg <- renderText({
    if(season_over()){
      sprintf(paste(
        "The season is over! You earned $%s, out of $%s possible.",
        "Click Reset to try again."),
        max(history() $ Revenue) %>% prettyNum(big.mark=","),
        state $ max_revenue %>% prettyNum(big.mark=","))
    } else {
      "Welcome to the Retailer Game! Choose a price and click Step to simulate."
    }
  })

  output $ inventory_plot <- renderPlot({
    (history()
      %>% ggplot(aes(t, Inventory))
      + theme_minimal()
      + geom_point()
      + geom_line()
      + theme(legend.position="bottom")
      + ggtitle("Inventory")
      + coord_cartesian(ylim=c(0, config $ max_capacity), xlim=c(0, config $ n_weeks))
      + xlab("Weeks")
      + ylab("Inventory (Units)"))
  })

  output $ revenue_plot <- renderPlot({
    (history()
      %>% ggplot(aes(t, Revenue))
      + theme_minimal()
      + geom_point()
      + geom_line()
      + theme(legend.position="bottom")
      + xlab("Weeks")
      + ggtitle("Revenue")
      + ylab("Revenue ($)")
      + coord_cartesian(ylim=c(0, state $ max_revenue), xlim=c(0, config $ n_weeks)))
  })

  output $ tbl <- renderDataTable(history())
}
