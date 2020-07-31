library(shiny)
source("functions.R")

# Parameters
server <- function(input, output){
  ## Initialization
  ## ------------------------------------
  state <- do.call(reactiveValues, init_state())

  observeEvent(input $ reset, {
    new_state <- init_state(as.integer(input $ seed))
    copy_list(new_state, state)
  })

  output $ discount <- renderUI({
    last_price <- state $ price_history [length(state $ price_history)]
    valid_prices <- keep(config $ price_levels, ~ .x <= last_price)
    discount_pct <-
      (max(config $ price_levels) - valid_prices) / max(config $ price_levels)
    names(valid_prices) <- percent(discount_pct)
    radioButtons("price", "Discount", choices=valid_prices)
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
        max(history() $ revenue) %>% prettyNum(big.mark=",", scientific=FALSE),
        maximum_revenue(state) %>% prettyNum(big.mark=",", scientific=FALSE))
    } else {
      sprintf("You have %d s to make a decision!", timer())
    }
  })

  output $ inventory_plot <- renderMetricsgraphics({
    history() %>%
      mjs_plot(x=t, y=inventory, linked=TRUE, title="Inventory") %>%
      mjs_line(interpolate='linear') %>%
      mjs_axis_x(xax_count=config $ n_weeks,
                 min_x=0, max_x=config $ n_weeks) %>%
      mjs_axis_y(show=TRUE, min_y=0, max_y=config $ max_capacity) %>%
      mjs_labs(x="Weeks", y="Inventory (Units)")
  })

  output $ revenue_plot <- renderMetricsgraphics({
    history() %>%
      mjs_plot(x=t, y=revenue, linked=TRUE, title="Revenue") %>%
      mjs_line(interpolate='linear') %>%
      mjs_axis_x(xax_count=config $ n_weeks,
                 min_x=0, max_x=config $ n_weeks) %>%
      mjs_axis_y(show=TRUE, min_y=0,
                 max_y=with(config, max_capacity * max(price_levels))) %>%
      mjs_labs(x="Weeks", y="Revenue (Units)")
  })

  output $ tbl <- renderDataTable(history()[-1, ])
  output $ download <- downloadHandler(
    filename = function() paste0("retailer-seed", input $ seed, ".csv"),
    content = function(file)
      write.csv(history()[-1, ], file, row.names = FALSE)
  )
}
