library(metricsgraphics)
library(shiny)
library(DT)
library(scales)
source("functions.R")

# Parameters
server <- function(input, output, session){
  ## Initialization
  ## ------------------------------------
  # State variables
  price_history <- reactiveVal(max(config $ price_levels))
  timer <- reactiveVal(config $ decision_time)

  current_seed <- reactive({
    input $ reset
    isolate({
      query_seed <-
        parseQueryString(session$clientData$url_search)[['seed']] %>%
        null2na %>% as.numeric
      coalesce(as.numeric(input $ seed), query_seed, sample(1e5, 1))
    })
  })

  scenario <- reactive(init_scenario(current_seed()))

  observeEvent(input $ reset, {
    timer(config $ decision_time)
    price_history(max(config $ price_levels))
  })

  observe({
    invalidateLater(1000, session)
    isolate({
      timer(max(timer() - 1, 0))
    })
  })

  output $ discount <- renderUI({
    last_price <- last(price_history())
    valid_prices <- keep(config $ price_levels, ~ .x <= last_price)
    discount_pct <- with(
      config, (max(price_levels) - valid_prices) / max(price_levels))
    names(valid_prices) <- percent(discount_pct)
    radioButtons("price", "Discount", choices=valid_prices)
  })

  ## Event loop
  ## -------------------------------------
  history <- reactive(summarise_state(scenario(), price_history()))
  season_over <- reactive(is_season_over(scenario(), price_history()))

  step <- function(){
    if(!season_over()){
      new_history <- update_history(
        scenario(), price_history(), input $ price)
      price_history(new_history)
      timer(config $ decision_time)
    }
  }

  observeEvent(input $ step, step())
  observe(if (timer() == 0) step())

  ## Output
  ## --------------------------------------
  output $ msg <- renderText({
    if(season_over()){
      sprintf(paste(
        "The season is over! You earned $%s, out of $%s possible.",
        "Click Reset to try again."),
        max(history() $ revenue) %>%
        prettyNum(big.mark=",", scientific=FALSE),
        maximum_revenue(scenario()) %>%
        prettyNum(big.mark=",", scientific=FALSE))
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
      mjs_labs(x="Weeks", y="Revenue ($)")
  })

  output $ tbl <- renderDT({
    (history()[-1, ]
      %>% select(-sales)
      %>% datatable(options=list(dom='t', paging=FALSE)))
  })

  output $ download <- downloadHandler(
    filename = function() paste0("retailer-seed", input $ seed, ".csv"),
    content = function(file)
      write.csv(history()[-1, ], file, row.names = FALSE)
  )

  output $ seed <- renderText({
    sprintf('Using seed: %d', current_seed())
  })
}
