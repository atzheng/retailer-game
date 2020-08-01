library(tidyverse)
library(rJava)

config <- list(
  n_weeks=16,
  n_prices=4,
  max_capacity=2000,
  price_levels=c(100, 90, 80, 60),
  decision_time=30,
  lift=c(1.0, 1.3, 1.7, 2.8))

init_scenario <- function(seed=NULL){
  # Use Java RNG for backwards compatibility.
  seed <- `if`(is.null(seed) || is.na(seed), sample(1:1e4, 1), seed)
  .jinit()
  g <- new(J("java.util.Random"), .jlong(seed))
  mean_scale <- 0.7 * config $ max_capacity / config $ n_weeks
  scale <- abs(g $ nextGaussian() * 0.6 * mean_scale + mean_scale)
  noise <- (rerun(config $ n_weeks * config $ n_prices, g $ nextGaussian())
    %>% matrix(ncol=config $ n_weeks) %>% t)
  list(noise=noise, scale=scale)
}

rep_each <- function(x, each)
  map2(x, each, ~ rep(.x, .y)) %>% do.call(c, .)

eval_prices <- function(scenario, prices)
  summarise_state(scenario, prices) %>% `$`(revenue) %>% max

maximum_revenue <- function(scenario){
  (rerun(length(config $ price_levels), seq(0, config $ n_weeks - 1))
    %>% expand.grid
    %>% filter(rowSums(.) == config $ n_weeks - 1)
    %>% apply(1, function(x){
      with(config, c(max(price_levels), rep_each(price_levels, x))) %>%
        eval_prices(scenario, .) })
    %>% max)
}

compute_lift <- function(prices){
  config $ lift[match(prices, config $ price_levels)]
}

compute_demand <- function(scenario, price_history){
  mean_demand <- compute_lift(price_history) * scenario $ scale
  noise <- scenario $ noise [seq_along(price_history),
                             match(price_history,
                                   config $ price_levels)] %>%
    as.matrix %>% diag %>% unlist
  pmax(mean_demand * 0.2 * noise + mean_demand, 0) %>% as.integer
}

is_season_over <- function(scenario, price_history){
  history <- summarise_state(scenario, price_history)
  (max(history $ t) >= config $ n_weeks
    || min(history $ inventory) <= 0
    || min(history $ price) <= min(config $ price_levels))
}

summarise_state <- function(scenario, price_history){
  prices <- c(config $ price_levels[1], price_history)
  demand <- c(0, compute_demand(scenario, price_history))
  cum_demand <- pmin(cumsum(demand), config $ max_capacity)
  inventory <- config $ max_capacity - cum_demand
  sales <- pmin(demand, dplyr::lag(inventory, default=config $ max_capacity))
  revenue <- cumsum(prices * sales)
  tibble(t=seq(0, length(demand) - 1),
         inventory=inventory,
         revenue=revenue,
         demand=demand,
         sales=sales,
         price=prices)
}

extend <- function(x, n) c(x, rep(last(x), n - length(x)))

update_history <- function(scenario, price_history, price){
  if(!is_season_over(scenario, price_history)){
    new_history <- c(price_history, as.numeric(price))
    if(is_season_over(scenario, new_history)){
      extend(new_history, config $ n_weeks)
    } else {
      new_history
    }
  }
}
