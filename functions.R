library(tidyverse)
library(rJava)

config <- list(
  n_weeks=16,
  n_prices=4,
  max_capacity=2000,
  price_levels=c(100, 90, 80, 60),
  lift=c(1.0, 1.3, 1.7, 2.8))

init_state <- function(seed=NULL){
  # Use Java RNG for backwards compatibility.
  seed <- `if`(is.null(seed), sample(1:1e4, 1), seed)
  .jinit()
  g <- new(J("java.util.Random"), .jlong(seed))
  mean_scale <- 0.7 * config $ max_capacity / config $ n_weeks
  scale <- abs(g $ nextGaussian() * 0.6 * mean_scale + mean_scale)
  noise <- (rerun(config $ n_weeks * config $ n_prices, g $ nextGaussian())
    %>% matrix(ncol=config $ n_weeks) %>% t)
  state <- list(noise=noise, price_history=c(100), scale=scale)
  state $ max_revenue <- maximum_revenue(state)
  state
}

rep_each <- function(x, each)
  map2(x, each, ~ rep(.x, .y)) %>% do.call(c, .)

eval_prices <- function(state, prices){
  list_modify(state, price_history=prices) %>%
    summarise_state %>%
    `$`(revenue) %>% max
}

maximum_revenue <- function(state){
  (rerun(length(config $ price_levels), seq(0, config $ n_weeks - 1))
    %>% cross(.filter=function(...) sum(...) != config $ n_weeks)
    %>% map(partial(rep_each, config $ price_levels))
    %>% map(partial(eval_prices, state))
    %>% unlist
    %>% max)
}

compute_lift <- function(prices){
  config $ lift[match(prices, config $ price_levels)]
}

compute_demand <- function(state){
  mean_demand <- compute_lift(state $ price_history) * state $ scale
  noise <- state $ noise [seq_along(state $ price_history),
                          match(state $ price_history,
                                config $ price_levels)] %>%
    as.matrix %>% diag %>% unlist
  pmax(mean_demand * 0.2 * noise + mean_demand, 0) %>% as.integer
}


is_season_over <- function(state){
  history <- summarise_state(state)
  (max(history $ t) >= config $ n_weeks
    || min(history $ inventory) <= 0
    || min(history $ price) <= min(config $ price_levels))
}

summarise_state <- function(state){
  prices <- c(config $ price_levels[1], state $ price_history)
  demand <- c(0, compute_demand(state))
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

complete_state <- function(state){
  T <- length(state $ price_history)
  state $ price_history <- c(
    state $ price_history,
    rep(state $ price_history[T], config $ n_weeks - T))
}

copy_list <- function(src, target){
    for(name in names(src)){
      target[[name]] <- src[[name]]
    }
}
