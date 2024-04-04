rm(list=ls())
library(tidyverse)

birthday_sample <- function(group_size = 23){
  possible_birthdays <- c(1:365, 1:366)
  return(sample(possible_birthdays, size = group_size, replace = FALSE))
}

birthday_match <- function(sample_input){
  sum(duplicated(sample_input)) > 0
}

many_samples <- function(reps=10, group_size=23){
  samples <- lapply(rep(group_size, reps), birthday_sample)
  matches <- sapply(samples, birthday_match)
  estimated_prob <- sum(matches) / reps
 
  return(estimated_prob)
}

estimated_prob <- many_samples()

estimated_prob

group_sizes <- 2:366
sample_size <- 1000

estimated_probs <- lapply(group_sizes, function(x){
  many_samples(reps = sample_size, group_size = x)
})

df <- data.frame(group_size = group_sizes, estimated_prob = unlist(estimated_probs))

birthday_plot <- ggplot(df %>% filter(group_size < 100), aes(group_size, estimated_prob))
plot + geom_point()

#Circle


make_df <- function(n = 1000) {
  x_coords <- runif(n, -1, 1)
  y_coords <- runif(n, -1, 1)
  points <- data.frame(x = x_coords, y = y_coords)
  points$radius <- sqrt(points$x^2 + points$y^2)
  points$in_circ <- points$radius <= 1
  pi_est <- 4 * sum(points$in_circ) / n
  return(list(pi_est = pi_est, df = points))
}

df <- make_df(10000)

plot <- ggplot(df$df, aes(x = df$df$x, y = df$df$y, color = in_circ, alpha = radius)) + geom_point()
plot

#x0 <- estimate_pi(1e4)
#x1 <- estimate_pi(1e5)
#x2 <- estimate_pi(1e6)
#x3 <- estimate_pi(1e7)


