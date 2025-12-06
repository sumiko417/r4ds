# 2.1 Coding basics
1 / 200 * 30
(59 + 73 + 2) / 3
sin(pi / 2)
x <- 3 * 4
x
primes <- c(2, 3, 5, 7, 11, 13)
primes * 2
primes - 1

# 2.2 Comments

# 2.3 What’s in a name?
x
this_is_a_really_long_name <- 2.5
this_is_a_really_long_name
r_rocks <- 2^3
r_rock
R_rocks

# 2.4 Calling functions
seq(from = 1, to = 10)
seq(1, 10)
x <- "hello world"
# x <- "hello

# 2.5 Exercises
my_variable <- 10
my_varıable

# libary(todyverse)
library(tidyverse)

# ggplot(dTA = mpg) +
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm")

my_bar_plot <- ggplot(mpg, aes(x = class)) +
  geom_bar()
my_scatter_plot <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave(filename = "mpg-plot.png", plot = my_bar_plot)
