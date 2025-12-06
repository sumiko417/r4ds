# 18.1 Introduction
# 18.1.1 Prerequisites
library(tidyverse)

# 18.2 Explicit missing values
# 18.2.1 Last observation carried forward
treatment <- tribble(
  ~person, ~treatment, ~response,
  "Derrick Whitmore", 1, 7,
  NA, 2, 10,
  NA, 3, NA,
  "Katherine Burke", 1, 4
)
treatment |>
  fill(everything())
treatment |>
  fill(everything(), .direction = "up")
# 18.2.2 Fixed values
x <- c(1, 4, NA, 7, NA)
coalesce(x, 0)
x <- c(1, 4, 5, 7, -99)
na_if(x, -99)
# 18.2.3 NaN
x <- c(NA, NaN)
x * 10
x == 1
is.na(x)
0 / 0
0 * Inf
Inf - Inf
sqrt(-1)

# 18.3 Implicit missing values
stocks <- tibble(
  year  = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr   = c(1, 2, 3, 4, 2, 3, 4),
  price = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)
# 18.3.1 Pivoting
stocks |>
  pivot_wider(
    names_from = qtr,
    values_from = price
  )
# 18.3.2 Complete
stocks |>
  complete(year, qtr)
stocks |>
  complete(year = 2019:2021, qtr)
stocks |>
  complete(year = 2019:2021, qtr = 1:5)
# 18.3.3 Joins
library(nycflights13)
flights |>
  distinct(faa = dest) |>
  anti_join(airports)
?airports
flights |>
  distinct(tailnum) |>
  anti_join(planes)
?planes
# 18.3.4 Exercises
flights |>
  distinct(carrier, tailnum) |>
  anti_join(planes) |>
  group_by(carrier) |>
  summarise(n = n()) |>
  arrange(desc(n))

# 18.4 Factors and empty groups
health <- tibble(
  name   = c("Ikaia", "Oletta", "Leriah", "Dashay", "Tresaun"),
  smoker = factor(c("no", "no", "no", "no", "no"), levels = c("yes", "no")),
  age    = c(34, 88, 75, 47, 56),
)
health |> count(smoker)
health |> count(smoker, .drop = FALSE)
ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete()
ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

health |>
  group_by(smoker, .drop = FALSE) |>
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  )
# A vector containing two missing values
x1 <- c(NA, NA)
length(x1)
# A vector containing nothing
x2 <- numeric()
length(x2)

x <- numeric(0)
max(x) # -Inf
min(x) # Inf
new_data <- c(5, 10, 2)
max(c(max(x), new_data))
# max(-Inf, 5, 10, 2) → 10
min(c(min(x), new_data))
# min(Inf, 5, 10, 2) → 2
health |>
  group_by(smoker) |>
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  ) |>
  complete(smoker)
