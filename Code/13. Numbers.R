# 13.1 Introduction
# 13.1.1 Prerequisites
library(tidyverse)
library(nycflights13)

# 13.2 Making numbers
x <- c("1.2", "5.6", "1e3")
parse_double(x)
x <- c("$1,234", "USD 3,513", "59%")
parse_number(x)

# 13.3 Counts
flights |>
  count(dest, sort = TRUE) |>
  view() |>
  print(n = Inf)

flights |>
  group_by(dest) |>
  summarize(
    n = n(),
    delay = mean(arr_delay, na.rm = TRUE)
  )

n()

flights |>
  group_by(dest) |>
  summarize(carriers = n_distinct(carrier)) |>
  arrange(desc(carriers))

flights |>
  group_by(tailnum) |>
  summarize(miles = sum(distance))
# is equivalent to sum, count&wt
flights |> count(tailnum, wt = distance)

flights |>
  group_by(dest) |>
  summarize(n_cancelled = sum(is.na(dep_time)))
# 13.3.1 Exercises
flights |>
  count(month, wt = is.na(dep_time))
?count

flights |> count(dest, sort = TRUE)
flights |>
  group_by(dest) |>
  summarize(n = n()) |>
  arrange(desc(n))
flights |> count(tailnum, wt = distance)
flights |>
  group_by(tailnum) |>
  summarize(total = sum(distance))

# 13.4 Numeric transformations
# 13.4.1 Arithmetic and recycling rules
x <- c(1, 2, 10, 20)
x / 5
x / c(5, 5, 5, 5)
x * c(1, 2)
x * c(1, 2, 3)
# == apply recycling rule while %in% does not
flights |>
  filter(month == c(1, 2))
flights |>
  filter(month %in% c(1, 2))
# 13.4.2 Minimum and maximum
df <- tribble(
  ~x, ~y,
  1, 3,
  5, 2,
  7, NA,
)
df |>
  mutate(
    min = pmin(x, y, na.rm = TRUE),
    max = pmax(x, y, na.rm = TRUE)
  )
df |>
  mutate(
    min = min(x, y, na.rm = TRUE),
    max = max(x, y, na.rm = TRUE)
  )
# 13.4.3 Modular arithmetic
1:10 %/% 3
1:10 %% 3
flights |>
  mutate(
    hour = sched_dep_time %/% 100,
    minute = sched_dep_time %% 100,
    .keep = "used",
    mmax = max(minute),
    hmax = max(hour),
    mmin = min(minute),
    hmin = min(hour)
  )

flights |>
  group_by(hour = sched_dep_time %/% 100) |>
  summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) |>
  filter(hour > 1) |>
  ggplot(aes(x = hour, y = prop_cancelled)) +
  geom_line(color = "grey50") +
  geom_point(aes(size = n))
# 13.4.4 Logarithms
# 13.4.5 Rounding
round(123.456)
round(123.556)
round(123.456, 2) # two digits
round(123.456, 1) # one digit
round(123.456, -1) # round to nearest ten
round(123.456, -2) # round to nearest hundred
round(c(1.5, 2.5))
x <- 123.456
floor(x)
ceiling(x)
# Round down to nearest two digits
floor(x / 0.01) * 0.01
# Round up to nearest two digits
ceiling(x / 0.01) * 0.01
# Round to nearest multiple of 4
round(x / 4) * 4
# Round to nearest 0.25
round(x / 0.25) * 0.25
# 13.4.6 Cutting numbers into ranges
x <- c(1, 2, 5, 10, 15, 20)
cut(x, breaks = c(0, 5, 10, 15, 20))
cut(x, breaks = c(0, 5, 10, 100))
cut(x,
  breaks = c(0, 5, 10, 15, 20),
  labels = c("sm", "md", "lg", "xl")
)
y <- c(NA, -10, 5, 10, 30)
cut(y, breaks = c(0, 5, 10, 15, 20))
cut(x, breaks = c(0, 5, 10, 15, 20), right = FALSE)
z <- c(0, 1, 2, 5, 10, 15, 20)
cut(z, breaks = c(0, 5, 10, 15, 20), include.lowest = TRUE)
?cut
# 13.4.7 Cumulative and rolling aggregates
x <- 1:10
cumsum(x)
# 13.4.8 Exercises
df <- tibble(x = seq(from = -5, to = +5, by = 0.1))
g <- ggplot(df, aes(x = x)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(breaks = -5:5)
gridExtra::grid.arrange(
  g + geom_line(aes(y = sin(x))) + labs(title = "sin(x)"),
  g + geom_line(aes(y = cos(x))) + labs(title = "cos(x)"),
  g + geom_line(aes(y = tan(x))) + labs(title = "tan(x)"),
  g + geom_line(aes(y = asin(x))) + labs(title = "asin(x)"),
  g + geom_line(aes(y = acos(x))) + labs(title = "acos(x)"),
  g + geom_line(aes(y = atan(x))) + labs(title = "atan(x)"),
  g + geom_line(aes(y = sinh(x))) + labs(title = "sinh(x)"),
  g + geom_line(aes(y = cosh(x))) + labs(title = "cosh(x)"),
  nrow = 2
)

flights |>
  filter(month == 1, day == 1) |>
  ggplot(aes(x = sched_dep_time, y = dep_delay)) +
  geom_point()

?flights

flights |>
  filter(month == 1, day == 1) |>
  mutate(
    shour = sched_dep_time %/% 100,
    sminute = sched_dep_time %% 100,
    stime = shour + sminute / 60
  ) |>
  ggplot(aes(x = stime, y = dep_delay)) +
  geom_point(size = 0.5) +
  scale_x_continuous(breaks = seq(0, 24, 4))

flights |>
  mutate(
    rdep_time = round(dep_time / 5) * 5,
    rarr_time = round(arr_time / 5) * 5,
    .keep = "used"
  )

# 13.5 General transformations
# 13.5.1 Ranks
x <- c(1, 2, 2, 3, 4, NA)
min_rank(x)
min_rank(desc(x))
df <- tibble(x = x)
df |>
  mutate(
    row_number = row_number(x),
    dense_rank = dense_rank(x),
    percent_rank = percent_rank(x),
    cume_dist = cume_dist(x)
  )
df <- tibble(id = 1:10)
df |>
  mutate(
    row0 = row_number() - 1,
    three_groups = row0 %% 3,
    three_in_each_group = row0 %/% 3
  )
# 13.5.2 Offsets
x <- c(2, 5, 11, 11, 19, 35)
lag(x)
lead(x)
x - lag(x)
x == lag(x)
lag(x, 2)
# 13.5.3 Consecutive identifiers
events <- tibble(
  time = c(0, 1, 2, 3, 5, 10, 12, 15, 17, 19, 20, 27, 28, 30)
)
events <- events |>
  mutate(
    diff1 = time - lag(time, default = first(time)),
    diff2 = time - lag(time, default = last(time)),
    diff3 = time - lag(time),
    has_gap = diff1 >= 5
  )
events
?lag
events |> mutate(
  group = cumsum(has_gap)
)
df <- tibble(
  x = c("a", "a", "a", "b", "c", "c", "d", "e", "a", "a", "b", "b"),
  y = c(1, 2, 3, 2, 4, 1, 3, 9, 4, 8, 10, 199)
)
df |>
  group_by(id = consecutive_id(x)) |>
  slice_head(n = 1)
# 13.5.4 Exercises
flights |>
  mutate(
    rank_delay = min_rank(pick(dep_delay, arr_delay)),
    .keep = "used",
  ) |>
  arrange(desc(rank_delay)) |>
  slice_head(n = 10)

flights |>
  select(tailnum, dep_delay, arr_delay) |>
  group_by(tailnum) |>
  summarize(
    avg1 = mean(dep_delay, na.rm = TRUE),
    avg2 = mean(arr_delay),
    n = n()
  ) |>
  filter(n > 5) |>
  arrange(desc(avg1))

flights
flights |>
  mutate(delay = arr_delay > 0) |>
  group_by(hour = sched_dep_time %/% 100) |>
  summarize(
    prop_delay = mean(delay, na.rm = TRUE),
    n = n()
  ) |>
  ggplot(aes(x = hour, y = prop_delay)) +
  geom_line(color = "grey50") +
  geom_point(aes(size = n)) +
  scale_x_continuous(breaks = seq(5, 24, 2))

flights |>
  group_by(dest) |>
  filter(row_number() < 4)
flights |>
  group_by(dest) |>
  filter(row_number(dep_delay) < 4)
flights2 <- flights |>
  group_by(dest) |>
  summarize(n = n())

flights3 <- flights |>
  group_by(dest) |>
  summarize(
    total = sum(dep_delay),
    prop = dep_delay / total
  )

flights |>
  filter(is.na(dep_delay))

flights |>
  mutate(hour = dep_time %/% 100) |>
  group_by(year, month, day, hour) |>
  summarize(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n > 5)

flights |>
  mutate(hour = dep_time %/% 100) |>
  group_by(year, month, day, hour) |>
  summarize(
    curr_delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(
    pre_delay = lag(curr_delay),
    diff = curr_delay - lag(curr_delay, default = first(curr_delay))
  ) |>
  filter(n > 5) |>
  ggplot(aes(x = curr_delay, y = pre_delay)) +
  geom_point(color = "grey50") +
  geom_smooth()
?flights

flights |>
  select(month, day, dest, tailnum, dep_time, arr_time, air_time) |>
  group_by(dest) |>
  mutate(
    min_air_time = min(air_time, na.rm = TRUE),
    avg_time = mean(air_time, na.rm = TRUE),
    ratio1 = air_time / avg_time,
    ratio2 = air_time / min_air_time
  ) |>
  ungroup() |>
  arrange(desc(ratio2)) |>
  slice_head(n = 100)

flights6 <- flights |>
  select(month, day, dest, carrier, tailnum, dep_delay) |>
  group_by(dest) |>
  mutate(
    carriers = n_distinct(carrier),
  ) |>
  filter(carriers > 2) |>
  group_by(dest, carrier) |>
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE),
  ) |>
  arrange(dest, avg_delay)

# 13.6 Numeric summaries
# 13.6.1 Center
flights |>
  group_by(year, month, day) |>
  summarize(
    mean = mean(dep_delay, na.rm = TRUE),
    median = median(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = mean, y = median)) +
  geom_abline(slope = 1, intercept = 0, color = "white", linewidth = 2) +
  geom_point()
# 13.6.2 Minimum, maximum, and quantiles
flights |>
  group_by(year, month, day) |>
  summarize(
    max = max(dep_delay, na.rm = TRUE),
    q95 = quantile(dep_delay, 0.95, na.rm = TRUE),
    .groups = "drop"
  )
# 13.6.3 Spread
flights |>
  group_by(origin, dest) |>
  summarize(
    distance_iqr = IQR(distance),
    n = n(),
    .groups = "drop"
  ) |>
  filter(distance_iqr > 0)

flights7 <- flights |>
  select(origin, dest, distance) |>
  group_by(origin, dest) |>
  summarize(
    distance_iqr = IQR(distance),
    n = n(),
    .groups = "drop"
  )
# 13.6.4 Distributions
flights |>
  filter(dep_delay < 120) |>
  ggplot(aes(x = dep_delay, group = interaction(day, month))) +
  geom_freqpoly(binwidth = 5, alpha = 1 / 5)
# 13.6.5 Positions
flights |>
  group_by(year, month, day) |>
  summarize(
    first_dep = first(dep_time, na_rm = TRUE),
    fifth_dep = nth(dep_time, 5, na_rm = TRUE),
    last_dep = last(dep_time, na_rm = TRUE)
  )
flights |>
  group_by(year, month, day) |>
  mutate(r = min_rank(sched_dep_time)) |>
  filter(r %in% c(1, max(r)))
?nth
# 13.6.6 With mutate()
# 13.6.7 Exercises
df1 <-
  flights |>
  group_by(tailnum) |>
  summarize(
    mean_dep_delay = mean(dep_delay, na.rm = TRUE),
    median_dep_delay = median(dep_delay, na.rm = TRUE),
    trim_mean_dep_delay = mean(dep_delay, na.rm = TRUE, trim = 0.025),
    mean_air_time_loss = mean(arr_delay - dep_delay, na.rm = TRUE),
    median_air_time_loss = median(arr_delay - dep_delay, na.rm = TRUE)
  )

df1 <- inner_join(df1, planes, by = "tailnum")

df1 |>
  group_by(engine) |>
  summarize(
    median_delay = mean(median_dep_delay, na.rm = TRUE),
    nos = n()
  ) |>
  mutate(is_positive = if_else(median_delay > 0, +1, -1)) |>
  ggplot(aes(
    x = median_delay,
    y = engine,
    fill = factor(is_positive),
    label = paste0("Flights = ", nos)
  )) +
  geom_bar(stat = "identity") +
  geom_text(hjust = 0.5) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-3, 5, 1)) +
  scale_fill_manual(values = c("#119644", "#ed523e")) +
  labs(
    x = "Average of the Median Depature Delays of each airplane (in minutes)",
    y = NULL,
    title = "Turbo-jet aircrafts have highest departure delays on average"
  ) +
  theme(
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "blank",
    panel.grid = element_blank()
  )
?flights

flights |>
  mutate(
    speed = (distance / air_time)
  ) |>
  ggplot(aes(x = speed, color = dest)) +
  geom_boxplot()

flights |>
  mutate(speed = 60 * distance / air_time) |>
  group_by(dest) |>
  summarise(
    nos = n(),
    mean = mean(speed, na.rm = TRUE),
    sd = sd(speed, na.rm = TRUE),
    CV = sd / mean
  ) |>
  arrange(desc(CV)) |>
  slice_head(n = 5)

flights |>
  filter(dest == "EGE")
flights |>
  filter(dest == "EGE") |>
  group_by(origin, distance) |>
  count()

flights |>
  filter(dest == "EGE") |>
  group_by(origin, distance) |>
  mutate(count1 = n()) |>
  distinct(origin, dest, distance, count1) |>
  ggplot(aes(x = origin, y = distance, size = count1)) +
  geom_point(alpha = 1 / 5)

?geom_point
