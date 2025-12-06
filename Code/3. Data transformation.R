# install.packages("tidyverse")
# library(tidyverse)
# 3.1 Introduction
# 3.1.1 Prerequisites
library(nycflights13)
# 3.1.2 nycflights13
flights
view(flights)
print(flights, width = Inf)
glimpse(flights)
# 3.1.3 dplyr basics
flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

# 3.2 Rows
# 3.2.1 filter()
flights |>
  filter(dep_delay > 120)
flights |>
  filter(month == 1 & day == 1)
flights |>
  filter(month == 1 | month == 2)
flights |>
  filter(month %in% c(1, 2))
jan1 <- flights |>
  filter(month == 1 & day == 1)
# 3.2.2 Common mistakes
flights |>
  filter(month = 1)
flights |>
  filter(month == 1 | 2)
# 3.2.3 arrange()
flights |>
  arrange(year, month, day, dep_time)
flights |>
  arrange(desc(dep_delay))
flights |>
  arrange(year, desc(dep_delay), desc(dep_time))
# 3.2.4 distinct()
flights |>
  distinct()
flights |>
  distinct(origin, dest)
flights |>
  distinct(origin, dest, .keep_all = TRUE)
flights |>
  count(origin, dest, sort = TRUE)
# 3.2.5 Exercises
flights |>
  filter(arr_delay >= 120)

flights |>
  filter(dest %in% c("IAH", "HOU"))

flights |>
  filter(carrier %in% c("UA", "AA", "DL"))

flights |>
  filter(month %in% c(7, 8, 9))

flights |>
  filter(arr_delay > 120 & dep_delay <= 0)

flights |>
  filter(dep_delay >= 60 & dep_delay - arr_delay > 30)

flights |>
  arrange(desc(dep_delay)) |>
  arrange(sched_dep_time)

?flights

flights |>
  arrange(desc(distance / air_time)) |>
  relocate(flight, year, month, day)

flights |>
  filter(year == 2013) |>
  group_by(carrier, flight) |>
  mutate(n = n()) |>
  arrange(desc(n)) |>
  relocate(n, carrier, flight)

flights |>
  arrange(desc(distance)) |>
  relocate(carrier, flight, distance)

flights |>
  arrange(distance) |>
  relocate(carrier, flight, distance)

# 3.3 Columns
# 3.3.1 mutate()
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day
  )
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )
flights |>
  filter(carrier == "UA")
mutate(
  gain = dep_delay - arr_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours,
  .keep = "used"
)
# 3.3.2 select()
flights |>
  select(year, month, day)
flights |>
  select(year:day)
flights |>
  select(!year:day)
flights |>
  select(where(is.character))
flights |>
  select(tail_num = tailnum)
# 3.3.3 rename()
flights |>
  rename(tail_num = tailnum)
# 3.3.4 relocate()
flights |>
  relocate(time_hour, air_time)
flights |>
  relocate(year:dep_time, .after = time_hour)
flights |>
  relocate(starts_with("arr"), .before = dep_time)
# 3.3.5 Exercises
flights |>
  mutate(
    delay1 = dep_delay,
    delay2 = dep_time - sched_dep_time,
    .keep = "used"
  ) |>
  filter(delay1 != delay2)

variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights |>
  select(any_of(variables))

flights |> select(contains("TIME"))
flights |> select(contains("TIME", ignore.case = FALSE))

flights |>
  rename(air_time_min = air_time) |>
  relocate(air_time_min)

# 3.4 The pipe
flights |>
  filter(dest == "IAH") |>
  mutate(speed = distance / air_time * 60) |>
  select(year:day, dep_time, carrier, flight, speed) |>
  arrange(desc(speed))
arrange(
  select(
    mutate(
      filter(
        flights,
        dest == "IAH"
      ),
      speed = distance / air_time * 60
    ),
    year:day, dep_time, carrier, flight, speed
  ),
  desc(speed)
)
flights1 <- filter(flights, dest == "IAH")
flights2 <- mutate(flights1, speed = distance / air_time * 60)
flights3 <- select(flights2, year:day, dep_time, carrier, flight, speed)
arrange(flights3, desc(speed))

# 3.5 Groups
# 3.5.1 group_by()
flights |>
  group_by(month)
# 3.5.2 summarize()
flights |>
  group_by(month) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )
flights |>
  group_by(month) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  )
flights |>
  group_by(month) |>
  summarize(
    avg_delay = mean(dep_delay),
    n = n()
  )
# 3.5.3 The slice_ functions
flights |>
  group_by(dest) |>
  slice_max(arr_delay, n = 1) |>
  relocate(dest)
flights |>
  # group_by(dest) |>
  slice_max(arr_delay, n = 1) |>
  relocate(dest)
# 3.5.4 Grouping by multiple variables
daily <- flights |>
  group_by(year, month, day)
daily
daily_flights <- daily |>
  summarize(n = n())
daily_flights1 <- daily_flights |>
  summarize(n = n())
daily_flights <- daily |>
  summarize(
    n = n(),
    .groups = "drop_last"
  )
# 3.5.5 Ungrouping
daily |>
  ungroup()
daily |>
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    flights = n()
  )
# 3.5.6 .by
flight1 <- flights |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = month
  )
flights |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = month
  )
flight1 |>
  summarize(
    avg_delay = mean(delay, na.rm = TRUE),
    flights = n()
  )
flights |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = c(origin, dest)
  )
# 3.5.7 Exercises
flights |>
  group_by(carrier) |>
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE)) |>
  arrange(desc(avg_delay))

library(gt)
flights |>
  group_by(dest, carrier) |>
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) |>
  # taking the highest average delay flight at each airport
  slice_max(order_by = avg_delay, n = 1) |>
  ungroup() |>
  # for each airline, summarize the number of airports where it is
  # the most delayed airline
  summarise(n = n(), .by = carrier) |>
  slice_head(n = 5) |>
  arrange(desc(n)) |>
  rename(
    Carrier = carrier,
    `Number of Airports` = n
  ) |>
  gt()

flights |>
  group_by(dest) |>
  arrange(desc(dep_delay)) |>
  slice_head(n = 1) |>
  relocate(dest, carrier, flight) |>
  gt()

plot <- flights |>
  group_by(hour) |>
  summarize(delay = mean(dep_delay, na.rm = TRUE))
plot |> ggplot(
  aes(x = hour, y = delay)
) +
  geom_smooth()

flights |>
  group_by(hour) |>
  summarize(delay = mean(dep_delay, na.rm = TRUE)) |>
  ggplot(
    aes(x = hour, y = delay)
  ) +
  geom_smooth()

flights |>
  # group_by(dest) |>
  slice_min(sched_dep_time, n = -1)

flights |>
  group_by(dest) |>
  slice_min(sched_dep_time, n = 1)

flights |>
  slice_min(dep_delay, n = -5) |>
  relocate(dep_delay)

flights |>
  filter(is.na(dest))

flights |>
  group_by(month) |>
  slice_min(sched_dep_time, n = -1)

flights |>
  group_by(dest) |>
  slice_min(sched_dep_time, n = -1)

df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)

df |>
  group_by(y)

df |>
  arrange(y)

df |>
  group_by(y) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x), .groups = "drop")

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y, z) |>
  mutate(mean_x = mean(x))

# 3.6 Case study: aggregates and sample size
install.packages("Lahman")
library(Lahman)
batters <- Lahman::Batting |>
  group_by(playerID) |>
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )
batters
?Batting
batters |>
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 1 / 10) +
  geom_smooth(se = FALSE)
batters |>
  filter(n > 100) |>
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 1 / 10) +
  geom_smooth(se = FALSE)
batters |>
  arrange(desc(performance))

# 3.7 Summary