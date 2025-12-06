# 19.1 Introduction
# 19.1.1 Prerequisites
library(tidyverse)
library(nycflights13)

# 19.2 Keys
# 19.2.1 Primary and foreign keys
airlines
airports
planes
weather
# 19.2.2 Checking primary keys
planes |>
  count(tailnum) |>
  filter(n > 1)
weather |>
  count(time_hour, origin) |>
  filter(n > 1)
planes |>
  filter(is.na(tailnum))
weather |>
  filter(is.na(time_hour) | is.na(origin))
# 19.2.3 Surrogate keys
flights |>
  count(time_hour, carrier, flight) |>
  filter(n > 1)
airports |>
  count(alt, lat) |>
  filter(n > 1)
zch <- airports |>
  count(alt, lat)
airports |>
  filter(alt == 13)
flights2 <- flights |>
  mutate(id = row_number(), .before = 1)
flights2
# 19.2.4 Exercises
weather |>
  count(year, month, day, hour, origin) |>
  filter(n > 1)
flights3 <- flights |>
  count(year, month, day) |>
  arrange(n) |>
  slice(1:10)
flights |>
  count(year, month, day) |>
  arrange(desc(n))
special_days <- tribble(
  ~year, ~month, ~day, ~holiday,
  2013, 01, 01, "New Years Day",
  2013, 07, 04, "Independence Day",
  2013, 11, 29, "Thanksgiving Day",
  2013, 12, 25, "Christmas Day"
)
library(Lahman)
?Batting
?People
?Salaries
library(Lahman)
Batting |>
  count(playerID, yearID, stint) |>
  filter(n > 1)
Batting |>
  as_tibble() |>
  group_by(playerID, yearID, stint) |>
  count() |>
  filter(n > 1)
zch1 <- Batting

# 19.3 Basic joins
# 19.3.1 Mutating joins
flights2 <- flights |>
  select(year, time_hour, origin, dest, tailnum, carrier)
flights2
flights2 |>
  left_join(airlines)
flights2 |>
  left_join(weather |> select(origin, time_hour, temp, wind_speed))
flights2 |>
  left_join(planes |> select(tailnum, type, engines, seats))
flights2 |>
  filter(tailnum == "N3ALAA") |>
  left_join(planes |> select(tailnum, type, engines, seats))
# 19.3.2 Specifying join keys
flights2 |>
  left_join(planes)
flights2 |>
  left_join(planes, join_by(tailnum))
flights2 |>
  left_join(airports, join_by(dest == faa))
flights2 |>
  left_join(airports, join_by(origin == faa))
# 19.3.3 Filtering joins
airports |>
  semi_join(flights2, join_by(faa == origin))
airports |>
  semi_join(flights2, join_by(faa == dest))
flights2 |>
  anti_join(airports, join_by(dest == faa)) |>
  distinct(dest)
flights2 |>
  anti_join(planes, join_by(tailnum)) |>
  distinct(tailnum)
# 19.3.4 Exercises
delayhours <- flights |>
  group_by(origin, time_hour) |>
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) |>
  arrange(desc(avg_delay), .by_group = TRUE) |>
  slice_head(n = 48) |>
  arrange(time_hour)
flights |>
  distinct(origin)
?weather
worst_hours <- flights %>%
  mutate(hour = sched_dep_time %/% 100) %>%
  group_by(origin, year, month, day, hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(dep_delay)) %>%
  slice(1:48)

top_dest <- flights2 |>
  count(dest, sort = TRUE) |>
  head(10)

flights2 |>
  semi_join(top_dest, join_by(dest == dest))

flights2 |>
  anti_join(weather) |>
  distinct(time_hour)

flights |>
  anti_join(planes, join_by(tailnum)) |>
  distinct(carrier, tailnum) |>
  count(carrier) |>
  arrange(desc(n))

all_carrs <- flights |>
  group_by(tailnum) |>
  distinct(carrier) |>
  summarise(carriers = paste0(carrier, collapse = ", ")) |>
  arrange(desc(str_length(carriers)))
planes |>
  left_join(all_carrs) |>
  view()

flights |>
  left_join(airports, join_by(dest == faa)) |>
  rename(
    "dest_lat" = lat,
    "dest_lon" = lon
  ) |>
  left_join(airports, join_by(origin == faa)) |>
  rename(
    "orgi_lat" = lat,
    "orfi_lon" = lon
  )

avg <- flights |>
  group_by(dest) |>
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE))

airports |>
  left_join(avg, join_by(faa == dest)) |>
  semi_join(flights, join_by(faa == dest)) |>
  ggplot(aes(x = lon, y = lat, colour = avg_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

flights |>
  mutate(Date = if_else((month == 6 & day == 13),
    "June 13, 2013",
    "Rest of the year"
  )) |>
  group_by(Date) |>
  summarise(average_departure_delay = mean(dep_delay, na.rm = TRUE))

# 19.4 How do joins work?
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

df1 <- tibble(key = c(1, 2, 2), val_x = c("x1", "x2", "x3"))
df1
df2 <- tibble(key = c(1, 2, 2), val_y = c("y1", "y2", "y3"))
df2
df1 |>
  inner_join(df2, join_by(key))
df1 |>
  inner_join(df2, join_by(key), relationship = "many-to-many")

# 19.5 Non-equi joins
x |> inner_join(y, join_by(key == key), keep = TRUE)
# 19.5.1 Cross joins
df <- tibble(name = c("John", "Simon", "Tracy", "Max"))
df |> cross_join(df)
# 19.5.2 Inequality joins
df <- tibble(id = 1:4, name = c("John", "Simon", "Tracy", "Max"))
df |> inner_join(df, join_by(id < id))
# 19.5.3 Rolling joins
parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03"))
)
set.seed(123)
employees <- tibble(
  name = sample(babynames::babynames$name, 100),
  birthday = ymd("2022-01-01") + (sample(365, 100, replace = TRUE) - 1)
)
employees
employees |>
  left_join(parties, join_by(closest(birthday >= party)))
employees |>
  anti_join(parties, join_by(closest(birthday >= party)))
# 19.5.4 Overlap joins
parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03")),
  start = ymd(c("2022-01-01", "2022-04-04", "2022-07-11", "2022-10-03")),
  end = ymd(c("2022-04-03", "2022-07-11", "2022-10-02", "2022-12-31"))
)
parties
parties |>
  inner_join(parties, join_by(overlaps(start, end, start, end), q < q)) |>
  select(start.x, end.x, start.y, end.y)
parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03")),
  start = ymd(c("2022-01-01", "2022-04-04", "2022-07-11", "2022-10-03")),
  end = ymd(c("2022-04-03", "2022-07-11", "2022-10-02", "2022-12-01"))
)
chk <- employees |>
  inner_join(parties,
    join_by(between(birthday, start, end)),
    unmatched = "error"
  )
x |> full_join(y, join_by(key == key))
x |> full_join(y, join_by(key == key), keep = TRUE)

parties |>
  inner_join(parties, join_by(overlaps(start, end, start, end))) |>
  select(start.x, end.x, start.y, end.y)
parties |>
  inner_join(parties, join_by(overlaps(start, end, start, end), q < q)) |>
  select(start.x, end.x, start.y, end.y)
