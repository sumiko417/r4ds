# 17.1 Introduction
# 17.1.1 Prerequisites
library(tidyverse)
library(nycflights13)

# 17.2 Creating date/times
today()
now()
# 17.2.1 During import
csv <- "
  date,datetime
  2022-01-02,2022-01-02 05:12
"
read_csv(csv)
csv <- "
  date
  01/02/15
"
read_csv(csv, col_types = cols(date = col_date("%m/%d/%y")))
read_csv(csv, col_types = cols(date = col_date("%d/%m/%y")))
read_csv(csv, col_types = cols(date = col_date("%y/%m/%d")))
# 17.2.2 From strings
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")
ymd("2017-01-31", tz = "EST")
# 17.2.3 From individual components
flights |>
  select(year, month, day, hour, minute)
flights |>
  select(year, month, day, hour, minute) |>
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights |>
  filter(!is.na(dep_time), !is.na(arr_time)) |>
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) |>
  select(origin, dest, ends_with("delay"), ends_with("time"))
flights_dt |>
  ggplot(aes(x = dep_time)) +
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day
flights_dt |>
  filter(dep_time < ymd(20130102)) |>
  ggplot(aes(x = dep_time)) +
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes
# 17.2.4 From other types
as_datetime(today())
as_date(now())
as_datetime(60 * 60 * 10)
as_date(365 * 10 + 2)
# 17.2.5 Exercises
ymd(c("2010-10-10", "bananas"))

library(lubridate)
d1 <- "date
\"January 1, 2010\""
read_csv(d1, col_types = cols(date = col_date("%B %d, %Y")))
mdy("January 1, 2010")

d2 <- "
  date
  2015-Mar-07
"
read_csv(d2, col_types = cols(date = col_date("%Y-%b-%e")))
ymd("2015-Mar-07")

d3 <- "
  date
  06-Jun-2017
"
read_csv(d3, col_types = cols(date = col_date("%e-%b-%Y")))
dmy("06-Jun-2017")

d4 <- "
  date
  August 19 (2015)
  July 1 (2015)
"
read_csv(d4, col_types = cols(date = col_date("%B %d (%Y)")))
d4 <- c("August 19 (2015)", "July 1 (2015)")
mdy(d4)

d5 <- "
  date
  12/30/14
"
read_csv(d5, col_types = cols(date = col_date("%m/%d/%y")))
mdy("12/30/14")

t1 <- "
  time
  1705
"
read_csv(t1, col_types = cols(time = col_time("%H%M")))
times <- c("1705", "0930", "2210")
parsed <- hm(gsub("^(\\d{2})(\\d{2})$", "\\1:\\2", times))
parsed

t2 <- "
  time
  11:15:10.12 PM
"
read_csv(t2, col_types = cols(time = col_time("%I:%M:%OS %p")))
hms("11:15:10.12 PM")

# 17.3 Date-time components
# 17.3.1 Getting components
datetime <- ymd_hms("2026-07-08 12:34:56")
year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)
month(datetime, label = TRUE)
month(datetime, label = TRUE, abbr = FALSE)
wday(datetime, label = TRUE, abbr = FALSE)
flights_dt |>
  mutate(wday = wday(dep_time, label = TRUE)) |>
  ggplot(aes(x = wday)) +
  geom_bar()
flights_dt |>
  mutate(minute = minute(dep_time)) |>
  group_by(minute) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) |>
  ggplot(aes(x = minute, y = avg_delay)) +
  geom_line()

sched_dep <- flights_dt |>
  mutate(minute = minute(sched_dep_time)) |>
  group_by(minute) |>
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(sched_dep, aes(x = minute, y = avg_delay)) +
  geom_line()
# 17.3.2 Rounding
flights_dt |>
  count(week = floor_date(dep_time, "week")) |>
  ggplot(aes(x = week, y = n)) +
  geom_line() +
  geom_point()

flights_dt |>
  count(week = floor_date(dep_time, "week"))

flights_dt |>
  mutate(dep_hour = dep_time - floor_date(dep_time, "day")) |>
  ggplot(aes(x = dep_hour)) +
  geom_freqpoly(binwidth = 60 * 30)

flights_dt |>
  mutate(dep_hour = dep_time - floor_date(dep_time, "day")) |>
  select(dep_hour, dep_time)

flights_dt |>
  mutate(dep_hour = hms::as_hms(dep_time - floor_date(dep_time, "day"))) |>
  ggplot(aes(x = dep_hour)) +
  geom_freqpoly(binwidth = 60 * 30)
# 17.3.3 Modifying components
(datetime <- ymd_hms("2026-07-08 12:34:56"))
year(datetime) <- 2030
datetime
month(datetime) <- 01
datetime
hour(datetime) <- hour(datetime) + 1
datetime
update(datetime, year = 2030, month = 2, mday = 2, hour = 2)
update(ymd("2023-02-01"), mday = 30)
update(ymd("2023-02-01"), hour = 400)
# 17.3.4 Exercises
?flights
zchk <- flights
flights |>
  mutate(
    dep_time = make_datetime(year, month, day, dep_time %/% 100, dep_time %% 100),
    dep_day = round_date(dep_time, unit = "day")
  ) |>
  group_by(dep_day) |>
  summarise(mean_air_time = mean(air_time, na.rm = TRUE)) |>
  ggplot(aes(
    x = dep_day,
    y = mean_air_time
  )) +
  geom_point() +
  geom_smooth(span = 0.5)

flights |>
  filter(!is.na(dep_time) & !is.na(sched_dep_time)) |>
  mutate(
    a_dep_time = make_datetime(year, month, day, dep_time %/% 100, dep_time %% 100),
    s_dep_time = make_datetime(year, month, day, sched_dep_time %/% 100, sched_dep_time %% 100),
    diff = (a_dep_time - s_dep_time) / 60,
    comparison = diff == dep_delay
  ) |>
  # filter(diff!=dep_delay)
  filter(!comparison & is.na(arr_time))

df1 <- flights |>
  mutate(
    a_dep_time = make_datetime(year, month, day, dep_time %/% 100, dep_time %% 100),
    a_arr_time = make_datetime(year, month, day, arr_time %/% 100, arr_time %% 100),
    dur = a_arr_time - a_dep_time,
    comparison = dur == air_time,
    .keep = "used"
  ) |>
  filter(!comparison)

flights |>
  mutate(
    dep_time = make_datetime(sched_dep_time %/% 100, sched_dep_time %% 100),
    dep_hour = floor_date(dep_time, unit = "hour")
  ) |>
  group_by(dep_hour) |>
  summarise(mean_arr_delay = mean(arr_delay, na.rm = TRUE)) |>
  ggplot(aes(
    x = dep_hour,
    y = mean_arr_delay
  )) +
  geom_point() +
  geom_smooth(span = 0.5)

flights |>
  mutate(
    dep_day = make_date(year, month, day),
    wwday = wday(dep_day, label = TRUE)
  ) |>
  group_by(wwday) |>
  summarise(ave_dep_delay = mean(dep_delay, na.rm = TRUE)) |>
  ggplot(aes(
    x = wwday,
    y = ave_dep_delay
  )) +
  geom_bar(stat = "identity")

ggplot(diamonds, aes(x = carat)) +
  geom_density()
ggplot(flights_dt, aes(x = minute(sched_dep_time))) +
  geom_histogram(binwidth = 1)

flights_dt |>
  mutate(
    minute = minute(dep_time),
    early = dep_delay < 0
  ) |>
  group_by(minute) %>%
  summarise(
    early = mean(early, na.rm = TRUE),
    n = n()
  ) |>
  ggplot(aes(minute, early)) +
  geom_line()

# 17.4 Time spans
# 17.4.1 Durations
h_age <- today() - ymd("1979-10-14")
h_age
as.duration(h_age)
dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)
2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)
tomorrow <- today() + ddays(1)
tomorrow
last_year <- today() - dyears(1)
last_year
one_am <- ymd_hms("2026-03-08 01:00:00", tz = "America/New_York")
one_am
one_am + ddays(1)
# 17.4.2 Periods
one_am
one_am + days(1)
hours(c(12, 24))
dhours(c(12, 24))
days(7)
ddays(7)
months(1:6)
10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)
# A leap year
ymd("2024-01-01") + dyears(1)
ymd("2024-01-01") + years(1)
# Daylight saving time
one_am
one_am + ddays(1)
one_am + days(1)
flights_dt |>
  filter(arr_time < dep_time)

flights_dt <- flights |>
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight),
    sched_arr_time = sched_arr_time + days(overnight)
  )
flights_dt |>
  filter(arr_time < dep_time)
# 17.4.3 Intervals
years(1) / days(1)
dyears(1) / ddays(365)
y2023 <- ymd("2023-01-01") %--% ymd("2024-01-01")
y2024 <- ymd("2024-01-01") %--% ymd("2025-01-01")
y2023
y2024
y2023 / days(1)
y2024 / days(1)
# 17.4.4 Exercises
df_1 <- flights |>
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight),
    sched_arr_time = sched_arr_time + days(overnight),
    # days(!overnight)
    days(overnight),
    .keep = "used"
  )
df_2 <- flights |>
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight),
    sched_arr_time = sched_arr_time + days(overnight),
    days(!overnight),
    .keep = "used"
  )
seq(
  from = ymd("2015-01-01"),
  to = ymd("2015-12-31"),
  by = "1 month"
)
ymd("2015-01-01") + months(0:11)
seq(
  from = ymd(paste0(year(today()), "-01-01")),
  to = ymd(paste0(year(today()), "-12-31")),
  by = "1 month"
)
ymd(paste0(year(today()), "-01-01")) + months(0:11)
find_age <- function(x) {
  age <- floor((interval(parse_date(x), today())) / years(1))

  cat("You are", age, "years old.")
}
find_age("1991-05-30")

# 17.5 Time zones
Sys.timezone()
length(OlsonNames())
head(OlsonNames())
x1 <- ymd_hms("2024-06-01 12:00:00", tz = "America/New_York")
x1
x2 <- ymd_hms("2024-06-01 18:00:00", tz = "Europe/Copenhagen")
x2
x3 <- ymd_hms("2024-06-02 04:00:00", tz = "Pacific/Auckland")
x3
x1 - x2
x1 - x3
x4 <- c(x1, x2, x3)
x4
x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a
x4a - x4
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
x4b - x4