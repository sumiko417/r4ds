# install.packages("tidyverse")
# 5.1 Introduction
# 5.1.1 Prerequisites
library(tidyverse)
# 5.2 Tidy data
# 5.2.1 Exercises
# 5.3 Lengthening data
# 5.3.1 Data in column names
billboard
billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank"
  )
billboard |>
  pivot_longer(
    cols = !c(artist, track, date.entered),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )
billboard_longer <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  ) |>
  mutate(
    week = parse_number(week)
  )
billboard_longer
billboard_longer |>
  ggplot(aes(x = week, y = rank, group = track)) +
  geom_line(alpha = 0.25) +
  scale_y_reverse()
# 5.3.2 How does pivoting work?
df <- tribble(
  ~id,  ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  120,  125
)
df |>
  pivot_longer(
    cols = starts_with("bp"),
    names_to = "measurement",
    values_to = "value"
  )
# 5.3.3 Many variables in column names
z <- who2
who2 |>
  pivot_longer(
    cols = !c(country, year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count",
    values_drop_na = TRUE
  )
# 5.3.4 Data and variable names in the column headers
household
household |>
  pivot_longer(
    cols = !c(family),
    names_to = c("dob", "child"),
    names_sep = "_",
    values_to = "count",
    values_drop_na = TRUE
  )
household |>
  pivot_longer(
    cols = !family,
    names_to = c(".value", "child"),
    names_sep = "_",
    values_drop_na = TRUE
  )
# 5.4 Widening data
cms_patient_experience
cms_patient_experience |>
  distinct(measure_cd, measure_title)
cms_patient_experience |>
  pivot_wider(
    names_from = measure_cd,
    values_from = prf_rate
  )
cms_patient_experience |>
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd,
    values_from = prf_rate
  )
# 5.4.1 How does pivot_wider() work?
df <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "B",        "bp1",    140,
  "B",        "bp2",    115,
  "A",        "bp2",    120,
  "A",        "bp3",    105
)
df
df |>
  pivot_wider(
    names_from = measurement,
    values_from = value
  )
df |>
  pivot_wider(
    names_from = measurement,
    values_from = value
  )
df |>
  distinct(measurement) |>
  pull()
df |>
  select(-measurement, -value) |>
  distinct()
df |>
  select(-measurement, -value) |>
  distinct() |>
  mutate(x = NA, y = NA, z = NA)
df <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "A",        "bp1",    102,
  "A",        "bp2",    120,
  "B",        "bp1",    140,
  "B",        "bp2",    115
)
z <- df |>
  pivot_wider(
    names_from = measurement,
    values_from = value
  )
z
df |>
  dplyr::summarise(n = dplyr::n(), .by = c(id, measurement)) |>
  dplyr::filter(n > 1L)
df |>
  group_by(id, measurement) |>
  summarize(n = n(), .groups = "drop") |>
  filter(n > 1)
vignette("pivot", package = "tidyr")
# 5.5 Summary