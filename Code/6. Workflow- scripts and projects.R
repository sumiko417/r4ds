# 6.1 Scripts
# 6.1.1 Running code
library(dplyr)
library(nycflights13)
not_cancelled <- flights |>
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled |>
  group_by(year, month, day) |>
  summarize(mean = mean(dep_delay))
z <- c(1, 2)
# 6.1.2 RStudio diagnostics
# 6.1.3 Saving and naming

# 6.2 Projects
# 6.2.1 What is the source of truth?
# 6.2.2 Where does your analysis live?
getwd()
# 6.2.3 RStudio projects
# 6.2.4 Relative and absolute paths

# 6.3 Exercises

# 6.4 Summary