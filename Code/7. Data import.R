# 7.1 Introduction
# 7.1.1 Prerequisites
library(tidyverse)

# 7.2 Reading data from a file
students <- read_csv("data/students.csv")
students
# 7.2.1 Practical advice
students <- read_csv("data/students.csv", na = c("N/A", ""))
students |>
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )
install.packages("janitor")
library(janitor)
test <- janitor::clean_names(students)
test
students |> janitor::clean_names()
students |>
  janitor::clean_names() |>
  mutate(meal_plan = factor(meal_plan))
students <- students |>
  janitor::clean_names() |>
  mutate(
    meal_plan = factor(meal_plan),
    age = parse_number(if_else(age == "five", "5", age))
  )
# 7.2.2 Other arguments
read_csv(
  "a,b,c
  1,2,3
  4,5,6"
)
read_csv(
  "The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3",
  # skip = 2
  # comment = "The f"
  comment = "The"
)
read_csv(
  "# A comment I want to skip
  x,y,z
  1,2,3",
  comment = "#"
)
read_csv(
  "1,2,3
  4,5,6",
  col_names = FALSE
)
read_csv(
  "1,2,3
  4,5,6",
  col_names = c("x", "y", "z")
)
# 7.2.3 Other file types
# 7.2.4 Exercises
read_csv("x,y\n1,'a,b'", quote = "\'")
?read_csv
read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
read_csv("a,b\n\1")
read_csv("a,b\n1,2\na,b")
read_delim("a;b\n1;3", delim = ";")
read_csv2("a;b\n1;3")
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
annoying |>
  select(`1`)
annoying |> ggplot(
  aes(x = `1`, y = `2`)
) +
  geom_point()
annoying |>
  mutate(`3` = `2` / `1`) |>
  rename(
    "one" = `1`,
    "two" = `2`,
    "three" = `3`
  )
annoying |> janitor::clean_names()

# 7.3 Controlling column types
# 7.3.1 Guessing types
read_csv("
  logical,numeric,date,string
  TRUE,1,2021-01-15,abc
  false,4.5,2021-02-15,def
  T,Inf,2021-02-16,ghi
")
# 7.3.2 Missing values, column types, and problems
simple_csv <- "
  x
  10
  .
  20
  30"
read_csv(simple_csv)
df <- read_csv(
  simple_csv,
  col_types = list(x = col_double())
)
problems(df)
read_csv(simple_csv, na = ".")
# 7.3.3 Column types
another_csv <- "
x,y,z
1,2,3"
read_csv(
  another_csv
)
read_csv(
  another_csv,
  col_types = cols(.default = col_character())
)
read_csv(
  another_csv,
  col_types = cols_only(x = col_character())
)

# 7.4 Reading data from multiple files
sales_files <- c("data/01-sales.csv", "data/02-sales.csv", "data/03-sales.csv")
read_csv(sales_files, id = "file")
sales_files <- list.files("data", pattern = "sales\\.csv$", full.names = TRUE)
sales_files

# 7.5 Writing to a file
write_csv(students, "students.csv")
students
write_csv(students, "students-2.csv")
read_csv("students-2.csv")
write_rds(students, "students.rds")
read_rds("students.rds")
install.packages("arrow")
library(arrow)
write_parquet(students, "students.parquet")
read_parquet("students.parquet")

# 7.6 Data entry
tibble(
  x = c(1, 2, 5),
  y = c("h", "m", "g"),
  z = c(0.08, 0.83, 0.60)
)
tribble(
  ~x, ~y, ~z,
  1, "h", 0.08,
  2, "m", 0.83,
  5, "g", 0.60
)

# 7.7 Summary