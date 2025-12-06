# 14.1 Introduction
# 14.1.1 Prerequisites
library(tidyverse)
install.packages("babynames")
library(babynames)

# 14.2 Creating a string
string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'
# 14.2.1 Escapes
double_quote <- "\""
double_quote
cat(double_quote)
single_quote <- "'"
single_quote
cat(single_quote)
?cat()
backslash <- "\\"
cat(backslash)
x <- c(single_quote, double_quote, backslash)
x
str_view(x)
# 14.2.2 Raw strings
tricky <- "double_quote <- \"\\\"\" # or '\"'
single_quote <- '\\'' # or \"'\""
str_view(tricky)
tricky <- r"(double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'")"
str_view(tricky)
tricky1 <- r"---(double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'")---"
str_view(tricky1)
# 14.2.3 Other special characters
?Quotes
x <- c("one\ntwo", "one\ttwo", "\u00b5", "\U0001f604")
x
str_view(x)
# 14.2.4 Exercises
test1 <- "He said \"That's amazing!\""
str_view(test1)
test2 <- r"--(\a\b\c\d)--"
str_view(test2)
test3 <- r"--(\\\\\\)--"
str_view(test3)
test4 <- "\\\\\\\\\\\\"
str_view(test4)
test3 == test4

x <- "This\u00a0is\u00a0tricky"
x
y <- "\u00a0"
y
cat(y)
str_view("\u00a0")

# 14.3 Creating many strings from data
# 14.3.1 str_c()
str_c("x", "y")
str_c("x", "y", "z")
str_c("Hello ", c("John", "Susan"))
df <- tibble(name = c("Flora", "David", "Terra", NA))
df |> mutate(greeting = str_c("Hi ", name, "!"))
df |>
  mutate(
    greeting1 = str_c("Hi ", coalesce(name, "you"), "!"),
    greeting2 = coalesce(str_c("Hi ", name, "!"), "Hi!")
  )
# 14.3.2 str_glue()
df |> mutate(greeting = str_glue("Hi {name}!"))
df |> mutate(greeting = str_glue("{{Hi {name}!}}"))
# 14.3.3 str_flatten()
str_flatten(c("x", "y", "z"))
str_flatten(c("x", "y", "z"), ", ")
str_flatten(c("x", "y", "z"), ", ", last = ", and ")
df <- tribble(
  ~name, ~fruit,
  "Carmen", "banana",
  "Carmen", "apple",
  "Marvin", "nectarine",
  "Terence", "cantaloupe",
  "Terence", "papaya",
  "Terence", "mandarin"
)
df |>
  group_by(name) |>
  summarize(fruits = str_flatten(fruit, ", "))
# 14.3.4 Exercises
str_c("hi ", NA)
str_c(letters[1:2], letters[1:3])
paste0("hi ", NA)
paste0(letters[1:2], letters[1:3])

paste("hello", "world")
paste0("hello", "world")
str_c("hello", "world")
paste("hello", "world", sep = "")
vec1 <- c("Hello", "Hi")
vec2 <- c("Amy", "Tom", "Neal")
paste(vec1, vec2)
paste0(vec1, vec2)
str_c(vec1, vec2)
vec1 <- c(vec1, "Hallo")
str_c(vec1, vec2)
str_c(vec1, vec2, sep = " ")
food <- c("Beef", "Pork", "Chicken")
price <- c(6, 5, 5)
price <- c(6, 5)
price <- c(6)
str_c("The price of ", food, " is ", price)
str_glue("The price of {food} is {price}")
str_glue("I'm {age} years old and live in {country}")

age <- c(6, 5, 5)
country <- c("A", "B", "C")
str_c("I'm ", age, " years old and live in ", country)
str_glue("I'm {age} years old and live in {country}")

title <- c("A", "B", "C")
test5 <- str_c("\\section{", title, "}")
str_view(test5)
test6 <- str_glue("\\section{{{title}}}")
str_view(test6)
test5 == test6
cat(test5)
cat(test6)

# 14.4 Extracting data from strings
# 14.4.1 Separating into rows
df1 <- tibble(x = c("a,b,c", "d,e", "f"))
df1
df1 |>
  separate_longer_delim(x, delim = ",")
df2 <- tibble(x = c("1211", "131", "21"))
df2 |>
  separate_longer_position(x, width = 1)
# 14.4.2 Separating into columns
df3 <- tibble(x = c("a10.1.2022", "b10.2.2011", "e15.1.2015"))
df3 |>
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", "edition", "year")
  )
df3 |>
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", NA, "year")
  )
df4 <- tibble(x = c("202215TX", "202122LA", "202325CA"))
df4 |>
  separate_wider_position(
    x,
    widths = c(year = 4, age = 2, state = 2)
  )
df4 |>
  separate_wider_position(
    x,
    widths = c(year = 4, 2, state = 2)
  )
# 14.4.3 Diagnosing widening problems
df <- tibble(x = c("1-1-1", "1-1-2", "1-3", "1-3-2", "1"))
df |>
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z")
  )
debug <- df |>
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "debug"
  )
debug
debug |> filter(!x_ok)
debug <- df |>
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y"),
    too_few = "debug",
    too_many = "merge"
  )
debug

df |>
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "align_start"
  )
df |>
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "align_end"
  )

df <- tibble(x = c("1-1-1", "1-1-2", "1-3-5-6", "1-3-2", "1-3-5-7-9"))
df |>
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z")
  )
debug1 <- df |>
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "debug"
  )
debug1 |> filter(!x_ok)
df |>
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "drop"
  )
df |>
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "merge"
  )

# 14.5 Letters
# 14.5.1 Length
str_length(c("a", "R for data science", NA))
babynames |>
  count(length = str_length(name), wt = n)
?babynames
babynames |>
  filter(str_length(name) == 15) |>
  count(name, wt = n, sort = TRUE)
# 14.5.2 Subsetting
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub("a", 1, 5)
babynames |>
  mutate(
    first = str_sub(name, 1, 1),
    last = str_sub(name, -1, -1)
  )
# 14.5.3 Exercises
# count() is a shortcut for group_by() + summarize(n = n()).
babynames |>
  mutate(
    middle1 = floor((str_length(name) + 1) / 2),
    middle2 = ceiling((str_length(name) + 1) / 2),
    first = str_sub(name, middle1, middle1),
    last = str_sub(name, middle2, middle2)
  )
babynames |>
  mutate(
    name_length = str_length(name),
    middle_letter_start = if_else(name_length %% 2 == 0,
      name_length / 2,
      (name_length / 2) + 0.5
    ),
    middle_letter_end = if_else(name_length %% 2 == 0,
      (name_length / 2) + 1,
      (name_length / 2) + 0.5
    ),
    middle_letter = str_sub(name,
      start = middle_letter_start,
      end = middle_letter_end
    )
  )
babynames |>
  mutate(
    length = str_length(name),
    first = str_sub(name, 1, 1),
    last = str_sub(name, -1, -1)
  ) |>
  group_by(year) |>
  count(length, wt = n) |>
  summarize(mean = weighted.mean(length, w = n)) |>
  ggplot(aes(x = year, y = mean)) +
  geom_point()

library(dplyr)
library(stringr)
library(ggplot2)
library(babynames)

# Add derived features: name length, first letter, last letter
babynames_features <- babynames |>
  mutate(
    length = str_length(name),
    first = str_sub(name, 1, 1),
    last = str_sub(name, -1, -1)
  )

# Trend in name length over time
length_trend <- babynames_features |>
  group_by(year, length) |>
  summarize(total = sum(n), .groups = "drop") |>
  group_by(year) |>
  mutate(prop = total / sum(total))

ggplot(
  length_trend,
  aes(
    x = year,
    y = prop,
    color = factor(length)
  )
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Trends in Baby Name Length Over Time",
    x = "Year", y = "Proportion of Babies",
    color = "Name Length"
  ) +
  theme_minimal()

# First letters
first_letter_trend <- babynames_features |>
  group_by(year, first) |>
  summarize(total = sum(n), .groups = "drop") |>
  group_by(year) |>
  mutate(prop = total / sum(total)) |>
  ungroup()

# Plot: top first letters in recent years
top_first <- first_letter_trend |>
  filter(year == max(year)) |>
  slice_max(prop, n = 10) |>
  pull(first)

ggplot(
  filter(first_letter_trend, first %in% top_first),
  aes(
    x = year,
    y = prop,
    color = first
  )
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Popularity of First Letters of Baby Names Over Time",
    x = "Year", y = "Proportion of Babies",
    color = "First Letter"
  ) +
  theme_minimal()

# Last letters
last_letter_trend <- babynames_features |>
  group_by(year, last) |>
  summarize(total = sum(n), .groups = "drop") |>
  group_by(year) |>
  mutate(prop = total / sum(total)) |>
  ungroup()

top_last <- last_letter_trend |>
  filter(year == max(year)) |>
  slice_max(prop, n = 10) |>
  pull(last)

ggplot(
  filter(last_letter_trend, last %in% top_last),
  aes(
    x = year,
    y = prop,
    color = last
  )
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Popularity of Last Letters of Baby Names Over Time",
    x = "Year", y = "Proportion of Babies",
    color = "Last Letter"
  ) +
  theme_minimal()

# 14.6 Non-English text
# 14.6.1 Encoding
charToRaw("Hadley")
library(readr)
x1 <- "text\nEl Ni\xf1o was particularly bad this year"
read_csv(x1)$text
x2 <- "text\n\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
read_csv(x2)$text
read_csv(x1, locale = locale(encoding = "Latin1"))$text
#> [1] "El Niño was particularly bad this year"
read_csv(x2, locale = locale(encoding = "Shift-JIS"))$text
#> [1] "こんにちは"
#>
# Original Latin1 string
x1 <- "text\nEl Ni\xf1o was particularly bad this year"
# Convert to UTF-8
x1_utf8 <- iconv(x1, from = "latin1", to = "UTF-8")
# Read CSV from string
df <- read_csv(I(x1_utf8))
# 14.6.2 Letter variations
u <- c("\u00fc", "u\u0308")
str_view(u)
str_length(u)
str_sub(u, 1, 1)
u[[1]] == u[[2]]
t1 <- u[[1]]
t2 <- u[[2]]
t3 <- u[1]
t4 <- u[2]
is.vector(t1)
is.vector(t3)
str_equal(u[[1]], u[[2]])
my_list <- list(
  numbers = 1:3,
  letters = c("a", "b", "c"),
  nested = list(x = 10, y = 20)
)
sub_list <- my_list[1]
print(sub_list)
typeof(sub_list) # "list"
length(sub_list) # 1

element <- my_list[[1]]
print(element)
typeof(element) # "integer"
length(element) # 3
# 14.6.3 Locale-dependent functions
stringi::stri_locale_list()
str_to_upper(c("i", "ı"))
str_to_upper(c("i", "ı"), locale = "tr")
str_sort(c("a", "c", "ch", "h", "z"))
str_sort(c("a", "c", "ch", "h", "z"), locale = "cs")

# 14.7 Summary
x <- c(single_quote, double_quote, backslash)
writeLines(x)
str_view(x)
x <- c("one\ntwo", "one\ttwo", "\u00b5", "\U0001f604")
x
str_view(x)
str_flatten(c("x", "y", "z"), ", ", last = ", and ")
?paste
paste("1st", "2nd", "3rd", collapse = ", ")
paste(c("1st", "2nd", "3rd"), collapse = ", ")
paste("1st", "2nd", "3rd", sep = ", ")
