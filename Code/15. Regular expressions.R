# 15.1 Introduction
# 15.1.1 Prerequisites
library(tidyverse)
library(babynames)

# 15.2 Pattern basics
str_view(fruit, "berry")
str_view(c("a", "ab", "ae", "bd", "ea", "eab"), "a.")
str_view(fruit, "a...e")
# ab? matches an "a", optionally followed by a "b".
str_view(c("a", "ab", "abb"), "ab?")
# ab+ matches an "a", followed by at least one "b".
str_view(c("a", "ab", "abb"), "ab+")
str_view(c("a", "ab", "abb"), "ab*")
str_view(words, "[aeiou]x[aeiou]")
str_view(words, "[^aeiou]y[^aeiou]")
str_view(fruit, "apple|melon|nut")
str_view(fruit, "aa|ee|ii|oo|uu")

# 15.3 Key functions
# 15.3.1 Detect matches
str_detect(c("a", "b", "c"), "[aeiou]")
babynames |>
  filter(str_detect(name, "x")) |>
  count(name, wt = n, sort = TRUE)
babynames |>
  filter(str_detect(name, "x")) |>
  count(name, sort = TRUE)
dt <- babynames
babynames |>
  # group_by(year) |>
  group_by(name) |>
  summarize(count_x = sum(str_detect(name, "x") * n)) |>
  filter(count_x > 0) |>
  arrange(desc(count_x))
babynames |>
  group_by(year) |>
  summarize(prop_x = mean(str_detect(name, "x"))) |>
  ggplot(aes(x = year, y = prop_x)) +
  geom_line()
str_detect(c("a", "b", "c"), "[aeiou]")
str_subset(c("a", "b", "c"), "[aeiou]")
str_which(c("a", "b", "c"), "[aeiou]")
# 15.3.2 Count matches
x <- c("apple", "banana", "pear")
str_count(x, "p")
str_count("abababa", "aba")
str_view("abababa", "aba")
babynames |>
  count(name) |>
  mutate(
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )
babynames |>
  count(name)
babynames |>
  filter(name == "Aaban")
babynames |>
  count(name) |>
  mutate(
    name = str_to_lower(name),
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )
# 15.3.3 Replace values
x <- c("apple", "pear", "banana")
str_replace_all(x, "[aeiou]", "-")
x <- c("apple", "pear", "banana")
str_remove_all(x, "[aeiou]")
str_replace_all(x, "[aeiou]", "")
# 15.3.4 Extract variables
df <- tribble(
  ~str,
  "<Sheryl>-F_34",
  "<Kisha>-F_45",
  "<Brandon>-N_33",
  "<Sharon>-F_38",
  "<Penny>-F_58",
  "<Justin>-M_41",
  "<Patricia>-F_84",
)
df |>
  separate_wider_regex(
    str,
    patterns = c(
      "<",
      name = "[A-Za-z]+",
      ">-",
      gender = ".",
      "_",
      age = "[0-9]+"
    )
  )
# 15.3.5 Exercises
babynames |>
  count(name) |>
  mutate(
    vowels = str_count(str_to_lower(name), "[aeiou]"),
    consonants = str_count(str_to_lower(name), "[^aeiou]"),
    len = str_length({
      name
    }),
    p = vowels / len
  ) |>
  arrange(desc(p))

x <- c("a/b/c/d/e")
y <- str_replace_all(x, "/", "\\\\")
y
cat(y)
z <- str_replace_all(y, "\\\\", "/")
cat(z)

df |>
  mutate(
    str = str_replace_all(str, "[A-Z]", tolower)
  )
df |>
  mutate(
    str = str_replace_all(str, "[A-Z]", "[a-z]")
  )
?str_replace_all
df1 <- tribble(
  ~str,
  "<Sheryl>-F_111-222-333",
  "<Kisha>-F_111-222.333",
  "<Brandon>-N_11-222-333",
  "<Sharon>-F_38",
  "<Penny>-F_58",
  "<Justin>-M_41",
  "<Patricia>-F_84",
)
df1 |>
  pull(str) |>
  str_detect("[0-9][0-9][0-9][-][0-9][0-9][0-9][-][0-9][0-9][0-9]")
df1 |>
  filter(str_detect(str, "[0-9]{3}-[0-9]{3}-[0-9]{3}"))

# 15.4 Pattern details
# 15.4.1 Escaping
# To create the regular expression \., we need to use \\.
dot <- "\\."
# But the expression itself only contains one \
str_view(dot)
#> [1] │ \.
# And this tells R to look for an explicit .
str_view(c("abc", "a.c", "bef"), "a\\.c")
#> [2] │ <a.c>

x <- "a\\b"
str_view(x)
#> [1] │ a\b
str_view(x, "\\\\")
#> [1] │ a<\>b
str_view(x, r"{\\}")
#> [1] │ a<\>b
str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")

# 15.4.2 Anchors
str_view(fruit, "^a")
str_view(fruit, "a$")
str_view(fruit, "apple")
str_view(fruit, "^apple$")
x <- c("summary(x)", "summarize(df)", "rowsum(x)", "sum(x)")
str_view(x, "sum")
str_view(x, "\\bsum\\b")
str_view("abc", c("$", "^", "\\b"))
str_replace_all("abc", c("$", "^", "\\b"), "--")
# 15.4.3 Character classes
x <- "abcd ABCD 12345 -!@#%."
str_view(x, "[abc]+")
str_view(x, "[a-z]+")
str_view(x, "[^a-z0-9]+")
str_view("a-b-c", "[a-c]")
str_view("a-b-c", "[a\\-c]")
# \d matches any digit;
# \D matches anything that isn’t a digit.
# \s matches any whitespace (e.g., space, tab, newline);
# \S matches anything that isn’t whitespace.
# \w matches any “word” character, i.e. letters and numbers;
# \W matches any “non-word” character.
x <- "abcd ABCD 12345 -!@#%."
str_view(x, "\\d+")
str_view(x, "\\D+")
str_view(x, "\\s+")
str_view(x, "\\S+")
str_view(x, "\\w+")
str_view(x, "\\W+")
# 15.4.4 Quantifiers
# 15.4.5 Operator precedence and parentheses
# 15.4.6 Grouping and capturing
str_view(fruit, "(..)\\1")
str_view(words, "^(..).*\\1$")
sentences |>
  str_replace("(\\w+) (\\w+) (\\w+)", "\\1 \\3 \\2") |>
  str_view()
sentences |>
  str_match("the (\\w+) (\\w+)") |>
  head()
df <- sentences |>
  str_match("the (\\w+) (\\w+)")
?slice
sentences |>
  str_match("the (\\w+) (\\w+)") |>
  as_tibble(.name_repair = "minimal") |>
  set_names("match", "word1", "word2")

x <- c("a g0ray cat", "a grey dog", "a gre0y dog", "0 gra0y dog")
str_match(x, "gr(e|a)(0)(y)")
x <- c("a gray cat", "a grey dog")
str_match(x, "gr(e|a)y")
str_match(x, "gr(?:e|a)y")
# 15.4.7 Exercises
# \"\'\\\\
# The string you want to match
input_string <- "\"'\\"
str_view(input_string)
# Pattern to match the literal string
match_pattern <- "\"\'\\\\"
str_view(match_pattern)
# Use str_detect to check if the string contains the pattern
if (str_detect(input_string, match_pattern)) {
  print("Pattern found in the input string.")
} else {
  print("Pattern not found in the input string.")
}
input_string1 <- "\"\'\\"
input_string2 <- "\"'\\"
str_view(input_string1)
str_view(input_string2)
input_string1 == input_string2

# The string you want to match
input_string3 <- "\"$^$\""
str_view(input_string3)

# Pattern to match the literal string
match_pattern <- "\"\\$\\^\\$\""
str_view(match_pattern)

# Use str_detect to check if the string contains the pattern
if (str_detect(input_string3, match_pattern)) {
  print("Pattern found in the input string.")
} else {
  print("Pattern not found in the input string.")
}

pattern1 <- "\"'\\\\"
pattern2 <- "\"\\$\\^\\$\""
text <- 'Here are the strings: "\'\\ and "$^$"'

grepl(pattern1, text) # TRUE
grepl(pattern2, text) # TRUE

df <- stringr::words
match_pattern <- "^y"
df |> str_detect(match_pattern)
str_view(df, "^y")
str_view(df, "^(?!y)")
str_view(df, "x$")
str_view(df, "^[A-Za-z]{3}$")
words |>
  str_subset(pattern = "\\b\\w{3}\\b")
match_pattern <- "\\b\\w{3}\\b"
str_view(match_pattern)
str_view(df, "\\b\\w{7}\\w*\\b")
str_view(df, "[aeiou][^aeiou]")
str_view(df, "[aeiou]{2,}")
str_view(df, "([aeiou][^aeiou]){2}")
str_view(df, "([aeiou][^aeiou])\\1")
df3 <- c(
  "airplane", "aeroplane", "aluminum", "aluminium",
  "analog", "analogue", "ass", "arse", "center", "centre",
  "defense", "defence", "donut", "doughnut", "gray", "grey",
  "modeling", "modelling", "skeptic", "sceptic",
  "summarize", "summarise"
)
df3
str_view(df3, "a(?:i|ero)plane")

str_view(df3, "alumini?um")
str_view(df3, "analog(ue)?")
str_view(df3, "a(ss|rse)")
str_view(df3, "cent(er|re)")
str_view(df3, "defen(c|s)e")
str_view(df3, "do(ugh)?nut")
str_view(df3, "gr(a|e)y")
str_view(df3, "model{1,2}ing")
str_view(df3, "s(k|c)eptic")
str_view(df3, "summari(z|s)e")

words |>
  str_replace("^([a-z]{1})(\\w*)([a-z]{1})$", "\\3\\2\\1") |>
  str_view()
words |>
  str_replace_all(
    pattern = "\\b(\\w)(\\w*)(\\w)\\b",
    replacement = "\\3\\2\\1"
  )
words |>
  str_replace("^([a-z])(\\w*)([a-z])$", "\\3\\2\\1") |>
  str_view()

df4 <- words |>
  str_replace("^([A-Za-z]{1})(\\w*)([A-Za-z]{1})$", "\\3\\2\\1")
df4
df5 <- words[df4 == words]
df5

# 15.5 Pattern control
# 15.5.1 Regex flags
bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))
x <- "Line 1\nLine 2\nLine 3"
str_view(x)
str_view(x, ".Line")
str_view(x, regex(".Line", dotall = TRUE))
x <- "Line 1\nLine 2\nLine 3"
str_view(x, "^Line")
str_view(x, regex("^Line", multiline = TRUE))
phone <- regex(
  r"(
    \(?     # optional opening parens
    (\d{3}) # area code
    [)\-]?  # optional closing parens or dash
    \ ?     # optional space
    (\d{3}) # another three numbers
    [\ -]?  # optional space or dash
    (\d{4}) # four more numbers
  )",
  comments = TRUE
)
str_extract(c("514\791-8141", "(123) 456 7890", "123456"), phone)
# 15.5.2 Fixed matches
str_view(c("", "a", "."), fixed("."))
str_view("x X", "X")
str_view("x X", fixed("X", ignore_case = TRUE))
str_view("x X", fixed("X"))
str_view("i İ ı I", fixed("İ", ignore_case = TRUE))
str_view("i İ ı I", coll("İ", ignore_case = TRUE, locale = "tr"))

# 15.6 Practice
# 15.6.1 Check your work
str_view(sentences, "^The")
str_view(sentences, "^The\\b")
str_view(sentences, "^The$")
str_view(sentences, "^She|He|It|They\\b")
str_view(sentences, "^(She|He|It|They)\\b")
pos <- c("He is a boy", "She had a good time")
neg <- c("Shells come from the sea", "Hadley said 'It's a great day'")
pattern <- "^(She|He|It|They)\\b"
str_detect(pos, pattern)
str_detect(neg, pattern)
# 15.6.2 Boolean operations
str_view(words, "^[^aeiou]+$")
str_view(words[!str_detect(words, "[aeiou]")])
str_view(words, "a.*b|b.*a")
words[str_detect(words, "a") & str_detect(words, "b")]
words[
  str_detect(words, "a") &
    str_detect(words, "e") &
    str_detect(words, "i") &
    str_detect(words, "o") &
    str_detect(words, "u")
]
# 15.6.3 Creating a pattern with code
str_view(sentences, "\\b(red|green|blue)\\b")
rgb <- c("red", "green", "blue")
str_c("\\b(", str_flatten(rgb, "|"), ")\\b")
str_view(colors())
cols <- colors()
cols <- cols[!str_detect(cols, "\\d")]
str_view(cols)
pattern <- str_c("\\b(", str_flatten(cols, "|"), ")\\b")
pattern
str_view(sentences, pattern)
# 15.6.4 Exercises
words[
  str_detect(words, "^x") |
    str_detect(words, "x$")
]
words[
  str_detect(words, "^x")
]
pattern <- str_c("^x|x$")
str_view(words, pattern)

words[
  str_detect(words, "^[aeiou]") &
    str_detect(words, "[^aeiou]$")
]
pattern <- str_c("^(i?)[aeiou].*[^aeiou]$")
str_view(words, pattern)

words[
  str_detect(words, "a") &
    str_detect(words, "e") &
    str_detect(words, "i") &
    str_detect(words, "o") &
    str_detect(words, "u")
]
pattern_all_vowels <- "(?i)^(?=.*a)(?=.*e)(?=.*i)(?=.*o)(?=.*u)"
str_subset(words, pattern_all_vowels)
p1 <- "(?i)^(?=.*c)(?=.*i)(?=.*)(?=.*e)"
str_subset(words, p1)
pattern_1a <- "\\b\\w*ie\\w*\\b"
pattern_1b <- "\\b\\w+ei\\w*\\b"
pattern_2a <- "\\b\\w*cei\\w*\\b"
pattern_2b <- "\\b\\w*cie\\w*\\b"
words[str_detect(words, pattern_1a)]
words[str_detect(words, pattern_1b)]
words[str_detect(words, pattern_2a)]
words[str_detect(words, pattern_2b)]

col_vec <- colours(distinct = TRUE)
col_vec
p1 <- str_view(col_vec[!str_detect(col_vec, "\\d")])
cat(p1, sep = ", ")
p1
p2 <- str_view(p1[!str_detect(p1, "\\b\\w*light\\w*\\b|\\b\\w*dark\\w*\\b")])
p2
p3 <- str_view(p1[str_detect(p1, "\\b\\w*light\\w*\\b|\\b\\w*dark\\w*\\b")])
p3
p4 <- p1[str_detect(p1, "\\b(?:light|dark)\\w*\\b")]
p5 <- p1[str_detect(p1, "\\b(light|dark)\\w*\\b")]
p4 == p5
colors <- c("lightblue", "darkred", "blue", "darkest", "highlight")
str_match(colors, "\\b(light|dark)\\w*\\b")
str_match(colors, "\\b(?:light|dark)\\w*\\b")
p3 <- str_view(p1[str_detect(p1, "\\b\\w*(light|dark)\\w*\\b")])
p3
# Extract all base R datasets into a character vector
base_r_packs <- data(package = "datasets")$results[, "Item"]
base_r_packs
# Remove all the names of grouping data.frames in parenthesis
base_r_packs1 <- str_replace_all(base_r_packs,
  pattern = "\\([^()]+\\)",
  replacement = ""
)
base_r_packs1
# Remove the whitespace, i.e., " " left after removing the parenthesis words
base_r_packs2 <- str_replace_all(base_r_packs1,
  pattern = "\\s+$",
  replacement = ""
)
# Create the regular expression
huge_regex <- str_c("\\b(", str_flatten(base_r_packs2, "|"), ")\\b")
test1 <- c("(text)", "(a(b)c)")
str_replace_all(test1,
  pattern = "\\([^()]+\\)",
  replacement = ""
)
str_detect(test1, "\\([^()]+\\)")

# 15.7 Regular expressions in other places
# 15.7.1 tidyverse
# 15.7.2 Base R
apropos("replace")
head(list.files(pattern = "\\.Rmd$"))

# 15.8 Summary
vignette("regular-expressions", package = "stringr")
install.packages("stringi")
