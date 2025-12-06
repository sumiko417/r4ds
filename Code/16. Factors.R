# 16.1 Introduction
# 16.1.1 Prerequisites
library(tidyverse)

# 16.2 Factor basics
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
sort(x1)
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(x1, levels = month_levels)
y1
y2 <- factor(x2, levels = month_levels)
y2
y2 <- fct(x2, levels = month_levels)
factor(x1)
x1 <- c("Dec", "Apr", "Jan", "Mar")
fct(x1)
levels(x1)
levels(y2)

csv <- "
month,value
Jan,12
Feb,56
Mar,12"

df <- read_csv(csv, col_types = cols(month = col_factor(month_levels)))
df$month

# 16.3 General Social Survey
gss_cat
?gss_cat
gss_cat |>
  count(race)
# 16.3.1 Exercises
levels(gss_cat$rincome)
col_level <- gss_cat$rincome %in% no_levels
?fct_relevel

no_levels <- levels(gss_cat$rincome)[c(1:3, 16)]
gss_cat |>
  mutate(col_level = rincome %in% no_levels) |>
  ggplot(aes(
    y = fct_relevel(rincome,
      "Not applicable",
      after = 3
    ),
    fill = col_level
  )) +
  geom_bar() +
  labs(
    x = "Number of respondents", y = NULL,
    title = "Income Levels of respondents in General Social Survey"
  )

gss_cat |>
  ggplot(aes(
    y = relig
  )) +
  geom_bar()

gss_cat |>
  ggplot(aes(
    y = partyid
  )) +
  geom_bar()
levels(gss_cat$relig)
levels(gss_cat$denom)
test1 <- gss_cat |>
  select(relig, denom) |>
  distinct()
test1
gss_cat |>
  group_by(relig) |>
  summarise(n = n_distinct(denom)) |>
  arrange(desc(n))
gss_cat |>
  group_by(relig) |>
  summarise(n = n_distinct(denom)) |>
  arrange(desc(n)) |>
  ggplot(aes(y = reorder(relig, n), x = n)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    x = "Number of denominations",
    y = NULL,
    title = "Only Protestant religion has demoninations within it"
  )

# 16.4 Modifying factor order
relig_summary <- gss_cat |>
  group_by(relig) |>
  summarize(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(x = tvhours, y = relig)) +
  geom_point()
ggplot(relig_summary, aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
  geom_point()

relig_summary |>
  mutate(
    relig = fct_reorder(relig, tvhours)
  ) |>
  ggplot(aes(x = tvhours, y = relig)) +
  geom_point()

rincome_summary <- gss_cat |>
  group_by(rincome) |>
  summarize(
    age = mean(age, na.rm = TRUE),
    n = n()
  )

ggplot(rincome_summary, aes(x = age, y = fct_reorder(rincome, age))) +
  geom_point()

ggplot(
  rincome_summary,
  aes(
    x = age,
    y = fct_relevel(rincome, "Not applicable", after = 0)
  )
) +
  geom_point()

by_age <- gss_cat |>
  filter(!is.na(age)) |>
  count(age, marital) |>
  group_by(age) |>
  mutate(
    prop = n / sum(n)
  )

ggplot(by_age, aes(x = age, y = prop, color = marital)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1")

ggplot(
  by_age,
  aes(
    x = age,
    y = prop,
    color = fct_reorder(marital, prop)
  )
) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "marital")

ggplot(
  by_age,
  aes(
    x = age,
    y = prop,
    color = fct_reorder2(marital, age, prop)
  )
) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "marital")
gss_cat |>
  mutate(marital = marital |> fct_infreq() |> fct_rev()) |>
  ggplot(aes(x = marital)) +
  geom_bar()
# 16.4.1 Exercises
gss_cat |>
  drop_na() |>
  # mutate(tvhours = as_factor(tvhours)) |>
  ggplot(aes(x = tvhours)) +
  geom_bar()

gss_cat
levels(gss_cat$marital)
levels(gss_cat$race)
levels(gss_cat$rincome)
levels(gss_cat$partyid)
levels(gss_cat$relig)
levels(gss_cat$denom)

keep(gss_cat, is.factor) |> names()
gss_cat |>
  ggplot(aes(x = marital)) +
  geom_bar()

# 16.5 Modifying factor levels
gss_cat |> count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
      "Republican, strong"    = "Strong republican",
      "Republican, weak"      = "Not str republican",
      "Independent, near rep" = "Ind,near rep",
      "Independent, near dem" = "Ind,near dem",
      "Democrat, weak"        = "Not str democrat",
      "Democrat, strong"      = "Strong democrat"
    )
  ) |>
  count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
      "Republican, strong"    = "Strong republican",
      "Republican, weak"      = "Not str republican",
      "Independent, near rep" = "Ind,near rep",
      "Independent, near dem" = "Ind,near dem",
      "Democrat, weak"        = "Not str democrat",
      "Democrat, strong"      = "Strong democrat",
      "Other"                 = "No answer",
      "Other"                 = "Don't know",
      "Other"                 = "Other party"
    )
  )

gss_cat |>
  mutate(
    partyid = fct_collapse(partyid,
      "other" = c("No answer", "Don't know", "Other party"),
      "rep" = c("Strong republican", "Not str republican"),
      "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
      "dem" = c("Not str democrat", "Strong democrat")
    )
  ) |>
  count(partyid)

gss_cat |>
  mutate(relig = fct_lump_lowfreq(relig)) |>
  count(relig)

gss_cat |>
  mutate(relig = fct_lump_n(relig, n = 10)) |>
  count(relig, sort = TRUE)
# 16.5.1 Exercises
gss_cat |>
  mutate(
    partyid = fct_collapse(partyid,
      "other" = c("No answer", "Don't know", "Other party"),
      "rep" = c("Strong republican", "Not str republican"),
      "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
      "dem" = c("Not str democrat", "Strong democrat")
    )
  ) |>
  count(year, partyid) |>
  group_by(year) |>
  mutate(
    prop = n / sum(n)
  ) |>
  ggplot(
    aes(
      x = year,
      y = prop,
      color = fct_reorder2(partyid, year, prop)
    )
  ) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "partyid")

levels(gss_cat$rincome)

gss_cat |>
  mutate(
    rincome = fct_collapse(rincome,
      "low" = c(
        "$4000 to 4999", "$3000 to 3999",
        "$1000 to 2999", "Lt $1000"
      ),
      "medium" = c(
        "$8000 to 9999", "$7000 to 7999",
        "$6000 to 6999", "$5000 to 5999"
      ),
      "high" = c(
        "$25000 or more", "$20000 - 24999",
        "$15000 - 19999", "$10000 - 14999"
      ),
      "na" = c(
        "No answer", "Don't know",
        "Refused", "Not applicable"
      )
    )
  ) |>
  dplyr::pull(rincome) |>
  levels()

library("stringr")
gss_cat %>%
  mutate(
    rincome =
      fct_collapse(
        rincome,
        `Unknown` = c("No answer", "Don't know", "Refused", "Not applicable"),
        `Lt $5000` = c("Lt $1000", str_c(
          "$", c("1000", "3000", "4000"),
          " to ", c("2999", "3999", "4999")
        )),
        `$5000 to 10000` = str_c(
          "$", c("5000", "6000", "7000", "8000"),
          " to ", c("5999", "6999", "7999", "9999")
        )
      )
  ) %>%
  ggplot(aes(x = rincome)) +
  geom_bar() +
  coord_flip()

# 16.6 Ordered factors
ordered(c("a", "b", "d", "c"))
# [1] a b d c
# Levels: a < b < c < d
ordered(c("Jan", "Dec", "Nov", "May"))
# [1] Jan Dec Nov May
# Levels: Dec < Jan < May < Nov

# 16.7 Summary