# 10.1 Introduction
# 10.1.1 Prerequisites
library(tidyverse)

# 10.2 Questions

# 10.3 Variation
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.5)
# 10.3.1 Typical values
smaller <- diamonds |>
  filter(carat < 3)
ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)
# 10.3.2 Unusual values
ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5)
ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
unusual <- diamonds |>
  filter(y < 3 | y > 20) |>
  select(price, x, y, z) |>
  arrange(y)
unusual
# 10.3.3 Exercises
ggplot(diamonds, aes(x = x)) +
  geom_histogram(binwidth = 0.5)
ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5)
ggplot(diamonds, aes(x = z)) +
  geom_histogram(binwidth = 0.5)

ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 2000)
price <- diamonds |>
  filter(price < 500) |>
  select(price, x, y, z) |>
  arrange(price)

p1 <- diamonds |>
  filter(carat == 0.99) |>
  select(price, x, y, z) |>
  arrange(price)

p2 <- diamonds |>
  filter(carat == 1) |>
  select(price, x, y, z) |>
  arrange(price)

gridExtra::grid.arrange(
  diamonds |>
    ggplot(aes(x = y)) +
    geom_histogram(binwidth = 0.1) +
    ylim(0, 1000) +
    xlim(0, 10) +
    labs(subtitle = "xlim and ylim remove data outside the limits, \neg. counts > 1000; or the observation at zero"),
  diamonds |>
    ggplot(aes(x = y)) +
    geom_histogram(binwidth = 0.1) +
    coord_cartesian(
      ylim = c(0, 1000),
      xlim = c(0, 10)
    ) +
    labs(subtitle = "coord_cartesian preserves data outside the limits, \neg. counts > 1000; or the observation at zero"),
  ncol = 2
)

# 10.4 Unusual values
diamonds2 <- diamonds |>
  filter(between(y, 3, 20))
diamonds2 <- diamonds |>
  mutate(y = if_else(y < 3 | y > 20, NA, y))
ggplot(diamonds2, aes(x = x, y = y)) +
  geom_point()
ggplot(diamonds2, aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

nycflights13::flights |>
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |>
  ggplot(aes(x = sched_dep_time)) +
  geom_freqpoly(aes(color = cancelled), binwidth = 1 / 4)
# 10.4.1 Exercises
ggplot(diamonds2, aes(x = y)) +
  geom_histogram()
ggplot(diamonds2, aes(x = y)) +
  geom_bar()

# Set a random seed for reproducibility
set.seed(123)
# Create a sample dataset with missing values
n <- 200
df <- data.frame(
  Category = sample(
    x = c("A", "B", "C", "D"),
    size = n,
    replace = TRUE
  ),
  Value = rnorm(n)
)
# Introduce missing values
df$Value[sample(1:n, 40)] <- NA
df$Category[sample(1:n, 40)] <- NA
# Create plots to demonstrate
gridExtra::grid.arrange(
  ggplot(df, aes(x = Value)) +
    geom_histogram(col = "grey", fill = "lightgrey") +
    theme_minimal() +
    labs(subtitle = "Histogram drops the missing values"),
  ggplot(df, aes(x = Category)) +
    geom_bar(col = "grey", fill = "lightgrey") +
    theme_minimal() +
    labs(subtitle = "Bar Chart includes missing values as a category"),
  ncol = 2
)
mean(df$Value)
mean(df$Value, na.rm = TRUE)

nycflights13::flights |>
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |>
  ggplot(aes(x = sched_dep_time)) +
  geom_freqpoly(aes(color = cancelled), binwidth = 1 / 4) +
  facet_wrap(~cancelled,
    scales = "free_y"
  )

# 10.5 Covariation
# 10.5.1 A categorical and a numerical variable
ggplot(diamonds, aes(x = price)) +
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)
ggplot(diamonds, aes(x = price, y = after_stat(density))) +
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot()
ggplot(mpg, aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot()
ggplot(mpg, aes(x = hwy, y = fct_reorder(class, hwy, median))) +
  geom_boxplot()
# 10.5.1.1 Exercises
nycflights13::flights |>
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |>
  ggplot(aes(x = sched_dep_time, y = after_stat(density))) +
  geom_freqpoly(aes(color = cancelled), binwidth = 1 / 2)

diamonds
install.packages("corrplot")
library(corrplot)
diamonds |>
  select(-c(cut, color, clarity)) |>
  cor() |>
  corrplot::corrplot(method = "number")
diamonds |>
  ggplot(aes(
    x = cut,
    y = carat
  )) +
  geom_boxplot()

ggplot(mpg, aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot() +
  coord_flip()
ggplot(mpg, aes(y = fct_reorder(class, hwy, median), x = hwy)) +
  geom_boxplot()

install.packages("lvplot")
library(lvplot)
ggplot(mpg, aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_lv()

diamonds |>
  ggplot(aes(
    x = cut,
    y = price
  )) +
  geom_violin()

diamonds |>
  ggplot(aes(price)) +
  geom_histogram() +
  facet_wrap(~cut, scales = "free")

diamonds |>
  ggplot(aes(price)) +
  geom_freqpoly(aes(color = cut)) +
  scale_color_brewer(palette = "Dark2")

diamonds |>
  ggplot(aes(price)) +
  geom_density(aes(color = cut))

install.packages("ggbeeswarm")
library(ggbeeswarm)
g <- diamonds |>
  filter(carat > 2.5) |>
  filter(cut %in% c("Very Good", "Premium", "Ideal")) |>
  ggplot(aes(
    x = cut,
    y = price
  )) +
  theme_light() +
  labs(x = "Cut", y = "Price")

gridExtra::grid.arrange(
  g + geom_point(alpha = 0.5) + labs(title = "geom_point()"),
  g + geom_jitter(alpha = 0.5) + labs(title = "geom_jitter"),
  g + geom_quasirandom(alpha = 0.5) + labs(title = "geom_quasirandom"),
  g + geom_beeswarm(alpha = 0.5) + labs(title = "geom_beeswarm")
)
# 10.5.2 Two categorical variables
ggplot(diamonds, aes(x = cut, y = color)) +
  geom_count()
diamonds |>
  count(color, cut)
diamonds |>
  count(color, cut) |>
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = n))
# 10.5.2.1 Exercises
diamonds |>
  count(cut, color) |>
  ggplot(aes(
    x = color,
    y = cut
  )) +
  geom_tile(aes(fill = n)) +
  scale_fill_continuous(type = "viridis") +
  labs(x = "Color", y = "Cut", fill = "Number of Diamonds") +
  geom_text(aes(label = n),
    color = "white"
  )

diamonds |>
  count(cut, color) |>
  # Create a factor to use for colouring the text
  # (since some colours in fil are darker)
  mutate(col_n = if_else(cut %in% c("Fair", "Good"),
    true = "Group1",
    false = "Group2"
  )) |>
  ggplot(aes(
    x = color,
    y = n,
    fill = cut,
    label = n
  )) +
  geom_bar(
    position = "fill",
    stat = "identity"
  ) +
  geom_text(
    aes(color = col_n),
    # adjust position so that numbers appear in middle of each segment
    position = position_fill(vjust = 0.5)
  ) +
  theme_classic() +
  scale_color_manual(values = c("white", "black")) +
  labs(
    x = "Colour of the diamonds",
    y = "Proportion of Diamonds",
    fill = "Cut"
  ) +
  guides(color = "none")

df <- nycflights13::flights |>
  group_by(month, dest) |>
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |>
  ungroup() |>
  # Removing destinations which have data missing for any one or more
  # months, so that our tile plot appears nice
  pivot_wider(
    names_from = month,
    values_from = avg_dep_delay
  ) |>
  drop_na() |>
  pivot_longer(
    cols = -dest,
    names_to = "Month",
    values_to = "avg_dep_delay"
  ) |>
  mutate(Month = as.numeric(Month)) |>
  mutate(month = as.factor(Month))

df |>
  ggplot(aes(
    y = dest,
    x = month,
    fill = avg_dep_delay
  )) +
  geom_tile(aes(group = month)) +
  scale_fill_viridis_c() +
  labs(
    x = "Month of the Year",
    y = "Destinations",
    fill = "Avg. Dep. Delay \n(in minutes)"
  ) +
  theme(axis.text.y = element_text(size = 5))
# 10.5.3 Two numerical variables
smaller <- diamonds |>
  filter(carat < 3)
ggplot(smaller, aes(x = carat, y = price)) +
  geom_point()
ggplot(smaller, aes(x = carat, y = price)) +
  geom_point(alpha = 1 / 100)
ggplot(smaller, aes(x = carat, y = price)) +
  geom_bin2d()
install.packages("hexbin")
ggplot(smaller, aes(x = carat, y = price)) +
  geom_hex()
ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)), varwidth = TRUE)
# 10.5.3.1 Exercises
gridExtra::grid.arrange(
  smaller |>
    ggplot(aes(x = price)) +
    geom_freqpoly(aes(color = cut_width(carat, width = 0.5)),
      lwd = 1, alpha = 0.8
    ),
  smaller |>
    ggplot(aes(x = price)) +
    geom_freqpoly(aes(color = cut_number(carat, n = 7)),
      lwd = 1, alpha = 0.8
    ),
  nrow = 2
)

gridExtra::grid.arrange(
  smaller |>
    ggplot(aes(
      x = price,
      y = carat
    )) +
    geom_boxplot(aes(group = cut_width(price, width = 2000)),
      outlier.alpha = 0.2
    ) +
    theme_minimal() +
    labs(
      x = "Price (in $)",
      title = "Boxplot",
      y = "Carat"
    ) +
    coord_flip(),
  smaller |>
    ggplot(aes(x = carat)) +
    geom_freqpoly(aes(color = cut_number(price, n = 7)),
      lwd = 1, alpha = 0.8
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "YlOrRd") +
    labs(
      x = "Carat of the Diamond",
      color = "Price (in $)",
      y = "Number of diamonds",
      title = "Frequency Polygon"
    ) +
    scale_y_continuous(n.breaks = 4),
  nrow = 2
)

diamonds |>
  mutate(big_c = ifelse(carat < 3,
    "Small Diamonds < 3 carats",
    "Big Diamonds > 3 Carats"
  )) |>
  mutate(big_c = factor(big_c,
    levels = c(
      "Small Diamonds < 3 carats",
      "Big Diamonds > 3 Carats"
    )
  )) |>
  ggplot(aes(
    x = price,
    y = carat
  )) +
  geom_boxplot(aes(group = cut_width(price, width = 2000)),
    outlier.alpha = 0.2
  ) +
  theme_minimal() +
  labs(
    x = "Price (in $)",
    y = "Carats"
  ) +
  coord_flip() +
  facet_wrap(~big_c,
    ncol = 2,
    scales = "free_x"
  )

diamonds |>
  ggplot(aes(
    x = price,
    y = carat
  )) +
  geom_boxplot(aes(group = cut_width(price, width = 2000)),
    outlier.alpha = 0.2
  ) +
  geom_smooth(
    se = FALSE, color = "darkgrey",
    lwd = 1.5, alpha = 0.3
  ) +
  facet_wrap(~cut, nrow = 5) +
  scale_x_continuous(labels = scales::comma_format(prefix = "$")) +
  coord_flip() +
  labs(x = "Carat", y = "Price")

diamonds |>
  filter(x >= 4) |>
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_number(carat, 20)))

# 10.6 Patterns and models
install.packages("tidymodels")
library(tidymodels)
diamonds <- diamonds |>
  mutate(
    log_price = log(price),
    log_carat = log(carat)
  )
diamonds_fit <- linear_reg() |>
  fit(log_price ~ log_carat, data = diamonds)
diamonds_aug <- augment(diamonds_fit, new_data = diamonds) |>
  mutate(.resid = exp(.resid))
ggplot(diamonds_aug, aes(x = carat, y = .resid)) +
  geom_point()
ggplot(diamonds_aug, aes(x = cut, y = .resid)) +
  geom_boxplot()

# 10.7 Summary