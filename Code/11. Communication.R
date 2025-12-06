# 11.1 Introduction
# 11.1.1 Prerequisites
library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)

# 11.2 Labels
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    color = "Car type",
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )

df <- tibble(
  x = 1:10,
  y = cumsum(x^2)
)

ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(x[i]),
    y = quote(sum(x[i]^2, i == 1, n))
  )
# 11.2.1 Exercises
data(mpg)
mpg |>
  ggplot(aes(
    x = displ,
    y = hwy,
    color = as_factor(class)
  )) +
  geom_point() +
  labs(
    title = "Higher displacement vehicles have lower mileage",
    subtitle = "2-seater sports cars have high mileage despite higher displacements",
    x = "Engine Displacement (in liters)",
    y = "Highway Mileage (miles per gallon)",
    caption = "Data from mpg dataset",
    color = "Vehicle Class"
  ) +
  theme(legend.position = "right") +
  scale_color_viridis_d()

?mpg
data(mpg)
mpg |>
  ggplot(aes(
    x = cty,
    y = hwy,
    color = drv,
    shape = drv
  )) +
  geom_point() +
  labs(
    x = "City MPG",
    y = "Highway MPG",
    color = "Type of \ndrive train",
    shape = "Type of \ndrive train"
  )

diamonds |>
  mutate(res_lm = (lm(diamonds$y ~ diamonds$x)$residuals)) |>
  filter(x >= 4) |>
  mutate(res_lm = res_lm < -1 | res_lm > 1) |>
  rowid_to_column() |>
  mutate(rowid = ifelse(res_lm, rowid, NA)) |>
  ggplot(aes(
    x = x,
    y = y,
    label = scales::comma(rowid)
  )) +
  geom_point(alpha = 0.2) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11)) +
  ggrepel::geom_text_repel(size = 3) +
  labs(
    x = "Length of diamond, in mm (x)",
    y = "Width of diamond, in mm (y)",
    title = "Certain diamonds have an abnormal shape",
    subtitle = "The labelled diamonds, with their IDs shown, have excessive length or width",
    caption = "Data from diamonds data-set, ggplot2"
  ) +
  theme_light()

# 11.3 Annotations
label_info <- mpg |>
  group_by(drv) |>
  arrange(desc(displ)) |>
  slice_head(n = 1) |>
  mutate(
    drive_type = case_when(
      drv == "f" ~ "front-wheel drive",
      drv == "r" ~ "rear-wheel drive",
      drv == "4" ~ "4-wheel drive"
    )
  ) |>
  select(displ, hwy, drv, drive_type)

label_info

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  geom_text(
    data = label_info,
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold", size = 5, hjust = "right", vjust = "bottom"
  ) +
  theme(legend.position = "none")

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  geom_label_repel(
    data = label_info,
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold", size = 5, nudge_y = 2
  ) +
  theme(legend.position = "none")

potential_outliers <- mpg |>
  filter(hwy > 40 | (hwy > 20 & displ > 5))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_text_repel(data = potential_outliers, aes(label = model)) +
  geom_point(data = potential_outliers, color = "red") +
  geom_point(
    data = potential_outliers,
    color = "red", size = 3, shape = "circle open"
  )

trend_text <- "Larger engine sizes tend to have lower fuel economy." |>
  str_wrap(width = 31)
trend_text

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  annotate(
    geom = "label", x = 3.5, y = 35,
    label = trend_text,
    hjust = "left", color = "red"
  ) +
  annotate(
    geom = "segment",
    x = 3, y = 35, xend = 5, yend = 25, color = "red",
    arrow = arrow(type = "closed")
  )
# 11.3.1 Exercises
text <- "Just to check"

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  geom_text(
    aes(
      x = Inf, y = Inf,
      label = text
    ),
    hjust = 1, vjust = 1
  )
?geom_text

g <- mpg |>
  ggplot(aes(
    x = displ,
    y = hwy
  )) +
  geom_point(position = position_jitter(seed = 1)) +
  theme_light()

g +
  # Add text labels at the four corners using infinite positions
  geom_text(
    aes(
      x = Inf, y = Inf,
      label = "High mileage, high displacement"
    ),
    hjust = 1, vjust = 1
  ) +
  geom_text(
    aes(
      x = -Inf, y = -Inf,
      label = "Low mileage, low displacement"
    ),
    hjust = 0, vjust = -0.2
  ) +
  geom_text(
    aes(
      x = Inf, y = -Inf,
      label = "Low mileage, high displacement"
    ),
    hjust = 1, vjust = -0.2
  ) +
  geom_text(
    aes(
      x = -Inf, y = Inf,
      label = "High mileage, Low displacement"
    ),
    hjust = 0, vjust = 1
  )

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  annotate(
    geom = "point", x = 4, y = 30,
    color = "blue", shape = 23, size = 6
  )

install.packages("gt")
library(gt)
data("gtcars")

gtcars |>
  # Remove NAs, else they will be treated as maximum
  drop_na() |>
  # Cosmetic improvements for labelling in plot
  mutate(bdy_style = str_to_title(bdy_style)) |>
  mutate(drivetrain = case_when(
    drivetrain == "awd" ~ "All-Wheel Drive",
    drivetrain == "rwd" ~ "Rear-Wheel Drive"
  )) |>
  # Determine car with maximum Highway mileage in each group
  # group_by(bdy_style) |> may adjust adding a label to a single facet or all
  mutate(
    high_cat = ifelse(mpg_h == max(mpg_h),
      yes = paste0(mfr, " ", model),
      no = NA
    )
  ) |>
  ggplot(aes(
    x = hp,
    y = mpg_h,
    color = drivetrain,
    label = high_cat
  )) +
  geom_point(size = 1.5) +
  geom_text_repel(color = "#454647") +

  # To makr mpg_h comparable visually across facets,
  # we select layout of 4 columns
  facet_wrap(~bdy_style, ncol = 4) +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(
    x = "Horsepower",
    y = "Highway Fuel efficiency (miles per gallon)",
    color = "Car's drivetrain",
    title = "Most fuel efficient car in each category has the least HP",
    caption = "Data grom gt package (gtcars)"
  )

tibble(
  x = 1:10,
  y = 2 * x + rnorm(10)
) |>
  ggplot(aes(x = x, y = y)) +
  geom_label(aes(label = x),
    label.padding = unit(0.5, "lines"), # Adjust padding
    label.size = 2, # Set text size
    label.r = unit(1, "lines")
  ) + # Set corner radius
  theme_void()

nos <- 8 # Number of spokes to create
angle <- seq(0, 7 / 4 * pi, length.out = nos) # Angles for 8 directions
arrow_length <- 0.8 # Length of line segment

g <- tibble(
  x_start = rep(0, nos), # Common starting point
  y_start = rep(0, nos),
  x_end = cos(angle) * arrow_length, # Calculate end points based on angles
  y_end = sin(angle) * arrow_length
) |>
  ggplot(aes(
    x = x_start,
    y = y_start,
    xend = x_end,
    yend = y_end
  )) +
  theme_void()

gridExtra::grid.arrange(
  g + geom_segment(arrow = arrow(type = "open")) +
    labs(subtitle = "arrow(type = open)"),
  g + geom_segment(arrow = arrow(type = "closed")) +
    labs(subtitle = "arrow(type = closed)"),
  g + geom_segment(arrow = arrow(angle = 15)) +
    labs(subtitle = "arrow(angle = 15)"),
  g + geom_segment(arrow = arrow(angle = 90)) +
    labs(subtitle = "arrow(angle = 90)"),
  g + geom_segment(arrow = arrow(ends = "first")) +
    labs(subtitle = "arrow(ends = first)"),
  g + geom_segment(arrow = arrow(ends = "last")) +
    labs(subtitle = "arrow(ends = last)"),
  g + geom_segment(arrow = arrow(ends = "both")) +
    labs(subtitle = "arrow(ends = both)"),
  g + geom_segment(arrow = arrow(length = unit(1, "cm"))) +
    labs(subtitle = "arrow(length = unit(1, cm)"),
  g + geom_segment(arrow = arrow(length = unit(0.5, "cm"))) +
    labs(subtitle = "arrow(length = unit(0.5, cm)"),
  ncol = 3
)

# 11.4 Scales
# 11.4.1 Default scales
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class))
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()
# 11.4.2 Axis ticks and legend keys
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  scale_color_discrete(
    labels = c("4" = "4-wheel", "f" = "front", "r" = "rear")
  )

ggplot(diamonds, aes(x = price, y = cut)) +
  geom_boxplot(alpha = 0.05) +
  scale_x_continuous(labels = label_dollar())

ggplot(diamonds, aes(x = price, y = cut)) +
  geom_boxplot(alpha = 0.05) +
  scale_x_continuous(
    labels = label_dollar(scale = 1 / 1000, suffix = "K"),
    breaks = seq(1000, 19000, by = 6000)
  )

ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill") +
  scale_y_continuous(
    name = "Percentage", labels = label_percent()
  )

presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(
    name = NULL,
    breaks = presidential$start,
    date_labels = "'%y"
  )
# 11.4.3 Legend layout
base <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class))

base + theme(legend.position = "right") # the default
base + theme(legend.position = "left")
base +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 3))
base +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 3))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 4)))
# 11.4.4 Replacing a scale
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d()

ggplot(diamonds, aes(x = log10(carat), y = log10(price))) +
  geom_bin2d()

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d() +
  scale_x_log10() +
  scale_y_log10()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  scale_color_brewer(palette = "Set1")

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_color_brewer(palette = "Set1")

presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(
    values = c(Republican = "#E81B23", Democratic = "#00AEF3")
  )

df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)

ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed() +
  labs(title = "Default, continuous", x = NULL, y = NULL)

ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed() +
  scale_fill_viridis_c() +
  labs(title = "Viridis, continuous", x = NULL, y = NULL)

ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed() +
  scale_fill_viridis_b() +
  labs(title = "Viridis, binned", x = NULL, y = NULL)
# 11.4.5 Zooming
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth()

mpg |>
  filter(displ >= 5 & displ <= 6 & hwy >= 10 & hwy <= 25) |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth() +
  scale_x_continuous(limits = c(5, 6)) +
  scale_y_continuous(limits = c(10, 25))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 6), ylim = c(10, 25))

suv <- mpg |> filter(class == "suv")
compact <- mpg |> filter(class == "compact")

ggplot(suv, aes(x = displ, y = hwy, color = drv)) +
  geom_point()

ggplot(compact, aes(x = displ, y = hwy, color = drv)) +
  geom_point()

x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv))

ggplot(suv, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

ggplot(compact, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale
# 11.4.6 Exercises
df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)

ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed()
?scale_fill_gradient

presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(
    values = c(Republican = "#E81B23", Democratic = "#00AEF3")
  )

data("presidential")

# Creating a vector of years where a new president takes office
# to allow plotting it on the x-axis
x_labs <-
  presidential |>
  select(start) |>
  as_vector() |>
  as_date()

y_labs <- seq(
  from = 1 + 33,
  to = nrow(presidential) + 33,
  by = 1
)

g <-
  presidential |>
  as_tibble() |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(
    x = start, xend = end,
    y = id, yend = id,
    col = party,
    label = name
  )) +
  geom_segment(lwd = 10) +

  # (a) Combining the two variants that customize colors and x axis breaks.
  scale_color_manual(values = c(
    "Democratic" = "blue",
    "Republican" = "red"
  )) +
  scale_x_continuous(
    breaks = x_labs,
    labels = year(x_labs)
  ) +

  # (b) Improving the display of the y axis.
  scale_y_reverse(
    breaks = y_labs,
    labels = y_labs
  ) +

  # (c) Labeling each term with the name of the president.
  geom_text(
    hjust = -0.05,
    vjust = -1.5
  ) +
  geom_rect(
    aes(
      xmin = start,
      xmax = end,
      ymin = 33.5,
      ymax = 45.5,
      fill = party
    ),
    alpha = 0.3,
    col = NA
  ) +
  scale_fill_manual(values = c(
    "Democratic" = "blue",
    "Republican" = "red"
  )) +

  # (d) Adding informative plot labels.
  labs(
    color = "President's Political Party",
    y = "Table of Precedence Number",
    x = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(
    panel.grid.major.x = element_line(color = "darkgrey"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text()
  ) +
  guides(fill = "none")

print(g)

x_4yr_labs <-
  presidential |>
  select(start) |>
  as_vector() |>
  as_date()

x_4yr_labs <- seq(
  from = ymd(x_4yr_labs[1]),
  to = ymd(x_4yr_labs[length(x_4yr_labs)]),
  by = "4 years"
)

g +
  scale_x_continuous(
    breaks = x_4yr_labs,
    labels = year(x_4yr_labs)
  )

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1 / 20)

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1 / 20) +
  theme_light() +
  guides(color = guide_legend(
    override.aes = list(
      size = 5,
      alpha = 0.5
    )
  ))

# 11.5 Themes
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw()

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  labs(
    title = "Larger engine sizes tend to have lower fuel economy",
    caption = "Source: https://fueleconomy.gov."
  ) +
  theme(
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal",
    legend.box.background = element_rect(color = "black"),
    plot.title = element_text(face = "bold"),
    # plot.title.position = "plot",
    # plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )
# 11.5.1 Exercises
install.packages("ggthemes")
library(ggthemes)
??ggthemes

p <- ggplot(mtcars) +
  geom_point(aes(x = wt, y = mpg, colour = factor(gear))) +
  facet_wrap(~am) +
  # Economist puts x-axis labels on the right-hand side
  scale_y_continuous(position = "right")

## Standard
p + theme_economist() +
  scale_colour_economist()

# Change axis lines to vertical
p + theme_economist(horizontal = FALSE) +
  scale_colour_economist() +
  coord_flip()

## White panel/light gray background
p + theme_economist_white() +
  scale_colour_economist() +
  theme(
    axis.text.x = element_text(face = "bold", color = "red"),
    axis.title.y = element_text(face = "bold", color = "pink")
  )

## All white variant
p + theme_economist_white(gray_bg = FALSE) +
  scale_colour_economist()

if (FALSE) {
  ## The Economist uses ITC Officina Sans
  library("extrafont")
  p + theme_economist(base_family = "ITC Officina Sans") +
    scale_colour_economist()

  ## Verdana is a widely available substitute
  p + theme_economist(base_family = "Verdana") +
    scale_colour_economist()
}

# 11.6 Layout
p1 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  labs(title = "Plot 1")
p2 <- ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_boxplot() +
  labs(title = "Plot 2")
p1 + p2

p3 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  labs(title = "Plot 3")
(p1 | p3) / p2

p1 <- ggplot(mpg, aes(x = drv, y = cty, color = drv)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Plot 1")

p2 <- ggplot(mpg, aes(x = drv, y = hwy, color = drv)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Plot 2")

p3 <- ggplot(mpg, aes(x = cty, color = drv, fill = drv)) +
  geom_density(alpha = 0.5) +
  labs(title = "Plot 3")

p4 <- ggplot(mpg, aes(x = hwy, color = drv, fill = drv)) +
  geom_density(alpha = 0.5) +
  labs(title = "Plot 4")

p5 <- ggplot(mpg, aes(x = cty, y = hwy, color = drv)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~drv) +
  labs(title = "Plot 5")

(guide_area() / (p1 + p2) / (p3 + p4) / p5) +
  plot_annotation(
    title = "City and highway mileage for cars with different drive trains",
    caption = "Source: https://fueleconomy.gov."
  ) +
  plot_layout(
    guides = "collect",
    heights = c(4, 2, 2, 2)
  ) &
  theme(legend.position = "top")
?plot_layout
# 11.6.1 Exercises
p1 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  labs(title = "Plot 1")
p2 <- ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_boxplot() +
  labs(title = "Plot 2")
p3 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  labs(title = "Plot 3")

(guide_area() / p1 / (p2 + p3)) +
  plot_annotation(
    tag_prefix = "Fig. ",
    tag_levels = "A",
    tag_suffix = ":"
  ) &
  theme(
    axis.text.x = element_text(face = "bold", color = "red"),
    axis.title.y = element_text(face = "bold", color = "pink")
  )
