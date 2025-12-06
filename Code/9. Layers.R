# 9.1 Introduction
# 9.1.1 Prerequisites
library(tidyverse)

# 9.2 Aesthetic mappings
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy, shape = class)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy, size = class)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy, alpha = class)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue")
# 9.2.1 Exercises
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(shape = 17, color = "pink")
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, color = "blue"))
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue")
?geom_point
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(shape = 22, stroke = 0.1)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = displ < 5))
ggplot(mpg, aes(x = displ, y = hwy, color = displ < 5)) +
  geom_point()

# 9.3 Geometric objects
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth()
ggplot(mpg, aes(x = displ, y = hwy, shape = drv)) +
  geom_smooth()
ggplot(mpg, aes(x = displ, y = hwy, linetype = drv)) +
  geom_smooth()
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(aes(linetype = drv))
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(group = drv))
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv))
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_point(
    data = mpg |> filter(class == "2seater"),
    color = "red"
  ) +
  geom_point(
    data = mpg |> filter(class == "2seater"),
    shape = "circle open", size = 3, color = "red"
  )
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 2)
ggplot(mpg, aes(x = hwy)) +
  geom_density()
ggplot(mpg, aes(x = hwy)) +
  geom_boxplot()
library(ggridges)
ggplot(mpg, aes(x = hwy, y = drv, fill = drv, color = drv)) +
  geom_density_ridges(alpha = 0.5, show.legend = FALSE)
# 9.3.1 Exercises
ggplot(mpg, aes(displ, hwy)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE)
ggplot(mpg, aes(displ, hwy)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = drv), se = FALSE)
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv), size = 3) +
  geom_smooth(aes(group = drv, color = drv), se = FALSE)
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv), size = 3) +
  geom_smooth(linewidth = 1.5, se = FALSE)
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv), size = 3) +
  geom_smooth(aes(linetype = drv), se = FALSE, linewidth = 1.5)
# ggplot2 has order when mapping
ggplot(mpg, aes(displ, hwy)) +
  geom_point(color = "white", size = 4) +
  geom_point(aes(color = drv), size = 3)
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv), size = 3) +
  geom_point(color = "white", size = 4)

# 9.4 Facets
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~cyl)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl, scales = "free")
# 9.4.1 Exercises
ggplot(mpg, aes(x = drv, y = cyl)) +
  geom_point() +
  facet_wrap(~hwy)
ggplot(mpg) +
  geom_point(aes(x = drv, y = cyl))
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~drv, dir = "h")
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~cyl)
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~cyl, nrow = 2)
?facet_wrap
ggplot(mpg, aes(x = displ)) +
  geom_histogram() +
  facet_grid(drv ~ .)
ggplot(mpg, aes(x = displ)) +
  geom_histogram() +
  facet_grid(. ~ drv)
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~drv, ncol = 1)
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~drv, nrow = 3, strip.position = "right")

# 9.5 Statistical transformations
ggplot(diamonds, aes(x = cut)) +
  geom_bar()
diamonds |>
  count(cut) |>
  ggplot(aes(x = cut, y = n)) +
  geom_bar(stat = "identity")
ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 99)) +
  geom_bar()
ggplot(diamonds, aes(x = cut, y = after_stat(prop))) +
  geom_bar()
?geom_bar
?ggplot
ggplot(diamonds) +
  stat_summary(
    aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )
ggplot(diamonds) +
  stat_summary(
    aes(x = cut, y = depth)
  )
?stat_bin
# 9.5.1 Exercises
diamonds |>
  group_by(cut) |>
  mutate(
    min = min(depth),
    max = max(depth),
    median = median(depth)
  ) |>
  ggplot(aes(x = cut, y = median)) +
  geom_pointrange(aes(ymin = min, ymax = max))
diamonds |>
  group_by(cut) |>
  summarize(
    lower = min(depth),
    upper = max(depth),
    midpoint = median(depth)
  ) |>
  ggplot(aes(x = cut, y = midpoint)) +
  geom_pointrange(aes(ymin = lower, ymax = upper))
?geom_pointrange
ggplot(diamonds, aes(x = cut, y = after_stat(prop))) +
  geom_bar()
ggplot(diamonds, aes(x = cut, fill = color, y = after_stat(prop))) +
  geom_bar()
library(tidyverse)
diamonds |>
  group_by(cut, color) |>
  summarise(n = n()) |>
  mutate(p = n / sum(n)) |>
  ggplot(
    aes(
      x = cut,
      fill = color,
      y = p
    )
  ) +
  geom_bar(stat = "identity")
diamonds |>
  group_by(cut, color) |>
  count() |>
  ggplot(aes(
    x = cut,
    y = n,
    fill = color
  )) +
  geom_bar(stat = "identity")
ggplot(
  diamonds,
  aes(
    x = cut,
    fill = color,
    y = after_stat(prop),
    group = color
  )
) +
  geom_bar()

# 9.6 Position adjustments
ggplot(mpg, aes(x = drv, color = drv)) +
  geom_bar()
ggplot(mpg, aes(x = drv, fill = drv)) +
  geom_bar()
ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar()
ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(alpha = 1 / 5, position = "identity")
ggplot(mpg, aes(x = drv, color = class)) +
  geom_bar(fill = NA, position = "identity")
ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(position = "fill")
ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(position = "dodge")
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "jitter")
# 9.6.1 Exercises
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(position = "jitter")
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "identity")
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "gray") +
  geom_jitter(height = 1, width = 1)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "gray") +
  geom_jitter(height = 1, width = 5)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "gray") +
  geom_jitter(height = 5, width = 1)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_jitter()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_count()
ggplot(mpg, aes(x = cty, y = displ)) +
  geom_boxplot()
ggplot(mpg, aes(x = cty, y = displ)) +
  geom_boxplot(position = "dodge2")

# 9.7 Coordinate systems
nz <- map_data("nz")
ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black")
ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()
bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = clarity, fill = clarity),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1)
bar + coord_flip()
bar + coord_polar()
# 9.7.1 Exercises
ggplot(diamonds, aes(x = "", fill = cut)) +
  geom_bar()
ggplot(diamonds, aes(x = "", fill = cut)) +
  geom_bar() +
  coord_polar(theta = "y")
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  geom_abline() +
  coord_fixed()

# 9.8 The layered grammar of graphics
# ggplot(data = <DATA>) +
#   <GEOM_FUNCTION>(
#     mapping = aes(<MAPPINGS>),
#     stat = <STAT>,
#     position = <POSITION>
#   ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>

# 9.9 Summary