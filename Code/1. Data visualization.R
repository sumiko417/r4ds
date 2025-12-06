# 1.1 Introduction
# 1.1.1 Prerequisites
install.packages("tidyverse")
library(tidyverse)
install.packages("palmerpenguins")
library(palmerpenguins)
install.packages("ggthemes")
library(ggthemes)

# 1.2 First steps
# 1.2.1 The penguins data frame
penguins
library(dplyr)
glimpse(penguins)
View(penguins)
?penguins
# 1.2.2 Ultimate goal
# 1.2.3 Creating a ggplot
ggplot(data = penguins)
# 1.2.4 Adding aesthetics and layers
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()
# 1.2.5 Exercises
ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point()

ggplot(
  data = penguins,
  mapping = aes(x = bill_depth_mm, y = bill_length_mm)
) +
  geom_point(na.rm = TRUE) +
  labs(
    caption = "Data come from the palmerpenguins package."
  )

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = bill_depth_mm)) +
  geom_smooth()

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

# 1.3 ggplot2 calls
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
penguins |>
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# 1.4 Visualizing distributions
# 1.4.1 A categorical variable
ggplot(penguins, aes(x = species)) +
  geom_bar()
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()
# 1.4.2 A numerical variable
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()
# 1.4.3 Exercises
penguins |> ggplot(aes(y = fct_infreq(species))) +
  geom_bar()

ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")

diamonds |> ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

# 1.5 Visualizing relationships
# 1.5.1 A numerical and a categorical variable
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 1)
ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)
# 1.5.2 Two categorical variables
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
# 1.5.3 Two numerical variables
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
# 1.5.4 Three or more variables
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)
# 1.5.5 Exercises
view(mpg)
mpg
glimpse(mpg)

mpg |> ggplot(aes(x = hwy, y = displ)) +
  geom_point(aes(color = year))

mpg |> ggplot(aes(x = hwy, y = displ, color = year)) +
  geom_point()

mpg |> ggplot(aes(x = hwy, y = displ)) +
  geom_point(aes(size = year))

mpg |> ggplot(aes(x = hwy, y = displ)) +
  geom_point(aes(shape = year))

mpg |> ggplot(aes(x = hwy, y = displ)) +
  geom_point(aes(color = year, size = year))

mpg |> ggplot(aes(x = hwy, y = displ)) +
  geom_point(aes(color = year, size = year, shape = trans))

mpg |> ggplot(aes(x = hwy, y = displ)) +
  geom_point(aes(linewidth = year))

mpg |> ggplot(aes(x = hwy, y = displ, linewidth = year)) +
  geom_point()

penguins |> ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(aes(color = species))

penguins |> ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point() +
  facet_wrap(~species)

ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm,
    color = species, shape = species
  )
) +
  geom_point() +
  labs(
    color = "Species",
    shape = "Species"
  )

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")

# 1.6 Saving your plots
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggsave(filename = "penguin-plot.png")
# 1.6.1 Exercises
ggplot(mpg, aes(x = class)) +
  geom_bar()

ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()

ggsave("mpg-plot.pdf")
?ggsave

# 1.7 Common problems

# 1.8 Summary