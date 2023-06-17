library(tidyverse)

# GOAL: Center and standardize every numeric column but `year` (by species and island)

palmerpenguins::penguins

# Helper function
center_and_standardize <- function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)

# THIS IS TEDIOUS
palmerpenguins::penguins |> 
  group_by(species, island) |> 
  mutate(
    bill_length_mm = center_and_standardize(bill_length_mm),
    bill_depth_mm = center_and_standardize(bill_depth_mm),
    flipper_length_mm = center_and_standardize(flipper_length_mm),
    body_mass_g = center_and_standardize(body_mass_g)
  )

# Same result, less typing
palmerpenguins::penguins |> 
  group_by(species, island) |> 
  mutate(
    across(
      .cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
      .fns = center_and_standardize
    )
  )


# Same result, even less typing (Alternative I)
palmerpenguins::penguins |> 
  group_by(species, island) |> 
  mutate(
    across(
      .cols = ends_with(c('_mm', '_g')),
      .fns = center_and_standardize
    )
  )

# Same result, even less typing (Alternative II)
palmerpenguins::penguins |> 
  group_by(species, island) |> 
  mutate(
    across(
      .cols = matches(c('bill', 'flipper', 'body')),
      .fns = center_and_standardize
    )
  )

# Same result, even less typing (Alternative III)
palmerpenguins::penguins |> 
  group_by(species, island) |> 
  mutate(
    across(
      .cols = c(where(is.numeric), -year),
      .fns = center_and_standardize
    )
  )



mpg |> 
  mutate(
    manufacturer = factor(manufacturer),
    model = factor(model),
    trans = factor(trans),
    drv = factor(drv),
    fl = factor(drv),
    class = factor(drv)
  )

mpg |> 
  mutate(
    across(
      .cols = c(manufacturer, model, trans, drv, fl, class),
      .fns = factor
    )
  )

# Same result, even less typing
mpg |> 
  mutate(
    across(
      .cols = where(is.character),
      .fns = factor
    )
  )
