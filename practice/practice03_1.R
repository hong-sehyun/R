library(tidyverse)
library(palmerpenguins)
install.packages("palmerpenguins")
glimpse(penguins)
t(map_df(penguins, ~sum(is.na(.))))

plot_data <- penguins %>%
  drop_na()

t(map_df(plot_data, ~sum(is.na(.))))

plot_data$spcies

