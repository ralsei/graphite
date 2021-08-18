library(gapminder)
library(tidyverse)

ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp)) +
  geom_point(alpha = 0.4, aes(color = continent)) +
  geom_smooth() +
  scale_x_log10() +
  labs(x = "GDP per capita (USD)",
       y = "Life expectancy (years)")
