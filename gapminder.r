library(tidyverse)

gdp_messy <- readr::read_csv("~/src/viz/data/gdppercapita_us_inflation_adjusted.csv")
gdp <- gapminder_messy %>% 
  gather(key = year, value = gdpPerCapita, -country) %>%
  filter(year >= 2000, year <= 2019)

life_messy <- readr::read_csv("~/src/viz/data/life_expectancy_years.csv")
life <- life_messy %>% 
  gather(key = year, value = lifeExpectancy, -country) %>% 
  filter(year >= 2000, year <= 2019)

continents <- readr::read_csv("~/src/viz/data/Countries-Continents.csv")

full <- gdp %>% inner_join(life) %>% inner_join(continents)

ggplot(data = full,
       mapping = aes(x = gdpPerCapita,
                     y = lifeExpectancy)) +
  geom_point(alpha = 0.3, aes(color = continent)) +
  geom_smooth(method = "gam") + 
  scale_x_log10(labels = scales::dollar) +
  scale_y_continuous(limits = c(40, 90)) +
  labs(x = "GDP per capita (USD)", y = "Life expectancy (years)",
       title = "GDP per capita vs life expectancy from 2000-2019")
