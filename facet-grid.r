library(socviz)
library(tidyverse)

p <- ggplot(data = gss_sm,
            mapping = aes(x = age, y = childs))
p + geom_point(alpha = 0.2) +
  facet_grid(sex ~ race) +
  geom_smooth(method = "loess") + 
  labs(x = "Age (yrs)", y = "# of children", 
       title = "Age vs. number of children among different genders and races")
