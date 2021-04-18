library(socviz)
library(tidyverse)

p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion, fill = religion))
p + geom_bar(mapping = aes(y = ..prop.., group = religion))
