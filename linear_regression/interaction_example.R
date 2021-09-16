library(ggplot2)
library(tidyverse)

# load our example data
example.df <- read.csv("./data/interaction_example_df.csv")

head(example.df)
dim(example.df)

# plot y vs x
ggplot(example.df) +
  geom_point(mapping = aes(x=x, y=y))
# are these correlated?
# is there any relationship?

# plot y vs z
ggplot(example.df) +
  geom_point(mapping = aes(x=z, y=y))
# are these correlated?
# is there any relationship?

# plot x, y and z together
ggplot(example.df) +
  geom_point(mapping = aes(x=x, y=z, col=y), size=3) + 
  scale_color_gradient(low="blue", high="green")
# now what's going on?
# why didn't this show up in the first two plots?

# linear regression model for y with x as a predictor
lm(y ~ x, data=example.df) %>% 
  summary()
# equivalent to:
summary(lm(y ~ x, data=example.df))

# with z as a predictor
lm(y ~ z, data=example.df) %>% 
  summary()

# with x, z and interaction terms
lm(y ~ x*z, data=example.df) %>%
  summary()

# x*z interaction term only
lm(y ~ x*z - x - z, data=example.df) %>%
  summary()

