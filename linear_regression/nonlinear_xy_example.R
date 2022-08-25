# explore nonlinear relationships in data

library(ggplot2)

xyz.df <- read.csv("./data/nonlinear_xyz_example.csv")
head(xyz.df)


# plot y vs x
ggplot(xyz.df) + 
  geom_point(mapping = aes(x=x, y=y))


# simple linear model
lm.1 <- lm(y ~ x, data=xyz.df)
summary(lm.1)

ggplot(xyz.df, mapping = aes(x=x, y=y)) + 
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ x, color="blue") + 
  ggtitle("Simple Linear Model")


# 2nd order polynomial model
lm.poly.2 <- lm(y ~ x + I(x^2), data=xyz.df)
summary(lm.poly.2)

ggplot(xyz.df, mapping = aes(x=x, y=y)) + 
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ x + I(x^2) ,color="blue") + 
  ggtitle("2nd-order polynomial")


# alternative approach: construct a new feature
xyz.df$x.is.positive <- xyz.df$x > 0

# use the new feature as an interaction term
lm.interaction <- lm(y ~ x + I(x*x.is.positive), data=xyz.df)
summary(lm.interaction)

# construct a new dataframe to plot our predictions
fake.x <- seq(-5,15,0.01)
fake.x.is.positive <- fake.x > 0
lm.interaction.preds <- lm.interaction$coefficients[1] + 
  lm.interaction$coefficients[2] * fake.x + 
  lm.interaction$coefficients[3] * (fake.x * fake.x.is.positive)

# 
ggplot() + 
  geom_point(xyz.df, mapping = aes(x=x, y=y)) + 
  geom_line(mapping = aes(x=x.ticks, y=lm.interaction.preds), col="blue")
  ggtitle("Interaction Term Model")


# plot y vs x
ggplot(xyz.df) + 
  geom_point(mapping = aes(x=x, y=z))

z.lm.1 <- lm(z ~ x, data=xyz.df)
summary(z.lm.1)

ggplot(xyz.df, mapping = aes(x=x, y=y)) + 
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ x, color="blue") + 
  ggtitle("Simple Linear Model")
