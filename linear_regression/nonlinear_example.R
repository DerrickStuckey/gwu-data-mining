# explore nonlinear relationships in data

library(ggplot2)

# load our example data
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


# plot z vs x
ggplot(xyz.df) + 
  geom_point(mapping = aes(x=x, y=z))

z.lm.1 <- lm(z ~ x, data=xyz.df)
summary(z.lm.1)

ggplot(xyz.df, mapping = aes(x=x, y=z)) + 
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=z ~ x, color="blue") + 
  ggtitle("Simple Linear Model")


# 2nd order polynomial model for z vs x
z.lm.poly.2 <- lm(z ~ x + I(x^2), data=xyz.df)
summary(z.lm.poly.2)

ggplot(xyz.df, mapping = aes(x=x, y=z)) + 
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=z ~ x + I(x^2) ,color="blue") + 
  ggtitle("2nd-order polynomial")


# 3nd order polynomial model for z vs x
z.lm.poly.3 <- lm(z ~ x + I(x^2) + I(x^3), data=xyz.df)
summary(z.lm.poly.3)

ggplot(xyz.df, mapping = aes(x=x, y=z)) + 
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=z ~ x + I(x^2) + I(x^3) ,color="blue") + 
  ggtitle("3rd-order polynomial")


# 10th order polynomial model for z vs x
ggplot(xyz.df, mapping = aes(x=x, y=z)) + 
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=z ~ poly(x,10),color="blue") + 
  ggtitle("10th-order polynomial")
# is this a better model?

