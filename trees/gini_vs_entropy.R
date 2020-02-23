
# Gini Impurity function
gini.impurity <- function(proportions) {
  i <- 1 - sum(proportions^2)
  return(i)
}

# class proportion examples
prop.a <- 0.4
prop.b <- 0.6

proportions.1 <- c(prop.a, prop.b)
gini.impurity(proportions.1)

proportions.1.reverse <- c(prop.b, prop.a)
gini.impurity(proportions.1.reverse)

gini.impurity(c(0.2,0.8))
gini.impurity(c(0.1,0.9))

# compute Gini Impurity for proportions 0 to 1
prop.a.vals <- seq(0,1,by=0.01)
gini.impurity.vals <- c()

for (prop.a in prop.a.vals) {
  prop.b <- 1 - prop.a
  props <- c(prop.a, prop.b)
  gini.impurity.val <- gini.impurity(props)
  gini.impurity.vals <- c(gini.impurity.vals, gini.impurity.val)
}

# plot Gini Impurity vs Class A Proportion
ggplot() + 
  geom_line(mapping = aes(x=prop.a.vals, y=gini.impurity.vals)) +
  xlab("Class A Proportion") + 
  ylab("Gini Impurity")


# Entropy Calculation
entropy <- function(proportions) {
  e <- -1 * sum(proportions * log(proportions, base=2))
  return(e)
}

# class proportion examples
prop.a <- 0.4
prop.b <- 0.6

proportions.1 <- c(prop.a, prop.b)
entropy(proportions.1)

proportions.1.reverse <- c(prop.b, prop.a)
entropy(proportions.1.reverse)

entropy(c(0.2,0.8))
entropy(c(0.1,0.9))

# compute Entropy for proportions 0 to 1
prop.a.vals <- seq(0,1,by=0.01)
entropy.vals <- c()

for (prop.a in prop.a.vals) {
  prop.b <- 1 - prop.a
  props <- c(prop.a, prop.b)
  entropy.val <- entropy(props)
  entropy.vals <- c(entropy.vals, entropy.val)
}

# plot Entropy vs Class A Proportion
ggplot() + 
  geom_line(mapping = aes(x=prop.a.vals, y=entropy.vals)) +
  xlab("Class A Proportion") + 
  ylab("Entropy")

# compare the two
gini.df <- data.frame("Proportion.A"=prop.a.vals,
                      "Value"=gini.impurity.vals,
                      "Metric"="Gini Impurity")
entropy.df <- data.frame("Proportion.A"=prop.a.vals,
                      "Value"=entropy.vals,
                      "Metric"="Entropy")
comp.df <- rbind(gini.df, entropy.df)

ggplot(data=comp.df) + 
  geom_line(mapping = aes(x=Proportion.A, y=Value, col=Metric)) +
  xlab("Class A Proportion")

# plot Entropy at half scale
entropy.df.2 <- data.frame("Proportion.A"=prop.a.vals,
                         "Value"=entropy.vals/2,
                         "Metric"="Entropy / 2")
comp.df.2 <- rbind(gini.df, entropy.df.2)

ggplot(data=comp.df.2) + 
  geom_line(mapping = aes(x=Proportion.A, y=Value, col=Metric)) +
  xlab("Class A Proportion")

# 3-class example:
prop.a <- 0.4
prop.b <- 0.3
prop.c <- 0.3

proportions.2 <- c(prop.a, prop.b, prop.c)
gini.impurity(proportions.2)
entropy(proportions.2)

