library(tidyverse)
library(readxl)
library(arules)

# from http://archive.ics.uci.edu/ml/datasets/Online+Retail
online.retail <- read_excel("./data/Online Retail.xlsx")
online.retail

online.retail %>% 
  summarize(
    unique.InvoiceNo = n_distinct(InvoiceNo),
    unique.StockCode = n_distinct(StockCode),
    unique.Description = n_distinct(Description)
  )
# interesting - more unique descriptions than stock codes

sum(is.na(online.retail$InvoiceNo))
sum(is.na(online.retail$StockCode))
sum(is.na(online.retail$Description))

# create a table to associate StockCode and Description
StockCode.Description.Frequency <-
  online.retail %>%
  group_by(StockCode, Description) %>%
  summarise(
    row.count = n()
  ) %>%
  arrange(desc(row.count))

StockCode.Description.Frequency
dim(StockCode.Description.Frequency)
length(unique(StockCode.Description.Frequency$StockCode))

# drop NA descriptions
StockCode.Description.Frequency <- 
  StockCode.Description.Frequency %>%
  filter(!is.na(Description))

dim(StockCode.Description.Frequency)
length(unique(StockCode.Description.Frequency$StockCode))
# looks like they are all unique now

# alternatively:
StockCode.Description.Frequency %>%
  ungroup %>%
  summarise(
    count = n(),
    count.distinct = n_distinct(StockCode)
  )

### set up our data as a transactions database ###

# only use InvoiceNo and StockCode columns
online.retail.staging <- 
  online.retail %>%
  select(InvoiceNo, StockCode) %>%
  mutate(Incidence = 1)
online.retail.staging

# make both of these columns factors
# online.retail.staging <- 
#   online.retail.staging %>%
#   mutate(
#     InvoiceNo = as.factor(InvoiceNo),
#     StockCode = as.factor(StockCode)
#   )
# online.retail.staging

# convert to a "wide-format" dataframe
# with InvoiceNo for rows, StockCode for columns, and a 1 or 0 for incidence
online.retail.wide <- 
  online.retail.staging %>%
  spread(StockCode, Incidence, fill=0)

# try again after de-duping
online.retail.staging <- 
  online.retail.staging %>%
  unique()

online.retail.wide <- 
  online.retail.staging %>%
  spread(StockCode, Incidence, fill=0)

online.retail.wide

# make 'InvoiceNo' the row names rather than an actual column
online.retail.wide <- 
  online.retail.wide %>% 
  remove_rownames %>%
  column_to_rownames(var = "InvoiceNo")

dim(online.retail.wide)
online.retail.wide[1:5,1:5]

# convert our wide training dataframe to a matrix
online.retail.matrix <- as.matrix(online.retail.wide)
  
transaction.data <- as(online.retail.matrix, "transactions")
transaction.data
# does this have the right number of transactions and items?

head(itemFrequency(transaction.data))

# run apriori algorithm to find frequent itemsets
rules <- apriori(transaction.data,
                 parameter = list(supp=0.01, conf = 0.5, maxlen = 3, target = "rules"))
rules

# see what rules were generated
top.10.rules <- sort(rules, by = "lift")[1:10]
inspect(top.10.rules)

# what do these actually mean?
StockCode.Description.Frequency %>%
  filter(StockCode %in% c(23171,23172))

StockCode.Description.Frequency %>%
  filter(StockCode %in% c(22746,22745))


# try converting the rules to a dataframe and joining with descriptions
rules.df <- as(inspect(rules), "data.frame")
# drop the 2nd column "=>"
rules.df <- rules.df[,-2]
head(rules.df)

# now we can make it a tibble
rules.df <- as_tibble(rules.df)
rules.df

# clean up the formatting so we can match lhs and rhs with the description lookup
# rules.df$lhs <- str_remove_all(rules.df$lhs, "[{}]")
# rules.df$rhs <- str_remove_all(rules.df$rhs, "[{}]")
rules.df <-
  rules.df %>%
  mutate(
    lhs = str_remove_all(lhs, "[{}]"),
    rhs = str_remove_all(rhs, "[{}]")
  )
rules.df

# create a StockCode Description lookup table out of the StockCode.Description.Frequency table
StockCode.Description.Lookup <- 
  StockCode.Description.Frequency %>%
  select(StockCode,Description)

# add the LHS description and rename it
rules.df <- 
  rules.df %>% 
  left_join(StockCode.Description.Lookup, by=c("lhs"="StockCode")) %>%
  rename( LHS.Description = Description )

# add the RHS description and rename it
rules.df <- 
  rules.df %>% 
  left_join(StockCode.Description.Lookup, by=c("rhs"="StockCode")) %>%
  rename( RHS.Description = Description )

rules.df

# look at the top rules again
rules.df %>% 
  arrange(desc(lift))

# how about top support rules?
rules.df %>% 
  arrange(desc(support))

# top confidence
rules.df %>% 
  arrange(desc(confidence))

# note: the description lookup we used only works when 
# lhs and rhs are 1 item each

