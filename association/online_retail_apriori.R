library(tidyverse)
library(readxl) # for read_excel() function
library(arules) # for apriori() function

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

# check number of rows and number of unique Stock Codes now:
StockCode.Description.Frequency
dim(StockCode.Description.Frequency)
length(unique(StockCode.Description.Frequency$StockCode))

# drop NA descriptions
StockCode.Description.Frequency <- 
  StockCode.Description.Frequency %>%
  filter(!is.na(Description))

# keep only the most frequent description
StockCode.Description.Frequency <- StockCode.Description.Frequency[
  !duplicated(StockCode.Description.Frequency$StockCode),]

# check number of rows and number of unique Stock Codes again
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
# (forget about 'Description' for now)
online.retail.staging <- 
  online.retail %>%
  select(InvoiceNo, StockCode) %>%
  mutate(Incidence = 1)
online.retail.staging

# convert to a "wide-format" dataframe
# with InvoiceNo for rows, StockCode for columns, and a 1 or 0 for incidence
online.retail.wide <- 
  online.retail.staging %>%
  spread(StockCode, Incidence, fill=0)

# try again after de-duping
online.retail.deduped <- 
  online.retail.staging %>%
  unique()

online.retail.wide <- 
  online.retail.deduped %>%
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
# support >= 1%
# confidence >= 50%
# itemsets of size <= 3
rules <- apriori(transaction.data,
                 parameter = list(supp=0.01, conf = 0.5, maxlen = 3, target = "rules"))
rules

# see what rules were generated
top.10.rules <- sort(rules, by = "lift")[1:10]
inspect(top.10.rules)

# what do these actually mean?
# items from rule 1:
StockCode.Description.Frequency %>%
  filter(StockCode %in% c(23172,23171))

# items from rule 5:
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
# make both changes at once with mutate()
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
# using a "left join" - only include an item from the joined table if it matches an entry in the original table
# left_join() is from the dplyr package included in tidyverse
names(rules.df)
names(StockCode.Description.Lookup)
rules.df <- 
  rules.df %>% 
  left_join(StockCode.Description.Lookup, by=c("lhs"="StockCode")) %>%
  rename( LHS.Description = Description )

# now add the RHS description and rename it
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
# LHS is a single item
# 3-item itemset rules just have Stock Codes for LHS:
rules.df.no.lhs.desc <- 
  rules.df %>%
  filter(
    is.na(LHS.Description)
  )
View(rules.df.no.lhs.desc)

