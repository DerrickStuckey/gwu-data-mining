# from https://www.kaggle.com/blastchar/telco-customer-telco.customers
telco.customers <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")
telco.customers

View(telco.customers)

table(telco.customers$Churn)
