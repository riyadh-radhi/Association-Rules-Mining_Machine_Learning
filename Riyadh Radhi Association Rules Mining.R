rm(list = ls())


library(arules)
library(arulesViz)
library(MASS)
library(dplyr)


#######   1   #######


df <- read.csv("Online Retail.csv")
splitted_df <- split(df[,"Description"],df[,"InvoiceNo"])
transaction <- as(splitted_df, "transactions")
summary(transaction)


######    2   #######

rules <- apriori(transaction, parameter = list(minlen =2, maxlen = 5, supp=0.001 , conf=0.7 ))
rules <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")

rules

#####    3    #######

inspect(rules[1:30])

#Invistigate the first rule
inspect(rules[1])

# To get the support value manually,we will divide the (count/number of invoices)
#(19/18536 =0.0010250323) which is the same as the one in the rule
#And to get the lift value, we divide that support value by the propability is A and B(lift = support/P(A)*P(B)) 

#Invistigate the rule number 20

inspect(rules[20])

# To get the support value manually,we will divide the (count/number of invoices)
#(22/18536 =0.00118687) which is the same as the one in the rule
#And to get the lift value, we divide that support value by the propability is A and B(lift = support/P(A)*P(B)) 

######  4    ######## 

d <- subset(rules, subset = lhs %ain% c("REGENCY TEA PLATE GREEN","REGENCY CAKE FORK") & rhs %in% "REGENCY TEA PLATE ROSES")
inspect(d[1])

#As we can see lift increased to 56.5122


##### 5 ######## Not complete

plot(rules[1:20],method= "graph")

#So what we see in the graph are our first 20 rules. They have different sizes based on the support value
#starting from the smallest with 0.001 support ending with the largest size with 0.002. Our color intensity 
#tells us about the lift value, the more intense the color, the higher our lift value is. 

#Also the graph actually shows the rules based on our description, so we can conclude the if/statement of 
#our rules by looking the arrows of each rule and wheather the arrow is outward or inward and so on. 

plot(rules[1:5],method= "graph")

### 6 #####


s <- subset(rules, subset = rhs %in% "REGENCY CAKE FORK")
inspect(unique(s@lhs))

### 7 ####

f <- subset(rules, subset = lhs %in% "REGENCY CAKE FORK")
inspect(c(unique(f@lhs),unique(f@rhs)))
