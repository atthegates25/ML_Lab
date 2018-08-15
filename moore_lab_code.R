library(stringr)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
#setwd("/Users/pcpu/Desktop/NYCData/Bootcamp/Projects/Shiny/Example Datasets/Machine_Learning_Lab/ML_Lab-master")

#orders <- read.csv("./data/orders.csv")
#returns <- read.csv("./data/returns.csv")  

orders <- fread("./data/orders.csv")
returns <- fread("./data/returns.csv")

####part 1: basic eda####
colnames(orders) <- tolower(colnames(orders))
colnames(returns) <- tolower(colnames(returns))
colnames(returns)[2] <- "order.id"

str(orders)
str(returns)

summary(orders)

##problem 1##
orders$profit <- as.numeric(gsub("[$, ]", "", orders$profit))
orders$sales <- as.numeric(gsub("[$, ]", "", orders$sales))
str(orders$profit)
str(orders$sales)

##problem 2##
orders$date <- as.Date(orders$order.date, "%m/%d/%y")
str(orders$order.date)
str(orders$date)

#extract year, month, and day
orders$year <- year(orders$date)
orders$month <- month(orders$date)

#check all months present
sort(unique(orders$year))
sort(unique(orders$month))

str(orders$date)

unique(orders$category)

#q1,
#sales at aggregate level
orders %>% 
  group_by(year, month) %>% 
  summarise(tot_orders = sum(quantity)) %>% 
  arrange(year, month) %>% 
  mutate(obs = row_number(),
         yearmo = paste(year, month, sep = "/")) %>% 
  ggplot(aes(obs, tot_orders)) +
  geom_line() +
  theme_bw() +
  labs(title = "Evolution of orders across time",
       x = "Time",
       y = "Total orders") +
  theme(plot.title = element_text(hjust = 0.5))

#orders across time
orders %>% 
  group_by(category, month) %>% 
  summarise(tot_orders = sum(quantity)) %>% 
  ggplot(aes(x = month, y = tot_orders)) +
  geom_bar(aes(fill = category), stat = "identity") +
  theme_bw() +
  labs(title = "Evolution of orders across time",
       x = "Time",
       y = "Total orders",
       fill = "Category") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

#q2,
#composition of orders across time
orders %>% 
  group_by(category, month) %>% 
  summarise(tot_orders = sum(quantity)) %>% 
  ggplot(aes(x = month, y = tot_orders)) +
  geom_bar(aes(fill = category), stat = "identity", position = "fill") +
  theme_bw() +
  labs(title = "Evolution of orders across time",
       x = "Time",
       y = "Total orders",
       fill = "Category") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

#sales trend by category
orders %>% 
  group_by(category, year, month) %>% 
  summarise(tot_orders = sum(quantity)) %>% 
  arrange(year, month) %>% 
  mutate(obs = row_number(),
         yearmo = paste(year, month, sep = "/")) %>%  
  ggplot(aes(obs, tot_orders, fill = category)) +
  geom_line() +
  theme_bw() +
  labs(title = "Evolution of orders across time",
       x = "Time",
       y = "Total orders") +
  theme(plot.title = element_text(hjust = 0.5))

##problem 3##
orders_wret <- orders %>% 
  left_join(returns, by = "order.id") %>% 
  mutate(returned = ifelse(is.na(returned) == TRUE, 0, 1))

#q1,
orders_wret %>% 
  filter(returned == 1) %>% 
  group_by(year) %>% 
  summarise(profit_loss = sum(profit)) %>% 
  ggplot(aes(year, profit_loss)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(title = "Profit loss across time",
       x = "Time",
       y = "Profit loss") +
  theme(plot.title = element_text(hjust = 0.5))

#q2,
#more than one return
nrow(orders_wret %>% 
      filter(returned == 1) %>% 
      group_by(customer.id) %>% 
      summarise(n = n_distinct(order.id)) %>% 
      filter(n > 1))

#more than five returns
nrow(orders_wret %>% 
       filter(returned == 1) %>% 
       group_by(customer.id) %>% 
       summarise(n = n_distinct(order.id)) %>% 
       filter(n > 5))

#q3,
#returns by region
orders_wret %>% 
  group_by(region.x, returned) %>% 
  summarise(no_returns = n_distinct(order.id)) %>% 
  mutate(colproduct = returned * no_returns,
         region = region.x) %>% 
  group_by(region) %>% 
  summarise(tot_sales = sum(no_returns),
            tot_returns = sum(colproduct),
            pct_return = tot_returns/tot_sales) %>% 
  arrange(desc(pct_return)) %>% 
  mutate(pct_return = round(100*pct_return, 2))

#q4,
#returns by category
orders_wret %>% 
  group_by(category, returned) %>%
  summarise(no_returns = sum(quantity)) %>% 
  mutate(colproduct = returned * no_returns) %>% 
  group_by(category) %>% 
  summarise(tot_sales = sum(no_returns),
            tot_returns = sum(colproduct),
            pct_return = tot_returns/tot_sales) %>% 
  arrange(desc(pct_return)) %>% 
  mutate(pct_return = round(100*pct_return, 2))
  
#returns by sub-category
orders_wret %>% 
  group_by(sub.category, returned) %>%
  summarise(no_returns = sum(quantity)) %>% 
  mutate(colproduct = returned * no_returns) %>% 
  group_by(sub.category) %>% 
  summarise(tot_sales = sum(no_returns),
            tot_returns = sum(colproduct),
            pct_return = tot_returns/tot_sales) %>% 
  arrange(desc(pct_return)) %>% 
  mutate(pct_return = round(100*pct_return, 2))