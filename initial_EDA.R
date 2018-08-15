library(data.table)

orders = fread('../../data/Orders.csv', stringsAsFactors = T)
returns = fread('../../data/Returns.csv', stringsAsFactors = T)

names(orders) # check column names
names(returns) # check column names
names(returns)[names(returns)=='Order ID']='Order.ID' # rename "Order ID" to Order.ID

orders$Sales = as.numeric(gsub('[$,]','',as.character(orders$Sales))) # convert from factor to numeric
orders$Profit = as.numeric(gsub('[$,]','',as.character(orders$Profit))) # convert from factor to numeric
orders$Order.Date = as.Date(as.character(orders$Order.Date),format = '%m/%d/%y') # convert from factor to date
orders$Ship.Date = as.Date(as.character(orders$Ship.Date),format = '%m/%d/%y') # convert from factor to date
orders$Month = as.factor(month(orders$Order.Date)) # add column for month of order date
orders$Year = as.factor(year(orders$Order.Date)) # add column for year of order date

str(orders) # inspect structure
summary(orders)
levels(orders$Customer.Name)[grep('Kevin',levels(orders$Customer.Name))]


str(returns)


apply(orders,MARGIN=2,function(c) sum(is.na(c))) # see which columns have NAs

head(orders)

library(tidyverse)

# plot decrease in total inventory by month grouping by year
orders %>% 
  group_by(., Month, Year) %>% 
  summarise(., Decrease_Inventory = sum(Quantity)) %>% 
  ggplot(., aes(x=Month,y=Decrease_Inventory)) + geom_bar(aes(fill=Year), stat='identity', position = 'dodge')

# plot average yearly decrease in inventory by month and category
orders %>% 
  group_by(., Month, Year, Category) %>% 
  summarise(., Decrease_Inventory = sum(Quantity)) %>% 
  group_by(., Month, Category) %>%
  summarise(., Avg_Decrease_Inventory=mean(Decrease_Inventory)) %>%
  ggplot(., aes(x=Month,y=Avg_Decrease_Inventory)) + geom_bar(aes(fill=Category), stat='identity', position = 'dodge')

  
# merge order and return data frames by order id and region  
orders_returns = merge(x=orders, y=returns, by = c('Order.ID','Region'), all.x = T)

# plot total profit lost from returns by year
orders_returns %>% 
  filter(., Returned=='Yes') %>% 
  group_by(., Year) %>% 
  summarise(., Profit_Lost = sum(Profit)) %>%
  ggplot(., aes(x=Year,y=Profit_Lost)) + geom_bar(aes(fill=Year),stat='identity')

# number of customers who returned more than once
orders_returns %>% 
  filter(., Returned=='Yes') %>% 
  group_by(., Customer.ID, Order.ID) %>% 
  summarise(.) %>%
  group_by(., Customer.ID) %>%
  summarise(., num_returns=n()) %>%
  filter(., num_returns > 1) %>%
  nrow(.)

# num customers who returned more than 5 times
orders_returns %>% 
  filter(., Returned=='Yes') %>% 
  group_by(., Customer.ID, Order.ID) %>% 
  summarise(.) %>%
  group_by(., Customer.ID) %>%
  summarise(., num_returns=n()) %>%
  filter(., num_returns > 5) %>%
  nrow(.)

# regions most likely to return an order (by num returns)
orders_returns %>% 
  filter(., Returned=='Yes') %>% 
  group_by(., Region, Order.ID) %>% 
  summarise(.) %>%
  group_by(., Region) %>%
  summarise(., num_returns=n()) %>%
  arrange(., desc(num_returns))

# regions most likely to return an order (by return %)
orders_returns %>% 
  mutate(., Return_Yes=ifelse(is.na(Returned),0,1)) %>%
  group_by(., Region, Order.ID) %>%
  summarise(., return=mean(Return_Yes)) %>%
  group_by(., Region) %>%
  summarise(., pct_return=sum(return)/n()) %>%
  arrange(., desc(pct_return))

# category/sub.category most likely to be returned (by % of orders returned)
orders_returns %>% 
  mutate(., Return_Yes=ifelse(is.na(Returned),0,1)) %>%
  group_by(., Category, Sub.Category, Order.ID) %>%
  summarise(., return=mean(Return_Yes)) %>%
  group_by(., Category, Sub.Category) %>%
  summarise(., pct_return=sum(return)/n()) %>%
  arrange(., desc(pct_return))

# category/sub.category most likely to be returned (by % of quantity returned)
orders_returns %>% 
  mutate(., Return_Yes=ifelse(is.na(Returned),0,1)) %>%
  group_by(., Category, Sub.Category) %>%
  summarise(., pct_returned = sum(Return_Yes)/n()) %>%
  arrange(., desc(pct_returned))

