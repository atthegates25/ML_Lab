

data = read.csv("./Orders.csv")
summary(data)

data %>% head()

data$Sales = as.numeric(gsub('[$,]', '', data$Sales))
data$Profit = as.numeric(gsub('[$,]', '', data$Profit))

#data %>% head()
str(data)

data$Order.Date <- as.Date(data$Order.Date,"%m/%d/%y")

data$year = year(data$Order.Date)
data$month= month(data$Order.Date)

str(data)

### Problem 2: Inventory Management 
data %>% group_by(month, Category) %>% summarise(Quant = sum(Quantity)/4) %>% ggplot(aes(x=month, y= Quant)) +
  geom_bar(aes(fill= Category),stat="identity", position = "dodge")

### Problem 3: Why did customers make returns?
returns = read.csv("./Returns.csv")

data$Order.ID = as.character(data$Order.ID)
returns$Order.ID = as.character(returns$Order.ID)

#mutate(returns = ifelse(Returned == "Yes", 1, 0 ))


data1 = data %>% left_join(returns, by ="Order.ID")

#1. How much profit did we lose due to returns each year?



#2. How many customer returned more than once? more than 5 times?

data %>% group_by(Customer.ID) %>% summarise(returns)

nrow(data1 %>% filter(Returned == 'Yes') %>% group_by(Customer.ID) %>% summarise(n=n_distinct(Order.ID)) %>% filter(n>5))
#str(data1)

#3. Which regions are more likely to return orders?


#data1 %>% group_by(Region.x, Returned) %>% summarise(no_returns = n_distinct(Order.ID))




orders_returns = 
  mutate(data1, Return_Yes=ifelse(is.na(Returned),0,1)) %>%
  group_by(., Region.x, Order.ID) %>%
  summarise(., return=mean(Return_Yes)) %>%
  group_by(., Region.x) %>%
  summarise(., pct_return=sum(return)/n()) %>%
  arrange(., desc(pct_return))
orders_returns

no_orders = data1 %>% sum()
  
returns_category = mutate(data1, Return_Yes=ifelse(is.na(Returned),0,1)) %>%
  group_by(., Category) %>%
  summarise(., return=sum(Return_Yes)/sum()) 
returns_category

str(data1)


orders_ret %>%
  group_by(sub.category, returned) %>%
  summarise(no_returns = sum(quantity)) %>%
  mutate(colproduct = returned * no_returns) %>%
  group_by(sub.category) %>%
  summarise(tot_sales = sum(no_returns),
            tot_returns = sum(colproduct),
            pct_return = tot_returns/tot_sales) %>%
  arrange(desc(pct_return)) %>%
  mutate(pct_return = round(100*pct_return, 2))

