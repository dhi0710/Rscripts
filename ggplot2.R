sales_day <- financial_sample %>% group_by(Date) %>% summarise(daily_sales = sum(Sales))
sales_day_p <- financial_sample %>% group_by(Date, Product) %>% summarise(daily_sales = sum(Sales))
profit_product <- financial_sample %>% group_by(Product) %>% summarise(total_profit = sum(Profit))

product_plot <- ggplot(sales_day_p, aes(Date,Product))

p1 <- ggplot2(product_plot) +
  geom_col(aes(x = Date, y = daily_sales, fill = Product)) 

agg_ord <- mutate(sales_day_p,
                  Hair = reorder(Date, -n, sum),
                  Eye = reorder(Product, -n, sum))

#here---------------------------------------------------------------------------------
sales_day_p <- financial_sample %>% group_by(Date, Product) %>% summarise(daily_sales = sum(Sales))
product_plot <- ggplot(sales_day_p, aes(Date,Product))
product_plot + geom_bar(aes(fill=Product,x = Date, y = daily_sales), stat="identity", width = 20) + 
  labs(title="Financial Sample", 
       subtitle="Daily Sales of Products", 
       caption="Source: Financial Sample") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + scale_y_continuous(name="Sales", labels = comma)
#here---------------------------------------------------------------------------------

geom_bar(aes(fill=class), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Manufacturers from 'mpg' dataset")

theme_set(theme_classic())
ggplot(sales_day, aes(x=Date)) + 
  geom_line(aes(y=daily_sales)) + 
  labs(title="Daily Sales Chart", 
       subtitle="Total of Daily Sales", 
       caption="Source: Financial Sample", 
       y="Sales") + scale_y_continuous(name="Sales", labels = comma)

# Allow Default X Axis Labels
ggplot(sales_day, aes(x=Date)) + 
  geom_line(aes(y=daily_sales)) + 
  labs(title="Daily Sales Chart", 
       subtitle="Total of Daily Sales", 
       caption="Source: Financial Sample", 
       y="Sales") + scale_y_continuous(name="Sales", labels = comma)

library(ggplot2)
theme_set(theme_classic())
profit_product <- financial_sample %>% group_by(Product) %>% summarise(total_profit = sum(Profit))
profit_product$Label <- paste(round(profit_product$total_profit/1000000,2),"M")
p <-  ggplot(profit_product, aes(x = 1, y = total_profit, fill = Product)) + geom_bar(stat = "identity")
p + coord_polar(theta = 'y') + theme_void() + geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) + labs(fill="Product",title="Pie Chart of Profit by Product",caption="Source: Financial Sample") 





pie <- ggplot(profit_product, aes(x = "", y=total_profit, fill = factor(Product))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank()) + 
  labs(fill="Product", 
 
       title="Pie Chart of class", 
       caption="Source: mpg") 
pie + coord_polar(theta = "y", start=0) + scale_y_continuous(label=scales::comma) + geom_text(aes(label = Label), position = position_stack(vjust = 0.5))