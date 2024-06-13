#Problem Statement:
#Sales management has gained importance to meet increasing competition and the
#need for improved methods of distribution to reduce cost and to increase profits. Sales
#management today is the most important function in a commercial and business
#enterprise.
library(dplyr)
library(ggplot2)
df<-read.csv("C:\\Users\\prach\\Downloads\\Amazon Sales data.csv")
#now, first determine if there's any missing value
anyNA(df)
str(df)
#Time trend analysis of amazon sales
df$Order.Date <- as.Date(df$Order.Date, format = "%m/%d/%Y")
df$Order.year<-format(df$Order.Date,format="%Y")
profit_by_year <- df %>%
  group_by(Order.year) %>%
  summarise(Total.Profit = sum(Total.Profit))
str(profit_by_year)
profit_by_year$Order.year<-as.Date(profit_by_year$Order.year,format="%Y")
pl1<-ggplot(profit_by_year,aes(x=Order.year,y=Total.Profit))+geom_line(colour="blue")+
  labs(title = "Time Trends of Sales",
       x = "Year",
       y = "Total Profit")
pl1
#plot 1 provides insights that between 2011 to 2013 the profits were increasing but then it came down so we need to focus on how to improve profits
colnames(df)
#Region based analysis --> Average profits per region
Region_Average_Profits<-df %>% group_by(Region) %>% summarise(average_profits=mean(Total.Profit)) %>% arrange(desc(average_profits))
Region_Average_Profits <- Region_Average_Profits %>%
  mutate(average_profits_percent = round((average_profits / sum(average_profits)) * 100,2))

pl2<-ggplot(Region_Average_Profits, aes(x="", y=average_profits_percent, fill=Region)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+geom_text(aes(label=average_profits_percent), position=position_stack(vjust = 0.2),size=3,fontface = 'bold')+
  labs(title = "Regions with Average Profits",
       x = "Average Profit Percenetage",
       y= "Average Profit Percentage")
pl2
#The Middle East has the highest average profits, suggesting strong market performance. 
#Consider increasing focus and investment in this region.

#Top 5 Item Types generating most Profits
Top_5_Items<-df %>% group_by(Item.Type) %>% 
  summarise(Total_profits=sum(Total.Profit))%>%
  arrange(desc(Total_profits)) %>% head(5)
pl3<-ggplot(Top_5_Items,aes(x=reorder(Item.Type,-Total_profits),y=Total_profits))+geom_bar(fill='darkblue',stat='identity')+
  geom_text(aes(label=Total_profits),position=position_stack(vjust=0.5),size=3,color="white",fontface="bold")+labs(title="Types of Items generating most profits",x='Item Types',y='Total Profits')
pl3
#Given that cosmetics and household items are the top profit-generating categories
#so consider focusing your resources and investment on further developing and expanding these product lines. 
#This could involve introducing new product variants, improving existing formulations, or enhancing packaging etc.a

# top countries as per total profits and items
Profit_Country <- df %>%
  group_by(Country, Item.Type) %>% 
  summarise(Total_profits = sum(Total.Profit)) %>% arrange(desc(Total_profits))
top_5_countries<-Profit_Country%>%group_by(Country)%>%summarise(Total_profits = sum(Total_profits))%>%arrange(desc(Total_profits))%>%head(5)
filterd_data<-Profit_Country %>% filter(Country %in% top_5_countries$Country)
#plot4
custom_colors <- c("Cosmetics" = "darkblue", "Household" = "blue3", "Cereal" = "deepskyblue4","Clothes"="deepskyblue","Snacks"="lightblue")
pl4<-ggplot(filterd_data,(aes(x=Country,y=Total_profits,fill = reorder(Item.Type,-Total_profits))))+
  geom_bar(stat = 'identity')+scale_fill_manual(values = custom_colors) + labs(x="Country",y="Total Profits",fill="Item Types")+
  geom_text(aes(label = Total_profits),color="white",fontface='bold', position = position_stack(vjust = 0.5), size = 3)
pl4
#This plot shows that Djibouti has highest profits out of all 5 countries and its major profit generated from orders in cosmetics
#Also out of Top 5 countries cosmetics items are generating most profits
#Based on the analysis, it appears that focusing on cosmetics sales could be a lucrative strategy for increasing profits. 
#This could involve further investment in marketing, distribution channels, or product development specifically tailored to the cosmetics market.
#Also in To increase sales of snacks in Honduras, Conduct market research to understand consumer preferences, tastes, and purchasing behavior related to snacks in Honduras. 
#Identify popular snack types, flavors, and packaging formats that resonate with the local population.
#In pakistan, Consider reallocating resources towards the cosmetics category by expanding your product offerings, introducing new cosmetic products, or enhancing existing ones.

#now, I will analyse which sales channel people of different top countries prefer more
sales_channel<-df %>% group_by(Country,Sales.Channel) %>%
  summarise(total_profits=sum(Total.Profit)) %>% 
  arrange(desc(total_profits))
filter_data <- sales_channel %>%
  filter(Country %in% top_5_countries$Country)
#plot5
pl5<- ggplot(filter_data,aes(x=reorder(Country,total_profits),y=total_profits,fill=Sales.Channel))+
  geom_bar(stat='identity')+scale_fill_manual(values = c("Offline" = "darkblue", "Online" = "blue")) +
  geom_text(aes(label = total_profits),color='white', position = position_stack(vjust = 0.5), size = 3,fontface='bold')+
  labs(title="Sales Channel Analysis",x="Country",y="Total Profits",fill="Sales Channel")
pl5
#The preference for purchasing products offline might be dominant in most countries due to factors such as accessibility, trust in physical stores, or cultural norms.
#Samoa's exclusive reliance on online sales could indicate an untapped market potential for online retailers. 
#In samoa's it might be an opportunity for businesses to invest in e-commerce infrastructure, digital marketing etc

#now lets analyse which item is most costly and needs to get modified so that to become affordable to others
most_costly_items<- df %>% 
  select(Item.Type,Unit.Price,Unit.Cost)%>%
  unique()%>%
  arrange(desc(Unit.Cost))%>%head(5)
str(most_costly_items)
pl6<-ggplot(most_costly_items, aes(x = Unit.Price, y = Unit.Cost, color = reorder(Item.Type,-Unit.Cost))) +
  geom_point() +
  geom_text(data = subset(most_costly_items, Item.Type != "Baby Food"), 
            aes(label = paste("(", round(Unit.Price, 2), ",", round(Unit.Cost, 2), ")"),fontface="bold"), 
            vjust = 0.2, hjust = 1.2, check_overlap = TRUE) +
  geom_text(data = subset(most_costly_items, Item.Type == "Baby Food"), 
            aes(label = paste("(", round(Unit.Price, 2), ",", round(Unit.Cost, 2), ")"),fontface="bold"), 
            vjust = -0.3, hjust = -0.1) +
  labs(x = "Unit Price", y = "Unit Cost", color = "Item Type")
pl6
# In plot 6 it is clearly seen that the high unit cost of household items and office supplies
#it suggests an opportunity for cost optimization. 
#By identifying cost-saving measures in production, sourcing, etc. 
#can lower the unit cost of these items, thereby improving profit margins.

#plot 7 
str(df)
df$Ship.Date<-as.Date(df$Ship.Date,format="%m/%d/%Y")
df <- df %>%
  mutate(Shipment.Time = as.numeric(Ship.Date - Order.Date))
country_shipment_time <- df %>%
  group_by(Country,Item.Type,Sales.Channel) %>%
  summarise(Average_Shipment_Time = mean(Shipment.Time, na.rm = TRUE)) %>%
  arrange(desc(Average_Shipment_Time))%>% head(5)
country_shipment_time$Item.Type<- as.factor(country_shipment_time$Item.Type)
country_shipment_time$Country <- ifelse(country_shipment_time$Country == "Democratic Republic of the Congo", 
                                        "Congo DR", 
                                        country_shipment_time$Country)
country_shipment_time$Country<-as.factor(country_shipment_time$Country)
str(country_shipment_time)
pl7<-ggplot(country_shipment_time, aes(x = Country, y = Average_Shipment_Time, group=1)) +
  geom_line(color="darkblue") + 
  geom_point(aes(color=Item.Type,size=3))+
labs(title = "Average Shipment Time by Country and Item Type",
       x = "Country",
       y = "Average Shipment Time") +
  theme_minimal()
pl7

#The Democratic Republic of the Congo (DRC) has the highest mean shipment time for beverages.
#This suggests that there may be significant logistical or supply chain issues affecting the delivery of beverages in the DRC.
#So invest in Infrastructure,Implementing advanced supply chain management practices can help improve supply chain
#Also For items like fruits, implementing cold chain logistics and faster transportation options can prevent spoilage and reduce delays.
last_years<-subset(df,Order.year=="2016" | Order.year=="2017")
str(df)
last_years_profits <- last_years %>%
  group_by(Region, Order.year) %>%
  summarise(Total_Profit = sum(Total.Profit, na.rm = TRUE))
last_years_profits <- last_years_profits %>%
  spread(Order.year, Total_Profit, fill = 0) %>%
  rename(Profit_2016 = `2016`, Profit_2017 = `2017`) %>%
  mutate(Change_in_Profit = Profit_2017 - Profit_2016)

pl8 <- ggplot(last_years_profits, aes(x = reorder(Region, -Change_in_Profit), y = Change_in_Profit, group = 1)) +
  geom_line(color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(title = "Change in Profits by Region (2016 to 2017)",
       x = "Region",
       y = "Change in Profit") +
  theme_minimal()
pl8

#Analyze and replicate the successful strategies from Sub-Saharan Africa and Central America in other regions.
#Continue to invest in and support the growing markets in these regions to sustain and further enhance profitability.
#In Loss-Making Regions implement cost-reduction strategies to improve profit margins.
