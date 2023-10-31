library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(lubridate)
library(tseries)
library(aTSA)
library(TidyDensity)


real_estate_df <- read.csv("./real_estate_sales.csv", sep=",")
median_income_df <- read.csv("./income.csv")
summary(real_estate_df)
summary(median_income_df)
dim(real_estate_df)
dim(median_income_df)
str(real_estate_df)
str(median_income_df)
View(median_income_df)


# Remove columns that we do not think add value at this point of time.
# Remove rows which have null values after careful examination.
clean_up_dataset = function() {
  
  sum(real_estate_df$Assessor.Remarks!="")
  real_estate_df <- subset(real_estate_df, select = -c(Assessor.Remarks))
  sum(real_estate_df$OPM.remarks!="")
  real_estate_df <- subset(real_estate_df, select = -c(OPM.remarks))
  sum(real_estate_df$Location!="")
  real_estate_df <- subset(real_estate_df, select = -c(Location))
  sum(real_estate_df$Property.Type=="")
  sum(real_estate_df$Residential.Type == "") 
  sum(real_estate_df$Property.Type == "" & real_estate_df$Residential.Type == "")
  null_values_property_residential <- real_estate_df[(real_estate_df$Property.Type == "" & real_estate_df$Residential.Type == ""),]
  groups <- null_values_property_residential %>% group_by(List.Year) %>% summarise(total_count = n(), .groups = 'drop')
  print(groups)
  # This data shows that we are losing maximum in the initial years from 2001 to 2005. There after the loss is not significant. 
  #Since we have large data set, we can assume that if we lose this data the impact is not significant.
  real_estate_df <- real_estate_df[!(real_estate_df$Property.Type == "" & real_estate_df$Residential.Type == ""),]
  
  sum(real_estate_df$Non.Use.Code == "") 
  real_estate_df <- subset(real_estate_df, select = -c(Non.Use.Code))
  #real_estate_df <- subset(real_estate_df, select = -c(Date.Recorded))
  return(real_estate_df)
  
}

#some of the rows of property type show duplicity . Example: values in column is one family, two family etc. And the same 
#value seems to be repeated in residential type. We can then modify the values as residential instead of one family or two family. 
#since, if commercial the corresponding values in residential type would be empty.
change_column_values = function() {
  unique(real_estate_df$Property.Type)
  temp_values <- c("Single Family", "Two Family", "Three Family", "Four Family", "Condo")
  sum(real_estate_df$Property.Type %in% temp_values)
  sum(real_estate_df$Property.Type == "Residential")
  real_estate_df$Property.Type[real_estate_df$Property.Type %in% temp_values & real_estate_df$Residential.Type != ""] <- "Residential"
  sum(real_estate_df$Property.Type == "Residential")
  unique(real_estate_df$Residential.Type)
  return(real_estate_df)
}



real_estate_df <- clean_up_dataset()
real_estate_df <- change_column_values();


#-----------Simple analysis on property Type-------
# This graph is to inform what type of property sale was maximum in the last 15 to 20 years.
real_estate_df %>% group_by(Property.Type) %>% summarise(count = n()) -> distribution_df1

ggplot(data=distribution_df1, aes(x="", y=count/1000, fill=Property.Type)) +
  geom_col(color="black") +
  coord_polar(theta="y")


#Sale Trend since 2006 . 
#Which year was the sale maximum ? 
real_estate_df %>% 
  group_by(List.Year) %>% 
  summarise(count = n()) %>%
  ggplot() +
  geom_bar(aes(x=List.Year, y=count, fill=List.Year), stat="identity", show.legend = FALSE)



# Year wise sale chart for each property type from 2006
real_estate_df %>%
  group_by(List.Year, Property.Type) %>%
  summarise(counts = n()) %>%
  ggplot() +
  labs(title = "Property Type sales trend per year", x = "Number of sales * 100",y = "Property Type") +
  geom_bar(aes(x=counts/100, y=Property.Type, fill=Property.Type), stat="identity", show.legend = FALSE) +
  facet_wrap(~ List.Year)
# ---------- Analysis ----------------------------------------------
# The maximum trading is in residential types. There is rise in vacant land trading in the year 2020
# ------------------------------------------------------------------


# Since the previous graph clearly indicates that it is residential property type that was traded
# maximum, we try to visualize year wise sale trend of residential property type form 2006
real_estate_df %>%
  filter(Residential.Type != "") %>%
  group_by(List.Year, Residential.Type) %>%
  summarise(counts = n()) %>%
  ggplot() +
  labs(title = "Residential Type sales trend per year", x = "Number of sales * 1000",y = "Residential Type") +
  geom_bar(aes(x=counts/1000, y=Residential.Type, fill=Residential.Type), stat="identity", show.legend = FALSE) +
  facet_wrap(~ List.Year)
# -----Analysis----------------------------------------------------
# Single family residential types are the favorites of Connecticut. There is fluctuation when residential type 
# is condo. It is seen to be highest in 2020. Possibly marking influx of people from other regions.?? More people prefering
#not staying with family? Or simply rise in population ? Unfortunately, the data cannot answer these questions yet.
# (it is an  open ended question)
#---------------------------------------------------------------------

# Year wise mean price trend of the residential property 
real_estate_df %>%
  filter(Residential.Type != "") %>%
  group_by(List.Year, Residential.Type) %>%
  summarise(mean_price = mean(Sale.Amount)) %>%
  ggplot() +
  labs(title = "Residential Type sales mean price trend per year", x = "mean of sales price  * 100000$ ",y = "Residential Type") +
  geom_bar(aes(x=mean_price/100000, y=Residential.Type, fill=Residential.Type), stat="identity", show.legend = FALSE) +
  facet_wrap(~ List.Year)
# ----------------------Analysis------------------------------
# prices of single family type have been the highest from 2006 to 2020. In 2019, prices of four Family 
# properties shows significant rise and significant drop in prices in 2020. Was covid the reason??
# prices for Two Family and Three Family are very close until 2019.


# Year wise  mean sales ratio trend of the residential property 
#mean_sale_ratio is (Assessed price / Sale price)
real_estate_df %>%
  filter(Residential.Type != "") %>%
  group_by(List.Year, Residential.Type) %>%
  summarise(mean_sale_ratio = mean(Sales.Ratio)) %>%
  ggplot() +
  labs(title = "Residential Type sales ratio mean price trend per year", x = "mean of sales ratio in hundred thousand's",y = "Residential Type") +
  geom_bar(aes(x=mean_sale_ratio, y=Residential.Type, fill=Residential.Type), stat="identity", show.legend = FALSE) +
  facet_wrap(~ List.Year)
# ----------------------Analysis------------------------------
# The above results were intriguing, Year 2006 shows a pattern for sales ratio.
#Let us find out more about it

max(real_estate_df$Sales.Ratio)
max(real_estate_df$Sale.Amount)

sum(real_estate_df$Assessed.Value == real_estate_df$Sales.Ratio)
#There are close to 2067 rows which show that the property were sold for S1.
#This could be a possible erroneous data, since the data source mentions that the trade should have
# minimum of 2000$ price.
# Let us identify in which years were they seen the maximum

free_sales_of_property <- real_estate_df[(real_estate_df$Assessed.Value == real_estate_df$Sales.Ratio),]
free_sales_of_property %>% group_by(List.Year) %>% count()

# this could be erroneous data, or some government scheme. However, for price prediction, we could 
# go ahead and drop such a data because this isn't helping in further price prediction.
real_estate_df_after_outlier_removal <- real_estate_df[!(real_estate_df$Assessed.Value == real_estate_df$Sales.Ratio),]

# Use the previous analysis on new dataset of real estate
real_estate_df_after_outlier_removal %>%
  filter(Residential.Type != "") %>%
  group_by(List.Year, Residential.Type) %>%
  summarise(mean_sale_ratio = mean(Sales.Ratio)) %>%
  ggplot() +
  labs(title = "Residential Type sales mean sales ratio per year", x = "mean of sales ratio",y = "Residential Type") +
  geom_bar(aes(x=mean_sale_ratio, y=Residential.Type, fill=Residential.Type), stat="identity", show.legend = FALSE) +
  facet_wrap(~ List.Year)

# Top 10 town wise mean price trend for single family and condo residential types. These two types were chosen considering that they
# are the favorites of the population
# We want to filter out the top 10 towns in Connecticut where the prices for residential property specifically 
# for Single Family and Condo is high

unique(real_estate_df_after_outlier_removal$Town)
real_estate_df_after_outlier_removal$List.Year = unclass(real_estate_df_after_outlier_removal$List.Year)
real_estate_df_after_outlier_removal %>%
  filter(Residential.Type %in% c("Single Family", "Condo") & List.Year > "2015") %>%
  group_by(Town, Residential.Type) %>%
  summarise(mean_sale_amount = mean(Sale.Amount)) %>%
  arrange(desc(mean_sale_amount)) %>%
  head(10) %>%
  ggplot() +
  labs(title = "Residential Type sales mean price trend per town for top 10 towns", x = "mean of sales amount",y = "Residential Type") +
  geom_bar(aes(x=mean_sale_amount/100000, y=Residential.Type, fill=Residential.Type), stat="identity", show.legend = FALSE) +
  facet_wrap(~ Town)

#-------------Based on the above data, we have our top 10 towns with highest mean real estate 
# sale price--------------
town_list = c("Darien", "Greenwich", "New Canaan", "Rocky Hill", "Washington", "Weston", "Westport", "Willington")


# Lets observe the median income of households in the top 10 towns obtained through previous analysis
# Data manipulation of income data set.
median_income_df_clean <- median_income_df[(median_income_df$Variable == "Per Capita Income"),]

# The Year is a range of 5 years. We are splitting with range min and max for better analysis
median_income_df_clean[c("year-range-min", "year-range-max")] <- str_split_fixed(median_income_df_clean$Year, "-", 2)
head(median_income_df_clean)
View(median_income_df_clean)
median_income_df_clean <- subset(median_income_df_clean, select = -c(Year))
head(median_income_df_clean$Value)
median_income_df_clean$Value = unclass(median_income_df_clean$Value)
head(median_income_df_clean)
median_income_df_clean$`year-range-max` <- as.numeric(median_income_df_clean$`year-range-max`)



# Mean household income across different diversity.
# Reason could be lesser population of some diverse groups.
# This gives an overview of concentration of diverse groups in the top 10 expensive towns from 2005 to 2020
median_income_df_clean %>% 
  filter(Town %in% town_list) %>%
  group_by(Town, Race.Ethnicity) %>%
  summarise(median_income = mean(Value)) %>%
  ggplot() +
  labs(title = "Median income trend of diversity for top 10 towns", x = "median income household * 1000",y = "Race.Ethnicity") +
  geom_bar(aes(x=median_income/1000, y=Race.Ethnicity, fill=Race.Ethnicity), stat="identity", show.legend = FALSE) +
  facet_wrap(~ Town)


median_income_df_clean %>% 
  group_by(`year-range-max`) %>%
  summarise(median_income = mean(Value)) %>%
  ggplot() +
  labs(title = "Median income trend overall", x = "median income household * 1000",y = "Year") +
  geom_bar(aes(y=median_income/1000, x=`year-range-max`, fill=(median_income/1000)), stat="identity", show.legend = FALSE)
  
  
  # Year wise per capita income change of the population of top 10 towns.
  median_income_df_clean %>% 
  filter(Town %in% town_list) %>%
  group_by(Town, `year-range-max`) %>%
  summarise(median_income = mean(Value)) %>%
  ggplot() +
  labs(title = "Income change in top 10 towns from 2009 to 2020", x = "Year",y = "Average Per capita income * 1000") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  geom_bar(aes(x=`year-range-max`, y=median_income/1000, fill=median_income/1000), stat="identity", show.legend = FALSE) +
  facet_wrap(~ Town)





# We plot a graph with median income trend over the period of 10 years and make a comparative analysis
# of Property sale trend vs median household Income in top 10 towns of Connecticut State
real_estate_df_after_outlier_removal$Date.Recorded <- as.Date(real_estate_df_after_outlier_removal$Date.Recorded, "%m/%d/%Y")

head(real_estate_df_after_outlier_removal)
dim(real_estate_df_after_outlier_removal)
real_estate_df1$Sale.Amount

# We want to analyse Time Series data. We will construct different data frames to suit our needs from here on.
# Constructing a smaller data set to analyse average sale prices over the period 
time_sales <-  real_estate_df_after_outlier_removal %>%
  mutate(month = format(Date.Recorded, "%m"), year = format(Date.Recorded, "%Y")) %>%
  group_by(month, year) %>%
  summarise(avg_sale_amount = mean(Sale.Amount))

head(time_sales)
dim(time_sales)

# Constructing date using just month and year. Date of the month may not be significant at this time.
time_sales$date <- paste(time_sales$year,"-",time_sales$month,"-01", sep="")
time_sales$date <- as.Date(time_sales$date)

ggplot(time_sales, aes(x=date, y=(avg_sale_amount/1000))) + 
  geom_boxplot()

# The result shows that there is a posibility of outlier. Upon further analysis using summary, we chip the data 
# to get better analysis
summary(time_sales$avg_sale_amount )

# Filtering time sales, because we see a few outliers, affecting the average 
time_sales1 <- time_sales[time_sales$avg_sale_amount < 600000, ]
time_sales2 <- time_sales[(time_sales$year > 2009 & time_sales$avg_sale_amount < 600000), ]
ggplot(time_sales1, aes(x=date, y=(avg_sale_amount/1000))) + 
  geom_boxplot()
summary(time_sales2$avg_sale_amount )
dim(time_sales2)
head(time_sales2)

ggplot(data=time_sales2, aes(x=date, y=avg_sale_amount/1000, group = 1)) + 
  geom_line()+
  scale_x_date(date_labels = "%Y", date_breaks = "year")
# We notice suddend drift in the timeline in 2020. This is because of 1 property that was sold for a very high 
#price.As a result the average price increased and a drift was noticed.

time_sales3 <- time_sales[(time_sales$year > 2016 & time_sales$avg_sale_amount < 600000), ]
ggplot(data=time_sales3, aes(month(date, label=TRUE, abbr=TRUE), y=avg_sale_amount/1000, group=factor(year(date)), colour=factor(year(date)))) +
  geom_line() +
  labs(x="Month", colour="Year")

# Checking Seasonality of the data set
time_sales4 <- time_sales[(time_sales$year < 2020 & time_sales$year > 2006 & time_sales$avg_sale_amount < 600000), ]
time_sales4$date <- paste(time_sales4$year,"-",time_sales4$month,"-01", sep="")
time_sales4$date <- as.Date(time_sales4$date)
ggplot(data=time_sales4, aes(month(date, label=TRUE, abbr=TRUE), y=avg_sale_amount/1000, group=factor(year(date)), colour=factor(year(date)))) +
  geom_line() +
  labs(x="Month", colour="Year")
head(real_estate_df_after_outlier_removal)

# after removing sale price outlier and plotting the trend. Shows seasonality but no clear trend.
time_sales11 <- time_sales[(time_sales$year > 2009 & time_sales$avg_sale_amount < 600000), ]
time_sales11$date <- paste(time_sales11$year,"-",time_sales11$month,"-01", sep="")
time_sales11$date <- as.Date(time_sales11$date)
ggplot(data=time_sales11, aes(x=date, y=avg_sale_amount/1000, group = 1)) + 
  geom_line()+
  scale_x_date(date_labels = "%Y", date_breaks = "year")





time_sales5 <- real_estate_df_after_outlier_removal %>%
  mutate(month = format(Date.Recorded, "%m"), year = format(Date.Recorded, "%Y")) %>%
  group_by(month, year) %>%
  summarise(no_of_sale = n())
time_sales5$date <- paste(time_sales5$year,"-",time_sales5$month,"-01", sep="")
time_sales5$date <- as.Date(time_sales5$date)




#Data Non Stationary or Stationary ?
df <- time_sales5
df <- time_sales5[order(time_sales5$date), ]
head(time_sales6)

# In order to handle the data drift for the year 2020, we chip the data to analyse the trend by removing the outlier.
# We are restricting the analysis between 2006 to 2019. 
# We can also remove a few outliers and run the analysis without filtering for year.
time_sales5 %>%
  filter(year > 2006 & year < 2019) %>%
  ggplot(aes(month(date, label=TRUE, abbr=TRUE), y=no_of_sale, group=factor(year(date)), colour=factor(year(date)))) +
  geom_line() +
  labs(x="Month", colour="Year") +
  title("Sale trend for each month from 2007 to 2018")

time_sales6 <- subset(time_sales5, year > 2006 & year < 2019 , select = c('date', 'no_of_sale'))
df <- time_sales6[order(time_sales6$date), ]
head(df)
df$no_of_sale <- as.numeric(df$no_of_sale)

# Performing ADF and KPSS test to rule out non stationarity in the data set
adf.test(df$no_of_sale)
time_sales7 <-  subset(time_sales5, select = c('date', 'no_of_sale'))
df1 <- time_sales7[order(time_sales7$date), ]
df1$no_of_sale <- as.numeric(df1$no_of_sale)
aTSA::adf.test(df1$no_of_sale)

time_sales8 <-  subset(time_sales4, select = c('date', 'avg_sale_amount'))
df2 <- time_sales8[order(time_sales8$date), ]
df2$avg_sale_amount <- as.numeric(df2$avg_sale_amount)
aTSA::adf.test(df2$avg_sale_amount)
aTSA::adf.test(diff(df2$avg_sale_amount/1000))

plot(cvar(diff(df2$avg_sale_amount/1000)), type = "l")

aTSA::stationary.test(df2$avg_sale_amount, method= "kpss")
aTSA::stationary.test(df2$avg_sale_amount, method= "pp")
aTSA::stationary.test(df2$avg_sale_amount, nlag=13)
acf(df2$avg_sale_amount) 
pacf(df2$avg_sale_amount) 
head(time_sales4)
time_sales4 <- time_sales4[order(time_sales4$date), ]
plot(acf(fitSalesYearMonth$residuals)) 


# Although as per adf test, we reject null hypothesis in favour of Alternate hypothesis, we want to run confirmatory tests
# We try to fit a linear regression model to verify linear time trend. We fit regression model against a 
# simple index number which keeps a track of year.
time_sales4$t <- 1:nrow(time_sales4) 
head(time_sales4)
View(time_sales4)
fitSalesYearIndex <- glm(log(avg_sale_amount) ~ t, data=time_sales4)
plot(acf(fitSalesYearIndex$residuals)) 
coef(fitSalesYearMonth) 
fitSalesYearMonth <- glm(log(avg_sale_amount) ~  month , data=time_sales4)
coef(fitSalesYearMonth) 
plot(acf(fitSalesYearMonth$residuals)) 

fitSalesYearIndexMonth <- glm(log(avg_sale_amount) ~ t + month , data=time_sales4)
plot(fitSalesYearIndexMonth$residuals, type="l")
# Mean Reverting series type of Auto regression Model
n <- length(time_sales4)

sale_amount <- log(time_sales4$avg_sale_amount)
coef(ARdj <- glm(sale_amount[-1] ~ sale_amount[-n]))

# We create AR model by adding lag to the input. 
time_sales4$lag1 <- c(NA, time_sales4$avg_sale_amount[-nrow(time_sales4)])
time_sales4[1:3,]
fitAR1 <- glm(log(avg_sale_amount) ~ log(lag1) + t + month, data=time_sales4)
plot((fitAR1$residuals), type="l")
coef(fitAR1) 
#co-eff, Beta1  = 0.8 , which is less than 1. The values are mean reverting.
# 




