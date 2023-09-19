library(dplyr)
library(ggplot2)
library(stringr)
install.packages(pkgs = "caret")
library(caret)

real_estate_df <- read.csv("./real_estate_sales.csv", sep=",")
median_income_df <- read.csv("./income.csv")
summary(real_estate_df)
summary(median_income_df)
dim(real_estate_df)
str(real_estate_df)



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
  real_estate_df <- subset(real_estate_df, select = -c(Date.Recorded))
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
str(real_estate_df)


# plotting of graphs 
real_estate_df$Property.Type <- factor(real_estate_df$Property.Type)
real_estate_df$Residential.Type <- factor(real_estate_df$Residential.Type)
#ggplot(data=real_estate_df, aes(x="", y=Property.Type, group=Property.Type))
real_estate_df %>% group_by(Property.Type) %>% count() -> distribution_df1

ggplot(data=distribution_df1, aes(x="", y=n, fill=Property.Type)) +
  geom_col(color="black") +
  geom_text(aes(label = Property.Type), position = position_stack(vjust = 0.5))+
 coord_polar(theta="y")

#Sale Trend since 2006 
real_estate_df$List.Year <- factor(real_estate_df$List.Year)
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
  labs(title = "Property Type sales trend per year", x = "Number of sales",y = "Property Type") +
  geom_bar(aes(x=counts, y=Property.Type, fill=Property.Type), stat="identity", show.legend = FALSE) +
  facet_wrap(~ List.Year)

# Year wise sale trend of residential property type form 2006

real_estate_df %>%
  filter(Residential.Type != "") %>%
  group_by(List.Year, Residential.Type) %>%
  summarise(counts = n()) %>%
  ggplot() +
  labs(title = "Residential Type sales trend per year", x = "Number of sales in 1000's",y = "Residential Type") +
  geom_bar(aes(x=counts/1000, y=Residential.Type, fill=Residential.Type), stat="identity", show.legend = FALSE) +
  facet_wrap(~ List.Year)

# Year wise mean price trend of the residential property 
real_estate_df %>%
  filter(Residential.Type != "") %>%
  group_by(List.Year, Residential.Type) %>%
  summarise(mean_price = mean(Sale.Amount)) %>%
  ggplot() +
  labs(title = "Residential Type sales mean price trend per year", x = "mean of sales price in hundred thousand's",y = "Residential Type") +
  geom_bar(aes(x=mean_price/100000, y=Residential.Type, fill=Residential.Type), stat="identity", show.legend = FALSE) +
  facet_wrap(~ List.Year)
  
# Year wise  mean sales ratio trend of the residential property 
real_estate_df %>%
  filter(Residential.Type != "") %>%
  group_by(List.Year, Residential.Type) %>%
  summarise(mean_sale_ratio = mean(Sales.Ratio)) %>%
  ggplot() +
  labs(title = "Residential Type sales mean price trend per year", x = "mean of sales ratio in hundred thousand's",y = "Residential Type") +
  geom_bar(aes(x=mean_sale_ratio, y=Residential.Type, fill=Residential.Type), stat="identity", show.legend = FALSE) +
  facet_wrap(~ List.Year)

# The above results were intriguing, Year 2006 shows a pattern for sales ratio.
#Let us find out more about it
max(real_estate_df$Sales.Ratio)
min(real_estate_df$Sale.Amount)

sum(real_estate_df$Assessed.Value == real_estate_df$Sales.Ratio)
#There are close to 2067 rows which show that the property were sold for S1.
# Let us identify in which years were they seen the maximum

free_sales_of_property <- real_estate_df[(real_estate_df$Assessed.Value == real_estate_df$Sales.Ratio),]
free_sales_of_property %>% group_by(List.Year) %>% count()

# this could be erroneous data, or some government scheme. However, for price prediction, we could 
# go ahead and drop such a data because this isn't helping in further price prediction. Basically these are outliers
real_estate_df_after_outlier_removal <- real_estate_df[!(real_estate_df$Assessed.Value == real_estate_df$Sales.Ratio),]

# Use the previous analysis on new dataset of real estate
real_estate_df_after_outlier_removal %>%
  filter(Residential.Type != "") %>%
  group_by(List.Year, Residential.Type) %>%
  summarise(mean_sale_ratio = mean(Sales.Ratio)) %>%
  ggplot() +
  labs(title = "Residential Type sales mean price trend per year", x = "mean of sales ratio",y = "Residential Type") +
  geom_bar(aes(x=mean_sale_ratio, y=Residential.Type, fill=Residential.Type), stat="identity", show.legend = FALSE) +
  facet_wrap(~ List.Year)

# Top 10 town wise mean price trend for single family and condo residential types. 

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

town_list = c("Darien", "Greenwich", "New Canaan", "Roxbury", "Washington", "Weston", "Westport", "Wilton")

# Lets observe the median income of households in the top 10 towns obtained through previous analysis
# Data manipulation of income data set.
median_income_df_clean <- median_income_df[(median_income_df$Variable == "Per Capita Income"),]

median_income_df_clean[c("year-range-min", "year-range-max")] <- str_split_fixed(median_income_df_clean$Year, "-", 2)
head(median_income_df_clean)
median_income_df_clean <- subset(median_income_df_clean, select = -c(Year))
head(median_income_df_clean$Value)
#Since there is large deviation in the income value, let us apply min max scaling to the data


median_income_df_clean$Value = unclass(median_income_df_clean$Value)


head(median_income_df_clean)
median_income_df_clean %>% 
  filter(Town %in% town_list) %>%
  group_by(Town, Race.Ethnicity) %>%
  summarise(median_income = mean(Value)) %>%
  ggplot() +
  labs(title = "Median income trend of diversity for top 10 towns", x = "median income household",y = "Race.Ethnicity") +
  geom_bar(aes(x=median_income/1000, y=Race.Ethnicity, fill=Race.Ethnicity), stat="identity", show.legend = FALSE) +
  facet_wrap(~ Town)
  
  