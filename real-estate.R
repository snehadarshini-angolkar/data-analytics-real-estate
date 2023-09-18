library(dplyr)
real_estate_df <- read.csv("./real_estate_sales.csv", sep=",")
median_income_df <- read.csv("./income.csv")
summary(real_estate_df)
summary(median_income_df)
dim(real_estate_df)
str(real_estate_df)
clean_up_dataset()
str(real_estate_df)


# Remove columns that we do not think add value at this point of time.
# Remove rows which have null values after careful examination.
clean_up_dataset() = function() {

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

  }

