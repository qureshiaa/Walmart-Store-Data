getwd()

wm <- read.csv(file = 'Walmart_Store_sales.csv',stringsAsFactors = FALSE)

wmdata = as_tibble(wm)

wmdata

library(dplyr)

wmdata %>% group_by(Holiday_Flag) %>% summarise(n = n())

wmdata %>% group_by(Store) %>% summarise(n = n()) %>% print(n = 50)

str(wmdata)

# Which store has maximum sales


wmdata %>% group_by(Store) %>% summarise(Total = sum(Weekly_Sales)) %>% arrange(-Total)

max_sales = wmdata %>% group_by(Store) %>% summarise(max_rev = max(Weekly_Sales))

max_sales %>% arrange(-max_rev) %>% print(n = 50)

# Store  max_rev
# <int>    <dbl>
#   1    14 3818686.
# 2    20 3766687.
# 3    10 3749058.

# Which store has a maximum standard deviation i.e., 
# the sales vary a lot. Also, find out the coefficient of mean to standard deviation

min_max_sales = wmdata %>% group_by(Store,Weekly_Sales) %>% summarise(min_sales = min(Weekly_Sales),
                                         max_sales = max(Weekly_Sales))

min_max_sales %>% group_by(Store,Weekly_Sales) %>% summarise(diff_sales = max_sales - min_sales) %>%
  arrange(-diff_sales)


store_sd = wmdata %>% group_by(Store) %>% summarise(weeksd = sd(Weekly_Sales)) %>%
  arrange(weeksd)

store_sd %>% arrange(-weeksd) %>% print(n = 50)

# Store  weeksd
# <int>   <dbl>
#   1    14 317570.
# 2    10 302262.
# 3    20 275901.
# 4     4 266201.
# 5    13 265507.

dim(wmdata)

str(wmdata)

# Which store/s has a good quarterly growth rate in Q3'2012

distinct(wmdata$Date)

wmdata %>% 
  group_by(Store) %>% 
  filter(wmdata$Date2 > "2012-06-30" & wmdata$Date2 < "2012-10-01") %>% 
  summarise(Total = sum(Weekly_Sales))

library(lubridate)

class(wmdata$Date)
?as.Date

wmdata$Date

wmdata$date2<-format(as.Date(wmdata$Date, "%d-%m-%Y"), "%m-%d-%y")

wmdata$date2

str(wmdata)

wmdata$Date2 = as.Date(wmdata$date2, "%m-%d-%y")

wmdata

wmdata$Date3 <- format(wmdata$Date, '%m-%d-%Y')

wmdata %>% 
  group_by(Store) %>% 
  filter(wmdata$Date2 > "2012-06-30" & wmdata$Date2 < "2012-10-01") %>% 
  summarise(Total = sum(Weekly_Sales))

wmdata %>% group_by(Store) %>%
  filter(Date2 > "2012-06-30" & Date2 < "2012-10-01") %>% summarise(n = n()) %>% print(n = 50)

wmdata %>% group_by(Store) %>%
  filter(Date2 > "2012-06-30" & Date2 < "2012-10-01") %>% summarise(maximum = sum(Weekly_Sales)) %>%
  arrange(-maximum)

# Store   maximum
# <int>     <dbl>
#   1     4 27796792.
# 2    20 26891527.
# 3    13 26421259.
# 4     2 24303355.
# 5    10 23037259.
# 6    27 22307711.


# Some holidays have a negative impact on sales. Find out holidays 
# which have higher sales than the mean sales in a non-holiday season for all stores together


wmdata %>% group_by(Holiday_Flag) %>% summarise(Num.of.Rows = n()) %>% summarise(Total.sales = sum(Weekly_Sales))

str(wmdata)

wmdata %>% group_by(Holiday_Flag) %>%
  summarise(Total.sales = sum(Weekly_Sales))

store_weekday = wmdata %>% group_by(Store) %>% filter(Holiday_Flag == 0) %>%
  summarise(Average.sales = mean(Weekly_Sales))

store_weekday

store_trans = wmdata %>% group_by(Store,Holiday_Flag) %>% summarise(Num.of.Rows = n())

store_trans

wmdata %>% select(Date2,Holiday_Flag) %>% filter(Holiday_Flag == 1) 

# Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
# Labour Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
# Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
# Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13

wmdata = wmdata %>% 
  mutate(holiday = case_when(Date2 == "2012-02-12" | Date2 == "2011-02-11" |
                            Date2 == "2013-02-08" | Date2 == "2010-02-12"  ~ "Super Bowl",
                            Date2 == "2012-09-07" | Date2 == "2011-09-11" |
                              Date2 == "2013-09-06" | Date2 == "2010-09-10"  ~ "Labor Day",
                            Date2 == "2012-11-23" | Date2 == "2011-11-25" |
                              Date2 == "2013-11-29" | Date2 == "2010-11-26" ~ "ThanksGiving Break",
                            Date2 == "2012-12-28" | Date2 == "2011-12-30" |
                              Date2 == "2013-12-27" | Date2 == "2010-12-31" ~ "Christmas"))

wmdata %>% group_by(Holiday_Flag,holiday) %>% filter(holiday != "NA") %>%
  summarise(countingrows = n())