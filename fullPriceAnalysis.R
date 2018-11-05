if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tibble")) install.packages("tibble")
if (!require("lubridate")) install.packages("lubridate")
if (!require("stargazer")) install.packages("stargazer")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("MASS")) install.packages("MASS")
if (!require("leaps")) install.packages("leaps")
if (!require("car")) install.packages("car")
library(tidyverse)
library(tibble)
library(lubridate)
library(stargazer)
library(dplyr)
library(ggplot2)
library(MASS)
library(leaps)
library(car)

# setwd("/home/e/R/airbnb.columbus/")
setwd("C:/Users/e/Documents/R/airbnb.columbus")

x <- read.csv("data/listings_full.csv")
glimpse(x)

x <- x %>% 
  dplyr::select(
    host_is_superhost, host_listings_count, host_total_listings_count,
                neighbourhood_cleansed, zipcode, property_type, room_type,
                accommodates, bathrooms, bedrooms, beds, bed_type, price,
                weekly_price, monthly_price, security_deposit, cleaning_fee,
                guests_included, extra_people, minimum_nights, maximum_nights,
                number_of_reviews, review_scores_rating, review_scores_accuracy,
                review_scores_cleanliness, review_scores_checkin,
                review_scores_communication, review_scores_location,
                review_scores_value, instant_bookable, is_business_travel_ready,
                cancellation_policy, require_guest_profile_picture,
                reviews_per_month) %>% 
  mutate(
    
  )
plot(x$host_listings_count)
hist(x$host_listings_count)
glimpse(x)

x <- x %>% 
  dplyr::select(host_listings_count, host_url, price,host_name) %>% 
  filter(host_listings_count > 100)
  

x <- x %>% 
  group_by(neighbourhood_cleansed) %>% 
  filter(n() > 20) %>% 
  mutate(location_is_Near.South = ifelse(neighbourhood_cleansed == "Near South",1,0)) %>% 
  mutate(location_is_Downtown = ifelse(neighbourhood_cleansed == "Downtown",1,0)) %>% 
  mutate(location_is_Near.East = ifelse(neighbourhood_cleansed == "Near East",1,0)) %>% 
  mutate(location_is_Clintonville = ifelse(neighbourhood_cleansed == "Clintonville",1,0)) %>% 
  mutate(location_is_West.Olentangy = ifelse(neighbourhood_cleansed == "West Olentangy",1,0)) %>% 
  mutate(location_is_Northland = ifelse(neighbourhood_cleansed == "Northland",1,0)) %>% 
  mutate(location_is_Rocky.Fork_Blacklick = ifelse(neighbourhood_cleansed == "Rocky Fork-Blacklick",1,0)) %>% 
  mutate(location_is_Northeast = ifelse(neighbourhood_cleansed == "Northeast",1,0)) %>% 
  mutate(location_is_Northwest = ifelse(neighbourhood_cleansed == "Northwest",1,0)) %>% 
  mutate(location_is_Hayden.Run = ifelse(neighbourhood_cleansed == "Hayden Run",1,0)) %>% 
  mutate(location_is_Hilltop = ifelse(neighbourhood_cleansed == "Hilltop",1,0)) %>% 
  mutate(location_is_Far.North = ifelse(neighbourhood_cleansed == "Far North",1,0)) %>% 
  mutate(Price = str_replace(price, "[$]","")) %>% 
  mutate(Price = as.numeric(price)) %>% 
  mutate(weekly_Price = str_replace(weekly_price, "[$]","")) %>% 
  mutate(weekly_Price = as.numeric(weekly_price)) %>% 
  mutate(monthly_Price = str_replace(monthly_price, "[$]","")) %>% 
  mutate(monthly_Price = as.numeric(monthly_price))

lm0 <- lm(Price~location_is_Near.South+
            location_is_Downtown+
            location_is_Near.East+
            location_is_Clintonville+
            location_is_West.Olentangy+
            location_is_Northland+
            location_is_Rocky.Fork_Blacklick+
            location_is_Northeast+
            location_is_Northwest+
            location_is_Hayden.Run+
            location_is_Far.North+
            location_is_Hilltop, 
          data=x.hoods)
summary(lm0)
anova(lm0)
par(mfrow=c(2,2))
plot(lm0) # seems fine, QQ shows over-dispersion

# would be nice to graph histograms or boxplots of the distribution of the price value
# based on the neighborhood, and then compar to the SSE and p values
# would expect large deviations in graph with higher SSE
# x.t <- x %>%
#   filter(neighbourhood_cleansed == c("Downtown","Northland","Clintonville")) %>% 
#   mutate(Price = str_replace(price, "[$]","")) %>% 
#   mutate(Price = as.numeric(price)) %>% 
#   select(neighbourhood_cleansed,Price)
# ggplot(x.t, aes(x=neighbourhood_cleansed, y=Price))+geom_boxplot()
# ggplot(x.t, aes(x=Price))+geom_histogram()

# Now we must select the best coefficients for linear model creation
# Not use stepAIC, stepwise apparently sort of useless

vif(lm0) # no multicollinearity detected, as expected
lm0.reg <- regsubsets(Price~location_is_Near.South+
                        location_is_Downtown+
                        location_is_Near.East+
                        location_is_Clintonville+
                        location_is_West.Olentangy+
                        location_is_Northland+
                        location_is_Rocky.Fork_Blacklick+
                        location_is_Northeast+
                        location_is_Northwest+
                        location_is_Hayden.Run+
                        location_is_Far.North+
                        location_is_Hilltop,
                      data=x.hoods)

lm0.reg.sum <- summary(lm0.reg)

# regsubsets plots show some different info
par(mfrow=c(2,2))

which.max(lm0.reg.sum$adjr2) # 6
plot(lm0.reg.sum$adjr2, xlab="# Variables",ylab="Adjusted R2",type="l")
points(6,lm0.reg.sum$adjr2[6],col="red",cex=2,pch=20)

which.min(lm0.reg.sum$rss)   # 8
plot(lm0.reg.sum$rss,   xlab="# Variables",ylab="RSS",type="l")
points(8,lm0.reg.sum$rss[8],col="red",cex=2,pch=20)

which.min(lm0.reg.sum$cp)    # 5
plot(lm0.reg.sum$cp,    xlab="# Variables",ylab="Cp",type="l")
points(5,lm0.reg.sum$cp[5],col="red",cex=2,pch=20)

which.min(lm0.reg.sum$bic)   # 1
plot(lm0.reg.sum$bic,   xlab="# Variables",ylab="BIC",type="l")
points(1,lm0.reg.sum$bic[1],col="red",cex=2,pch=20)

# may as well just take the mean of all the predictors
mean(c(1,5,8,6)) # 5

par(mfrow=c(1,1))
plot(lm0.reg) # BIC
plot(lm0.reg,scale="r2")
plot(lm0.reg,scale="adjr2")
plot(lm0.reg,scale="Cp")

# choosing the 5 most influential locations:
coef(lm0.reg,5)
# Base case, Downtown, Northland, Rocky Fork/Blacklick, Northeast, Hilltop

# lets try the updated model
lm0.1 <- lm(Price~
              location_is_Downtown+
              location_is_Northland+
              location_is_Rocky.Fork_Blacklick+
              location_is_Northeast+
              location_is_Hilltop, 
            data=x.hoods)
summary(lm0.1)
anova(lm0.1)
par(mfrow=c(2,2))
plot(lm0.1)

# Validation set approach







# try the same above for monthly/weekly prices?


x <- x %>% 
  # mutate(days.as.host = as.integer(today()-ymd(x$host_since))) %>% 
  # mutate(host_is_superhost = as.integer(ifelse(host_is_superhost == "t", 1,0))) %>% 
  mutate(location_is_Near.North_University = ifelse(neighbourhood_cleansed == "Near North/University",1,0)) %>%  
  mutate(location_is_Eastland_Brice = ifelse(neighbourhood_cleansed == "Eastland/Brice",1,0)) %>% 
  mutate(location_is_West.Scioto = ifelse(neighbourhood_cleansed == "West Scioto",1,0)) %>% 
  mutate(location_is_Far.West = ifelse(neighbourhood_cleansed == "Far West",1,0)) %>% 
  mutate(location_is_Franklinton = ifelse(neighbourhood_cleansed == "Franklinton",1,0)) %>% 
  mutate(location_is_Southeast = ifelse(neighbourhood_cleansed == "Southeast",1,0)) %>% 
  mutate(location_is_Far.Northwest = ifelse(neighbourhood_cleansed == "Far Northwest",1,0)) %>% 
  mutate(location_is_Far.East = ifelse(neighbourhood_cleansed == "Far East",1,0)) %>% 
  mutate(location_is_Westland = ifelse(neighbourhood_cleansed == "Westland",1,0)) %>% 
  mutate(location_is_Rickenbacker = ifelse(neighbourhood_cleansed == "Rickenbacker",1,0)) %>% 
  mutate(location_is_Far.South = ifelse(neighbourhood_cleansed == "Far South",1,0)) %>% 
  mutate(location_is_Greenlawn_Frank.Road = ifelse(neighbourhood_cleansed == "Greenlawn/Frank Road",1,0)) %>% 
  mutate(room_is_Entire.home = ifelse(room_type == "Entire home/apt",1,0)) %>% 
  mutate(room_is_Private.room = ifelse(room_type == "Private room",1,0)) %>% 
  mutate(room_is_Shared.room = ifelse(room_type == "Shared room",1,0)) %>% 
  mutate(security_Deposit = str_replace(security_deposit, "[$]","")) %>%
  mutate(security_Deposit = as.numeric(security_deposit)) %>%
  mutate(cleaning_Fee = str_replace(cleaning_fee, "[$]","")) %>%
  mutate(cleaning_Fee = as.numeric(cleaning_fee)) %>%
  mutate(extra_People = str_replace(extra_people, "[$]","")) %>%
  mutate(extra_People = as.numeric(extra_people)) %>% 
  mutate(is_instant_bookable = as.integer(ifelse(instant_bookable == "t", 1,0))) %>% 
  mutate(is_business_ready = as.integer(ifelse(is_business_travel_ready == "t", 1,0))) %>% 
  mutate(cancellation_is_flexible = as.integer(ifelse(cancellation_policy == "flexible",1,0))) %>% 
  mutate(cancellation_is_moderate = as.integer(ifelse(cancellation_policy == "moderate",1,0))) %>% 
  mutate(cancellation_is_strict_14 = as.integer(ifelse(cancellation_policy == "strict_14_with_grace_period",1,0))) %>% 
  mutate(cancellation_is_super_strict_30 = as.integer(ifelse(cancellation_policy == "super_strict_30",1,0))) %>% 
  mutate(cancellation_is_super_strict_60 = as.integer(ifelse(cancellation_policy == "super_strict_60",1,0)))
glimpse(x.1)  

x.2 <- x.1 %>% 
  select(days.as.host, host_is_superhost, host_total_listings_count,
         location_is_Near.North_University, location_is_Near.East, location_is_Clintonville, location_is_Near.South,
         location_is_West.Olentangy, location_is_North.Linden, location_is_Eastland_Brice, location_is_South.Linden,
         location_is_Rocky.Fork_Blacklick, location_is_Downtown, location_is_West.Scioto, location_is_Northeast,
         location_is_Hilltop, location_is_Far.West, location_is_Eastmoor_Walnut.Ridge, location_is_Southeast,
         location_is_Northland, location_is_Northwest, location_is_Far.Northwest, location_is_Far.East, location_is_Westland,
         location_is_Hayden.Run, location_is_Franklinton, location_is_Far.North, location_is_Rickenbacker, location_is_Far.South,
         location_is_Greenlawn_Frank.Road, room_is_Entire.home, room_is_Private.room, room_is_Shared.room,
         accommodates, bathrooms, bedrooms, beds, 
         Price, weekly_Price, monthly_Price, security_Deposit, cleaning_Fee,
         extra_People, 
         is_instant_bookable, is_business_ready, minimum_nights, maximum_nights, 
         number_of_reviews, review_scores_rating, review_scores_accuracy, 
         review_scores_cleanliness, review_scores_checkin, review_scores_communication, 
         review_scores_location, review_scores_value, cancellation_is_flexible, 
         cancellation_is_moderate, cancellation_is_strict_14, 
         cancellation_is_super_strict_30, cancellation_is_super_strict_60)
# glimpse(x.2)

### Capacity related to price?
x.3 <- x.2 %>% 
  filter(accommodates < 11)

lm0 <- lm(Price ~ accommodates, data=x.3)
summary(lm0) # 1 unit increase in capacity = $4 decrease in price
plot(lm0) # QQ plot shows under-dispersed data

ggplot(data=x.3, aes(x=accommodates, y=Price)) +
  # geom_point(alpha=.1) +
  geom_jitter(alpha=.1)+
  stat_smooth(method="lm", fullrange = TRUE)

### Price vs room type?
lm1 <- lm(Price ~ room_is_Entire.home + room_is_Private.room + room_is_Shared.room, data=x.2)
summary(lm1) 
plot(lm1)
ggplot(data=x.1, aes(x=room_type,y=Price)) + 
  # geom_point(alpha=0.1) +
  # geom_jitter(alpha=.3)+
  geom_boxplot()

### Price vs location?