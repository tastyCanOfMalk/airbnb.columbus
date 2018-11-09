## Load packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tibble")) install.packages("tibble")
if (!require("lubridate")) install.packages("lubridate")
if (!require("stargazer")) install.packages("stargazer")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("MASS")) install.packages("MASS")
library(tidyverse)
library(tibble)
library(lubridate)
library(stargazer)
library(dplyr)
library(ggplot2)
library(MASS)

setwd("/home/e/R/airbnb.columbus/")
# setwd("C:/Users/e/Documents/R/airbnb.columbus")

x <- read.csv("data/listings_full.csv")

x <- x %>% 
  mutate(days.as.host = as.integer(today()-ymd(x$host_since))) %>% 
  mutate(host_is_superhost = as.integer(ifelse(host_is_superhost == "t", 1,0))) %>% 
  mutate(location_is_Near.North_University = ifelse(neighbourhood_cleansed == "Near North/University",1,0)) %>%  
  mutate(location_is_Near.East = ifelse(neighbourhood_cleansed == "Near East",1,0)) %>% 
  mutate(location_is_Clintonville = ifelse(neighbourhood_cleansed == "Clintonville",1,0)) %>% 
  mutate(location_is_Near.South = ifelse(neighbourhood_cleansed == "Near South",1,0)) %>% 
  mutate(location_is_West.Olentangy = ifelse(neighbourhood_cleansed == "West Olentangy",1,0)) %>% 
  mutate(location_is_North.Linden = ifelse(neighbourhood_cleansed == "North Linden",1,0)) %>% 
  mutate(location_is_Eastland_Brice = ifelse(neighbourhood_cleansed == "Eastland/Brice",1,0)) %>% 
  mutate(location_is_South.Linden = ifelse(neighbourhood_cleansed == "South Linden",1,0)) %>% 
  mutate(location_is_Rocky.Fork_Blacklick = ifelse(neighbourhood_cleansed == "Rocky Fork-Blacklick",1,0)) %>% 
  mutate(location_is_Downtown = ifelse(neighbourhood_cleansed == "Downtown",1,0)) %>% 
  mutate(location_is_West.Scioto = ifelse(neighbourhood_cleansed == "West Scioto",1,0)) %>% 
  mutate(location_is_Northeast = ifelse(neighbourhood_cleansed == "Northeast",1,0)) %>% 
  mutate(location_is_Hilltop = ifelse(neighbourhood_cleansed == "Hilltop",1,0)) %>% 
  mutate(location_is_Far.West = ifelse(neighbourhood_cleansed == "Far West",1,0)) %>% 
  mutate(location_is_Eastmoor_Walnut.Ridge = ifelse(neighbourhood_cleansed == "Eastmoor/Walnut Ridge",1,0)) %>% 
  mutate(location_is_Southeast = ifelse(neighbourhood_cleansed == "Southeast",1,0)) %>% 
  mutate(location_is_Northland = ifelse(neighbourhood_cleansed == "Northland",1,0)) %>% 
  mutate(location_is_Northwest = ifelse(neighbourhood_cleansed == "Northwest",1,0)) %>% 
  mutate(location_is_Far.Northwest = ifelse(neighbourhood_cleansed == "Far Northwest",1,0)) %>% 
  mutate(location_is_Far.East = ifelse(neighbourhood_cleansed == "Far East",1,0)) %>% 
  mutate(location_is_Westland = ifelse(neighbourhood_cleansed == "Westland",1,0)) %>% 
  mutate(location_is_Hayden.Run = ifelse(neighbourhood_cleansed == "Hayden Run",1,0)) %>% 
  mutate(location_is_Franklinton = ifelse(neighbourhood_cleansed == "Franklinton",1,0)) %>% 
  mutate(location_is_Far.North = ifelse(neighbourhood_cleansed == "Far North",1,0)) %>% 
  mutate(location_is_Rickenbacker = ifelse(neighbourhood_cleansed == "Rickenbacker",1,0)) %>% 
  mutate(location_is_Far.South = ifelse(neighbourhood_cleansed == "Far South",1,0)) %>% 
  mutate(location_is_Greenlawn_Frank.Road = ifelse(neighbourhood_cleansed == "Greenlawn/Frank Road",1,0)) %>% 
  mutate(room_is_Entire.home = ifelse(room_type == "Entire home/apt",1,0)) %>% 
  mutate(room_is_Private.room = ifelse(room_type == "Private room",1,0)) %>% 
  mutate(room_is_Shared.room = ifelse(room_type == "Shared room",1,0)) %>% 
  mutate(Price = str_replace(price, "[$]","")) %>% 
  mutate(Price = as.numeric(price)) %>% 
  mutate(weekly_Price = str_replace(weekly_price, "[$]","")) %>% 
  mutate(weekly_Price = as.numeric(weekly_price)) %>% 
  mutate(monthly_Price = str_replace(monthly_price, "[$]","")) %>% 
  mutate(monthly_Price = as.numeric(monthly_price)) %>% 
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

# Price vs location
lm0 <- lm(review_scores_cleanliness ~ location_is_Near.North_University + location_is_Near.East + location_is_Clintonville + location_is_Near.South +
                    location_is_West.Olentangy + location_is_North.Linden + location_is_Eastland_Brice + location_is_South.Linden +
                    location_is_Rocky.Fork_Blacklick + location_is_Downtown + location_is_West.Scioto + location_is_Northeast +
                    location_is_Hilltop + location_is_Far.West + location_is_Eastmoor_Walnut.Ridge + location_is_Southeast +
                    location_is_Northland + location_is_Northwest + location_is_Far.Northwest + location_is_Far.East + location_is_Westland +
                    location_is_Hayden.Run + location_is_Franklinton + location_is_Far.North + location_is_Rickenbacker + location_is_Far.South +
                    location_is_Greenlawn_Frank.Road, data=x)
summary(lm0)
lm0.step <- stepAIC(lm0, direction="both")
summary(lm0.step)

# Reviews correlation with price?
lm1 <- lm(Price ~ review_scores_rating + review_scores_accuracy + review_scores_cleanliness + 
          review_scores_checkin + review_scores_communication + review_scores_location + review_scores_value,
          data=x)
summary(lm1)
lm1.step <- stepAIC(lm1, direction="both")
summary(lm1.step)

# Interactions between reviews and locations?

# reviews vs stuff
lm2 <- lm(review_scores_rating ~ days.as.host + Price + accommodates + bathrooms + bedrooms + beds, data=x)
summary(lm2)
lm2.step <- stepAIC(lm2)
summary(lm2.step)

