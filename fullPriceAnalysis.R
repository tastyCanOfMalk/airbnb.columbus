if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tibble")) install.packages("tibble")
if (!require("lubridate")) install.packages("lubridate")
# if (!require("stargazer")) install.packages("stargazer")
# if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("MASS")) install.packages("MASS")
if (!require("leaps")) install.packages("leaps")
if (!require("car")) install.packages("car")
library(tidyverse)
library(tibble)
library(lubridate)
# library(stargazer)
# library(dplyr)
library(ggplot2)
# library(MASS)
library(leaps)
library(car)

setwd("/home/e/R/airbnb.columbus/")
# setwd("C:/Users/e/Documents/R/airbnb.columbus")

x <- read.csv("data/listings_full.csv")
glimpse(x)
summary(x)

# we have hosts_listings_count with a median of 2.00 and a max of 1308.0
  # some hosts have dramatically more listings than others
  # these are likely businesses and we'll filter them out
  # minimum listings of this group is 93
  # SoBe and SoBeNY, for example, have listings ranging in location from NY to IL
  plot(x$host_listings_count)
  hist(x$host_listings_count) 
  y <- x %>% 
    filter(host_listings_count > 25) %>% 
    dplyr::select(host_name,host_url,listing_url,host_listings_count)
  summary(y)

# accomodates, bathrooms, bedrooms, beds also show skewed medians and maximums,
# filtering for host_listings_count should adjust these
# high reviews per month probably will also be adjusted
  # since one bedroom counts as a listing, most airbnb users rent out a single room
  # there are less and less that list 2 or 3 or more rooms
  # we'll choose a cutoff of around 10 listings as this represents hosts
  # that use airbnb for passive income still
  y <- x %>% 
    filter(host_listings_count < 10)
  summary(y)
  hist(y$host_listings_count)
  
# some listings are maxed on the have_availability metrics, which means
# they have zero bookings, investigate and possibly filter out
  y <- x %>% 
    filter(availability_90 == 90) %>% 
    dplyr::select(host_name, listing_url, availability_60, availability_90, availability_365)
  summary(y)
  # seems that a few of these have max availability because they are priced poorly
  # or undesirable due to location or other factors
  # probably keep them in the dataset but remember to filter out when calculating price

# review scores are all very high, oddly.
  summary(x$review_scores_rating)
  y <- x %>%
    filter(review_scores_rating<50)
  # interestingly the lowest scores are from the company above
  hist(x$review_scores_rating)
  # ratings are definitely skewed


y <- x %>% 
  mutate(days_as_host = as.integer(today()-ymd(x$host_since))) %>% 
  mutate(host_is_superhost = as.integer(ifelse(host_is_superhost == "t",1,0))) %>% 
  mutate(is_location_exact = as.integer(ifelse(is_location_exact == "t",1,0))) %>% 
  mutate(Price = str_replace(price, "[$]","")) %>% 
  mutate(Price = as.integer(price)) %>%   
  mutate(weekly_Price = str_replace(weekly_price, "[$]","")) %>% 
  mutate(weekly_Price = as.integer( weekly_price)) %>%   
  mutate(monthly_Price = str_replace(monthly_price, "[$]","")) %>% 
  mutate(monthly_Price = as.integer( monthly_price)) %>%   
  mutate(security_Deposit = str_replace(security_deposit, "[$]","")) %>% 
  mutate(security_Deposit = as.integer( security_deposit)) %>%   
  mutate(cleaning_Fee = str_replace(cleaning_fee, "[$]","")) %>% 
  mutate(cleaning_Fee = as.integer( cleaning_fee)) %>%   
  mutate(extra_People = str_replace(extra_people, "[$]","")) %>% 
  mutate(extra_People = as.integer( extra_people)) %>%   
  mutate(has_availability = as.integer(ifelse(has_availability == "t",1,0))) %>% 
  mutate(instant_bookable = as.integer(ifelse(instant_bookable == "t",1,0))) %>% 
  mutate(is_business_travel_ready = as.integer(ifelse(is_business_travel_ready == "t",1,0))) %>% 
  mutate(loc.is.Near.North_University = as.integer(ifelse(neighbourhood_cleansed == "Near North/University",1,0))) %>%  
  mutate(loc.is.Near.East = as.integer(ifelse(neighbourhood_cleansed == "Near East",1,0))) %>% 
  mutate(loc.is.Clintonville = as.integer(ifelse(neighbourhood_cleansed == "Clintonville",1,0))) %>% 
  mutate(loc.is.Near.South = as.integer(ifelse(neighbourhood_cleansed == "Near South",1,0))) %>% 
  mutate(loc.is.West.Olentangy = as.integer(ifelse(neighbourhood_cleansed == "West Olentangy",1,0))) %>% 
  mutate(loc.is.North.Linden = as.integer(ifelse(neighbourhood_cleansed == "North Linden",1,0))) %>% 
  mutate(loc.is.Eastland_Brice = as.integer(ifelse(neighbourhood_cleansed == "Eastland/Brice",1,0))) %>% 
  mutate(loc.is.South.Linden = as.integer(ifelse(neighbourhood_cleansed == "South Linden",1,0))) %>% 
  mutate(loc.is.Rocky.Fork_Blacklick = as.integer(ifelse(neighbourhood_cleansed == "Rocky Fork-Blacklick",1,0))) %>% 
  mutate(loc.is.Downtown = as.integer(ifelse(neighbourhood_cleansed == "Downtown",1,0))) %>% 
  mutate(loc.is.West.Scioto = as.integer(ifelse(neighbourhood_cleansed == "West Scioto",1,0))) %>% 
  mutate(loc.is.Northeast = as.integer(ifelse(neighbourhood_cleansed == "Northeast",1,0))) %>% 
  mutate(loc.is.Hilltop = as.integer(ifelse(neighbourhood_cleansed == "Hilltop",1,0))) %>% 
  mutate(loc.is.Far.West = as.integer(ifelse(neighbourhood_cleansed == "Far West",1,0))) %>% 
  mutate(loc.is.Eastmoor_Walnut.Ridge = as.integer(ifelse(neighbourhood_cleansed == "Eastmoor/Walnut Ridge",1,0))) %>% 
  mutate(loc.is.Southeast = as.integer(ifelse(neighbourhood_cleansed == "Southeast",1,0))) %>% 
  mutate(loc.is.Northland = as.integer(ifelse(neighbourhood_cleansed == "Northland",1,0))) %>% 
  mutate(loc.is.Northwest = as.integer(ifelse(neighbourhood_cleansed == "Northwest",1,0))) %>% 
  mutate(loc.is.Far.Northwest = as.integer(ifelse(neighbourhood_cleansed == "Far Northwest",1,0))) %>% 
  mutate(loc.is.Far.East = as.integer(ifelse(neighbourhood_cleansed == "Far East",1,0))) %>% 
  mutate(loc.is.Westland = as.integer(ifelse(neighbourhood_cleansed == "Westland",1,0))) %>% 
  mutate(loc.is.Hayden.Run = as.integer(ifelse(neighbourhood_cleansed == "Hayden Run",1,0))) %>% 
  mutate(loc.is.Franklinton = as.integer(ifelse(neighbourhood_cleansed == "Franklinton",1,0))) %>% 
  mutate(loc.is.Far.North = as.integer(ifelse(neighbourhood_cleansed == "Far North",1,0))) %>% 
  mutate(loc.is.Rickenbacker = as.integer(ifelse(neighbourhood_cleansed == "Rickenbacker",1,0))) %>% 
  mutate(loc.is.Far.South = as.integer(ifelse(neighbourhood_cleansed == "Far South",1,0))) %>% 
  mutate(loc.is.Greenlawn_Frank.Road = as.integer(ifelse(neighbourhood_cleansed == "Greenlawn/Frank Road",1,0))) %>% 
  mutate(room.is.Entire.home = as.integer(ifelse(room_type == "Entire home/apt",1,0))) %>% 
  mutate(room.is.Private.room = as.integer(ifelse(room_type == "Private room",1,0))) %>% 
  mutate(room.is.Shared.room = as.integer(ifelse(room_type == "Shared room",1,0))) %>% 
  mutate(cancellation.is.flexible = as.integer(ifelse(cancellation_policy == "flexible",1,0))) %>% 
  mutate(cancellation.is.moderate = as.integer(ifelse(cancellation_policy == "moderate",1,0))) %>% 
  mutate(cancellation.is.strict_14 = as.integer(ifelse(cancellation_policy == "strict_14_with_grace_period",1,0))) %>% 
  mutate(cancellation.is.super_strict_30 = as.integer(ifelse(cancellation_policy == "super_strict_30",1,0))) %>% 
  mutate(cancellation.is.super_strict_60 = as.integer(ifelse(cancellation_policy == "super_strict_60",1,0))) %>% 
  select(
    listing_url, host_url, host_name, days_as_host, host_is_superhost, 
    host_listings_count, latitude, longitude, is_location_exact, zipcode, 
    loc.is.Near.North_University,
    loc.is.Near.East,
    loc.is.Clintonville ,
    loc.is.Near.South,
    loc.is.West.Olentangy,
    loc.is.North.Linden,
    loc.is.Eastland_Brice,
    loc.is.South.Linden,
    loc.is.Rocky.Fork_Blacklick,
    loc.is.Downtown,
    loc.is.West.Scioto,
    loc.is.Northeast,
    loc.is.Hilltop ,
    loc.is.Far.West,
    loc.is.Eastmoor_Walnut.Ridge,
    loc.is.Southeast,
    loc.is.Northland,
    loc.is.Northwest,
    loc.is.Far.Northwest,
    loc.is.Far.East,
    loc.is.Westland,
    loc.is.Hayden.Run,
    loc.is.Franklinton,
    loc.is.Far.North,
    loc.is.Rickenbacker,
    loc.is.Far.South,
    loc.is.Greenlawn_Frank.Road,
    room.is.Shared.room, room.is.Private.room, room.is.Entire.home,
    accommodates, bathrooms, bedrooms, beds, 
    Price, weekly_Price, monthly_Price, security_Deposit, cleaning_Fee,
    guests_included, extra_People, minimum_nights, maximum_nights,
    has_availability, availability_30, availability_60, 
    availability_90, availability_365, 
    number_of_reviews, review_scores_rating, review_scores_accuracy,
    review_scores_cleanliness, review_scores_checkin,
    review_scores_communication, review_scores_location,
    review_scores_value, instant_bookable, is_business_travel_ready,
    cancellation.is.flexible,
    cancellation.is.moderate,
    cancellation.is.strict_14,
    cancellation.is.super_strict_30,
    cancellation.is.super_strict_60, 
    reviews_per_month
    ) %>% 
summary(y)

# first let's do a lm with all variables, predicting price
lm.all <- lm(Price ~ days_as_host+ host_is_superhost+ host_listings_count+
  loc.is.Near.North_University+ loc.is.Near.East+ loc.is.Clintonville + loc.is.Near.South+
  loc.is.West.Olentangy+ loc.is.North.Linden+ loc.is.Eastland_Brice+ loc.is.South.Linden+
  loc.is.Rocky.Fork_Blacklick+ loc.is.Downtown+ loc.is.West.Scioto+ loc.is.Northeast+
  loc.is.Hilltop+ loc.is.Far.West+ loc.is.Eastmoor_Walnut.Ridge+ loc.is.Southeast+
  loc.is.Northland+ loc.is.Northwest+ loc.is.Far.Northwest+ loc.is.Far.East+
  loc.is.Westland+ loc.is.Hayden.Run+ loc.is.Franklinton+ loc.is.Far.North+
  loc.is.Rickenbacker+ loc.is.Far.South+ loc.is.Greenlawn_Frank.Road+
  room.is.Shared.room+ room.is.Private.room+ room.is.Entire.home+
  accommodates+ bathrooms+ bedrooms+ beds+ 
  weekly_Price+ monthly_Price+ security_Deposit+ cleaning_Fee+
  guests_included+ extra_People+ minimum_nights+ maximum_nights+
  has_availability+ availability_30+ availability_60+ 
  availability_90+ availability_365+ 
  number_of_reviews+ review_scores_rating+ review_scores_accuracy+
  review_scores_cleanliness+ review_scores_checkin+
  review_scores_communication+ review_scores_location+
  review_scores_value+ instant_bookable+ is_business_travel_ready+
  cancellation.is.flexible+
  cancellation.is.moderate+
  cancellation.is.strict_14+
  cancellation.is.super_strict_30+
  cancellation.is.super_strict_60+ 
  reviews_per_month, data=y)
lm.allsummary(lm.all)
anova(lm.all)
vif(lm.all)
