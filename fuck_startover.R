setwd("C:/Users/e/Documents/R/airbnb.columbus")
x <- read.csv("data/listings_full.csv")

# convert/drop useless factors
x <- x %>% 
  mutate(days_as_host = as.integer(today()-ymd(x$host_since))) %>% 
  mutate(host_is_superhost = as.integer(ifelse(host_is_superhost == "t",1,0))) %>% 
  mutate(price = str_replace(price, "[$]","")) %>% 
  mutate(Price = as.integer(price)) %>%   
  mutate(security_deposit = str_replace(security_deposit, "[$]","")) %>% 
  mutate(security_Deposit = as.integer( security_deposit)) %>%   
  mutate(cleaning_fee = str_replace(cleaning_fee, "[$]","")) %>% 
  mutate(cleaning_Fee = as.integer( cleaning_fee)) %>%   
  mutate(extra_people = str_replace(extra_people, "[$]","")) %>% 
  mutate(extra_People = as.integer( extra_people)) %>%   
  mutate(instant_bookable = as.integer(ifelse(instant_bookable == "t",1,0)))

# Replace NA's with 0
x$extra_People[is.na(y$extra_People)] <- 0
x$cleaning_Fee[is.na(y$cleaning_Fee)] <- 0
x$security_Deposit[is.na(y$security_Deposit)] <- 0

# Extrapolate daily price to weekly/monthly when necessary
for(k in 1:length(x$monthly_Price)){
  if(is.na(x$weekly_Price[[k]])){
    x$weekly_Price[[k]] = x$Price[[k]]*7
  }  
  if(is.na(x$monthly_Price[[k]])){
    x$monthly_Price[[k]] = x$Price[[k]]*30
  }  
}

# Location analysis, what location has the highest prices?
lm.loc <- lm(Price~neighbourhood_cleansed,data=x)
summary(lm.loc)
