################----------Final_Feature_Creation-----------#####################

library(dplyr)
library(readr)
library(lubridate)

# Step 1: Load the combined dataset (tweets + user metadata)
data <- read_csv("~/Desktop/STAT 654 Project/almost_final_model_data.csv")

# Create url_in_user binary feature
data <- data %>%
  mutate(url_in_user = ifelse(is.na(url), 0, 1))

# Preview
table(data$url_in_user)
head(data %>% select(url, url_in_user))

#handle N/A in the default_profile and assume = 0
data <- data %>%
  mutate(default_profile = ifelse(is.na(default_profile), 0, default_profile))

# Quick check
table(data$default_profile)

#same thing with geo enabled
data <- data %>%
  mutate(geo_enabled = ifelse(is.na(geo_enabled), 0, geo_enabled))

table(data$geo_enabled)

#same with also profile background image
data <- data %>%
  mutate(profile_use_background_image = ifelse(is.na(profile_use_background_image), 0, profile_use_background_image))

table(data$profile_use_background_image)

#-----------delete verified and protected ? not much data here, useless

#account age in refrence of now 
data <- data %>%
  mutate(
    timestamp = as.Date(timestamp),             # strip time portion
    account_age_days = as.numeric(Sys.Date() - timestamp)  # age as of today
  )

summary(data$account_age_days)

# follower-to-friend ratio
data <- data %>%
  mutate(follower_friend_ratio = followers_count / (friends_count + 1))

#creditentials
data <- data %>%
  mutate(credibility_score = listed_count / (followers_count + 1))

#profile custimization 
data <- data %>%
  mutate(customization_score = (1 - default_profile + profile_use_background_image) / 2)


#rescaling for logistic regression
#scaled_data <- data %>%
#mutate(across(c(
#followers_count, friends_count, favourites_count,
#follower_friend_ratio, credibility_score,
#customization_score, account_age_days
# ), scale))


write_csv(data, "~/Desktop/STAT 654 Project/final_model_ready_data.csv")