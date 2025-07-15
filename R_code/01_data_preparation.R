#####################----------Merge_User_Data-----------#####################

install.packages("dplyr")
install.packages("readr")
install.packages("tibble")
install.packages("tidyverse")  # optional, includes dplyr + more

# Load necessary library
library(dplyr)
library(readr)
library(tibble)

file1 <- "~/Desktop/users.genuine_accounts.csv"
file2 <- "~/Desktop/users.social_spambots_1.csv"

keep_cols <- c(  # use keep_cols here to match your select()
  "id",
  "statuses_count",
  "followers_count",
  "friends_count",
  "favourites_count",
  "listed_count",  # ✅ typo fixed
  "url",
  "default_profile",
  "geo_enabled",
  "profile_use_background_image",
  "protected",
  "verified",
  "created_at",
  "timestamp"
)

#Load and Clean Datasets
genuine <- read_csv(file1) %>%
  select(any_of(keep_cols)) %>%
  mutate(label = 0)

fake1 <- read_csv(file2) %>%
  select(any_of(keep_cols)) %>%
  mutate(label = 1)


# Combine all into one dataframe
all_users <- bind_rows(genuine, fake1)

# Preview
head(all_users)

table(all_users$label)  # Confirm counts before writing

# Save to a clean CSV
write_csv(all_users, "~/Desktop/combined_users(italianpolotics)_clean.csv")

#####################----------Merge_Tweet_Data-----------#####################
library(dplyr)
library(readr)

# Define your timestamp fixer
fix_timestamp <- function(ts) {
  if (is.numeric(ts)) {
    return(as.POSIXct(ts, origin = "1970-01-01"))
  } else {
    return(ts)
  }
}

# File paths
file1 <- "~/Desktop/tweets.genuine_accounts.csv"
file2 <- "~/Desktop/tweets.social_spambots_1.csv"

# Columns to keep
keep_cols <- c(
  "id", "text", "source", "user_id",
  "retweet_count", "reply_count", "favorite_count",
  "num_hashtags", "num_urls", "num_mentions",
  "created_at", "timestamp"
)

# Read genuine tweets
genuine <- read_csv(file1) %>%
  select(any_of(keep_cols)) %>%
  mutate(
    id = as.character(id),
    label = 0,
    timestamp = fix_timestamp(timestamp)
  )

fake1 <- read_csv(file2) %>%
  select(any_of(keep_cols)) %>%
  mutate(
    id = as.character(id),
    label = 1,
    timestamp = fix_timestamp(timestamp)
  )


# Combine them
all_tweets <- bind_rows(genuine, fake1)

# Confirm
table(all_tweets$label)

# Save
write_csv(all_tweets, "~/Desktop/combined_tweets_clean_(italianpolitisc).csv")

###############----------Merge_Tweet_and_User_data-----------###################

library(dplyr)
library(readr)

# File path with quotes to handle special characters and spaces
tweets <- read_csv("~/Desktop/STAT 654 Project/combined_tweets_clean_(italianpolitisc).csv")

nrow(tweets)
head(tweets)

library(dplyr)

tweet_features <- tweets %>%
  group_by(user_id) %>%
  summarize(
    tweet_count = n(),
    avg_retweet_count = mean(retweet_count, na.rm = TRUE),
    avg_reply_count = mean(reply_count, na.rm = TRUE),
    avg_favorite_count = mean(favorite_count, na.rm = TRUE),
    avg_num_hashtags = mean(num_hashtags, na.rm = TRUE),
    avg_num_urls = mean(num_urls, na.rm = TRUE),
    avg_num_mentions = mean(num_mentions, na.rm = TRUE),
    avg_text_length = mean(nchar(text), na.rm = TRUE),
    percent_with_url = mean(num_urls > 0, na.rm = TRUE),
    percent_with_hashtags = mean(num_hashtags > 0, na.rm = TRUE),
    percent_with_mentions = mean(num_mentions > 0, na.rm = TRUE)
  ) %>%
  ungroup()

user_data <- read_csv("~/Desktop/STAT 654 Project/combined_users(italianpolotics)_clean.csv")

nrow(user_data)
head(user_data)

combined_data <- left_join(user_data, tweet_features, by = c("id" = "user_id"))

head(combined_data)
summary(combined_data)

combined_data_with_tweets <- combined_data %>%
  filter(!is.na(tweet_count))

nrow(combined_data_with_tweets)  # Should be ~2075
summary(combined_data_with_tweets$tweet_count)

#there is one NA is user_id making for the difference of 2075 users in tweet_features
# and 2074 in the combined data
setdiff(tweet_features$user_id, user_data$id)
write_csv(combined_data_with_tweets, "~/Desktop/STAT 654 Project/almost_final_model_data.csv")