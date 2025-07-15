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

##################----------Data_exploration-----------#######################

library(dplyr)
library(readr)

# Load the cleaned and scaled dataset
data <- read_csv("~/Desktop/STAT 654 Project/final_model_ready_data.csv")

# Summary statistics by label (0 = human, 1 = bot)
summary_by_class <- data %>%
  group_by(label) %>%
  summarize(
    n = n(),
    avg_followers = mean(followers_count, na.rm = TRUE),
    avg_friends = mean(friends_count, na.rm = TRUE),
    avg_follower_friend_ratio = mean(follower_friend_ratio, na.rm = TRUE),
    avg_credibility = mean(credibility_score, na.rm = TRUE),
    avg_customization = mean(customization_score, na.rm = TRUE),
    avg_account_age_days = mean(account_age_days, na.rm = TRUE),
    avg_favourites_per_day = mean(favourites_count / (account_age_days + 1), na.rm = TRUE),
    avg_has_url = mean(url_in_user, na.rm = TRUE),
    avg_geo_enabled = mean(geo_enabled, na.rm = TRUE)
  )

print(summary_by_class)
View(summary_by_class)

##################----------Beginning Model Testing----------#######################
# Code Sourced from 
#https://stats.stackexchange.com/questions/41443/how-to-actually-plot-a-sample-tree-from-randomforestgettree
# Remove older version of packages if necessary
options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)
library(devtools)
#
if(!('reprtree' %in% installed.packages())){
  install_github('munoztd0/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))
bot <- read.csv("C:/Users/Diogo/Downloads/TAMU Spring 2025/STAT 654/Data_Files/final_model_ready_data.csv", stringsAsFactors = FALSE)
summary(bot$avg_reply_count)
#
barplot(table(bot$label), main = "", xlab = "")
title(main = paste("Distribution of Bot label"),
      xlab = "label")
legend("topright", legend = c("Genuine User = 0", "Bot = 1"))
#
c(length(bot$label[which(bot$label == "0")]), length(bot$label[which(bot$label == "1")]))
#
# Visualizing numeric and categorical predictors
botPredictors <- subset(bot, select = -c(id, label,url, protected, verified,
                                         created_at, timestamp))
par(mfrow= c(2,2))
for(i in 1:(ncol(botPredictors)-1)){
  if(is.numeric(botPredictors[1,i])){
    hist(botPredictors[,i], main = "", xlab = "", breaks = 100)
    title(main = paste("Distribution of",colnames(botPredictors)[i]),
          xlab = colnames(botPredictors)[i])
  }
  else{
    barplot(table(botPredictors[,i]), main = "", xlab = "")
    title(main = paste("Distribution of ",colnames(botPredictors)[i]),
          xlab = colnames(botPredictors)[i])
  }
}
#
# Boxplots bot comparisons
# for(i in 2:(ncol(botPredictors)-1)){
# name <- colnames(botPredictors)[i]
# ggplot(data = bot, mapping = aes(x = label, y = name))+
# geom_boxplot()
#
# }
par(mfrow= c(1,1))
#
# Boxplot of log follower count of Humans vs Bots
ggplot(data = bot, mapping = aes(x = as.factor(label), y = log(followers_count)))+
  geom_boxplot(outliers = TRUE) + labs(title = "Boxplot of Log followers_count")+
  scale_x_discrete(labels= c("Human","Bot"))
#
# Boxplot of Account Age of Humans vs Bots
ggplot(data = bot, mapping = aes(x = as.factor(label), y = account_age_days))+
  geom_boxplot(outliers = TRUE) + labs(title = "Boxplot of account_age_days")+
  scale_x_discrete(labels= c("Human","Bot"))
#
box_idx <- which(bot$label == "1")
summary(bot$account_age_days[box_idx])
#
# Boxplot of Tweet Count of Humans vs Bots
ggplot(data = bot, mapping = aes(x = as.factor(label), y = tweet_count))+
  geom_boxplot(outliers = TRUE) + labs(title = "Boxplot of tweet_count")+
  scale_x_discrete(labels= c("Human","Bot"))
#
box_idx <- which(bot$label == "1")
summary(bot$tweet_count[box_idx])
#
summary(bot$tweet_count[-box_idx])
#
# Boxplots of Predictors comparing Human vs Bots
for(i in 1:(ncol(botPredictors)-1)){
  if(is.numeric(botPredictors[1,i])){
    y <- colnames(botPredictors)[i]
    print(ggplot(data = bot, mapping = aes(x = as.factor(label), y = .data[[y]]))+
            geom_boxplot(outliers = TRUE) + labs(title = paste("Box plot of",colnames(botPredictors)[i]))+
            scale_x_discrete(labels= c("Human","Bot"))
    )
  }
}
#
# Correlation of predictors
corrplot(cor(botPredictors), method="circle",
         tl.col="black", tl.cex=0.7, tl.srt=70,
         ,number.cex=0.35, addCoef.col = "black")
#
# Mean, Median, and Standard Deviation of Predictors
idBot <- box_idx
options("scipen"=100, "digits"=4)
colMeanBot <- apply(botPredictors[idBot,],2,mean)
colMeanHuman <- apply(botPredictors[-idBot,],2,mean)

colMeanBot
#
colMeanHuman
#
colMedianBot <- apply(botPredictors[idBot,],2,median)
colMedianHuman <- apply(botPredictors[-idBot,],2,median)
colMedianBot
#
colMedianHuman
colSdBot <- apply(botPredictors[idBot,],2,sd)
colSdHuman <- apply(botPredictors[-idBot,],2,sd)
colSdBot
#
colSdHuman
#
# Splitting data into training and test set
set.seed(1)
# botData with predictors removed based on EDA
botData <- data.frame(subset(botPredictors,
                             select =-c(avg_reply_count,
                                        percent_with_url,
                                        percent_with_mentions,percent_with_hashtags,
                                        default_profile,
                                        listed_count
                             )),
                      label = bot$label)
train <- sample(1:nrow(botData), nrow(botData)*0.8)
#
set.seed(1)

# 5-fold CV Lasso Logistic Regression
xTrain <- as.matrix(botData[train,-ncol(botData)])
xTest <- as.matrix(botData[-train,-ncol(botData)])

cv.out <- cv.glmnet(xTrain,
                    
                    as.double(botData[train,c("label")]),
                    alpha = 1,
                    nfolds = 5,
                    family = "binomial")

bestlam <- cv.out$lambda.min
# Model coefficients
lasso.coef <- predict(cv.out, type = "coefficients", s= bestlam,
                      xTrain)
lasso.coef
#
# Need to fix issue with predictions not being between 0 or 1
lasso.pred <- predict(cv.out, type = "response", s= bestlam,
                      newx = xTest)
lasso.class <- predict(cv.out, type = "class", s= bestlam,
                       newx = xTest)

# Confusion Matrix
confusionMatrix(data = as.factor(ifelse(lasso.class == "1", 1, 0)), reference = as.factor(botData[-train,c("label")]))
#
pred <- prediction(lasso.pred, botData[-train, c("label")])
perf <- performance(pred,"tpr","fpr")
plot(perf)
title(main = "ROC curve")
#
lda.fit <- lda(label~., data = botData, subset = train)
lda.class <- predict(lda.fit, botData[-train,])
confusionMatrix(data = as.factor(lda.class$class), reference = as.factor(botData[-train,c("label")]))
#
# Visualization of ROC curve based on https://stackoverflow.com/questions/41533811/roc-curve-in-linear-discriminant-analysis-with-r
pred <- prediction(lda.class$posterior[,2], botData[-train, c("label")])
perf <- performance(pred,"tpr","fpr")
plot(perf)
title(main = "ROC curve")
#
set.seed(1)
# 5-fold CV decision tree with small cp allowing for more complex
# decision tree
fit <- rpart(label~., data = botData, subset = train, method = "class", control = rpart.control(cp = 0.005, xval = 5))
rpart.plot(fit, main = "Decision Tree for Bot Detection Dataset")
#
tree.pred <- predict(fit, botData[-train,])
tree.pred1 <- tree.pred[,c("1")]

confusionMatrix(data = as.factor(ifelse(tree.pred1 >= 0.5, 1, 0)), reference = as.factor(botData[-train,c("label")]))
#
fit.rf <- randomForest(as.factor(label)~., data = botData,
                       
                       subset = train,
                       importance = TRUE)

fit.bagging <- randomForest(as.factor(label)~., data = botData,
                            
                            subset = train,
                            importance = TRUE)

# Need to consider what this brings to the table
# plot(fit.rf, log="y")
# varImpPlot(fit.rf)
rf.pred <- predict(fit.rf, botData[-train,], type = "prob")
rf.pred1 <- rf.pred[,c("1")]
# Confusion Matrix
confusionMatrix(data = as.factor(ifelse(rf.pred1 >= 0.5, 1, 0)), reference = as.factor(botData[-train,c("label")]))
#
# ROC curve
pred <- prediction(rf.pred1, botData[-train, c("label")])
perf <- performance(pred,"tpr","fpr")
plot(perf)
title(main = "ROC curve")
#
# Need to overwrite functions internally in order to
# create a tree that is readable
trace(reprtree:::labelBG, edit = TRUE)
#
trace(reprtree:::labelYN, edit = TRUE)
#
trace(reprtree:::plot.reprtree, edit = TRUE)
#
reprtree :: plot.getTree(fit.rf)
#
set.seed(1)
k_grid <- expand.grid(k = 1:100)
botData <- data.frame(subset(botPredictors, select =-c(avg_reply_count)),
                      
                      label = as.factor(bot$label))
botData <- data.frame(subset(botPredictors,
                             
                             select =-c(avg_reply_count,
                                        percent_with_url,
                                        percent_with_mentions,
                                        percent_with_hashtags,
                                        default_profile,
                                        listed_count
                             )),
                      label = as.factor(bot$label))
botDataSc <- botData[, -ncol(botData)]
# Need to scale data due to data being dispersed poorly
botDataSc <- scale(botDataSc)
## Need to check if this is reasonable for binary variables
# Need to determine appropriate number of folds
trainControl <- trainControl(method = "cv", number = 5)
# Ensuring all variables are in a dataframe
botDataSc <- data.frame(botDataSc,label =botData$label)
knn_model <- train(label~.,
                   
                   data=botDataSc[train,],
                   method = "knn",
                   trControl = trainControl,
                   tuneGrid = k_grid)
# Prints and plots model results
print(knn_model)
#
plot(knn_model)
#
tb <-table(knn.pred, botDataSc[-train, c("label")])
tb
#
# Accuracy
(tb[1,1]+tb[2,2])/(tb[1,1]+tb[1,2]+(tb[2,1]+tb[2,2]))
#
# Sensitivity
(tb[2,2])/(tb[2,2]+(tb[2,1]))
#
# Specificity
(tb[1,1])/(tb[1,1]+(tb[1,2]))



