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

##################----------in-depth-exploration----------#######################
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
bot <- read.csv("C:/Users/Danie/Downloads/TAMU Spring 2025/STAT 654/Data_Files/final_model_ready_data.csv", stringsAsFactors = FALSE)
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