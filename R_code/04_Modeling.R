##################----------Modeling----------#######################

#make sure to have required packages installed

bot <- read.csv("C:/Users/Diogo/Downloads/TAMU Spring 2025/STAT 654/Data_Files/final_model_ready_data.csv", stringsAsFactors = FALSE)
botPredictors <- subset(bot, select = -c(id, label,url, protected, verified,
                                         created_at, timestamp))

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