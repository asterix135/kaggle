## First Attempt is just to see how well we can do with knn

require(caret)
require(e1071)
require(class)

num_data <- read.csv('train.csv')
kaggle_test <- read.csv('test.csv')
num_data$label <- as.factor(num_data$label)
set.seed(2943587)
train_crit <- createDataPartition(y=num_data$label, p=0.7, list = FALSE)
training <- num_data[train_crit,]
testing <- num_data[-train_crit,]
test_crit <- createDataPartition(y=testing$label, p=0.7, list=FALSE)
validation <- testing[-test_crit,]
testing <- training[test_crit,]


#pre-processing

# 1. Get rid of zero-variance variables
drop_cols <- nearZeroVar(training)
training <- training[,-drop_cols]
testing <- testing[,-drop_cols]
validation <- validation[,-drop_cols]
num_data <- num_data[,-drop_cols]

# kaggle test data has all columns shifted one to the left
drop_kaggle_cols <- drop_cols - 1
kaggle_test <- kaggle_test[,-drop_kaggle_cols]


# 2. Let's see how good a job knn (k=3) does with our split off test set

testing_preds <- knn(training, testing, training$label, 3)

# Use this to start creating a d/f for ensemble learning

test_results <- data.frame(truth=testing$label, knn=testing_preds)
confusionMatrix(testing_preds, testing$label)

# This gets us 98% accuracy, so let's try a first submission to kaggle with just
# kNN and see how it does

kaggle_preds <- knn(num_data[,2:252], kaggle_test, num_data$label, 3)

## save to data frame
kaggle_results <- data.frame(knn=kaggle_preds)

# Submission
kaggle_preds <- data.frame(ImageId=1:length(kaggle_preds), label = kaggle_preds)
write.csv(kaggle_preds, 'kaggle_knn_preds.csv', row.names=FALSE)

# Results: 0.96900


## OK, let's try a random forest approach

rf_model<-train(label ~ ., data=training, method="rf",
                trControl = trainControl(method="cv", number=5),
                prox=TRUE, allowParallel=TRUE)
save(rf_model, file = 'rf_model.RData')
# to load model:
# load('rf_model.RData')

test_rf_pred <- predict(rf_model, testing)
test_results <- cbind(test_results, test_rf_pred)
confusionMatrix(test_rf_pred, testing$label)

# 100% accuracy on test set!
# let's see how we do with kaggle's test set

kaggle_rf_preds <- predict(rf_model, kaggle_test)

kaggle_results <- cbind(kaggle_results, rf = kaggle_rf_preds)

kaggle_rf_preds <- data.frame(ImageId=1:length(kaggle_rf_preds), 
                              label = kaggle_rf_preds)
write.csv(kaggle_rf_preds, 'kaggle_rf_preds.csv', row.names=FALSE)

# 0.95471 - not an improvement :(
# should try playing with these...

## gbm model?

gbm_model<-train(label ~ ., data=training, method="gbm",
                 trControl = trainControl(method="cv", number=5),
                 prox=TRUE, allowParallel=TRUE)

test_gbm_preds <- predict(gbm_model, testing)
kaggle_gbm_preds <- predict(gbm_model, kaggle_test)