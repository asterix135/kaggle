## First Attempt is just to see how well we can do with knn

require(caret)
require(e1071)
require(class)

num_data2 <- read.csv('train.csv')
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
kaggle_test <- kaggle_test[,-drop_cols]
num_data <- num_data[,-drop_cols]

# 2. Let's see how good a job knn (k=3) does with our split off test set

testing_preds <- knn(training, testing, training$label, 3)

# Use this to start creating a d/f for ensemble learning

test_results <- data.frame(truth=testing$label, knn=testing_preds)

# This gets us 98% accuracy, so let's try a first submission to kaggle with just
# kNN and see how it does

kaggle_preds <- knn(num_data, kaggle_test, num_data$label, 3)

## save to data frame
kaggle_results <- data.frame(knn=kaggle_preds)

# Submission
kaggle_preds <- data.frame(ImageId=1:nrow(kaggle_preds), label = kaggle_preds)
write.csv(kaggle_preds, 'kaggle_knn_preds.csv', row.names=FALSE)

# Results: 0.92414


## OK, let's try a random forest approach

rf_model<-train(label ~ ., data=training, method="rf",
                trControl = trainControl(method="cv", number=5),
                prox=TRUE, allowParallel=TRUE)
