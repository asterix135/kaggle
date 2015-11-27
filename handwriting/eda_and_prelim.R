# Preliminary stuff for handwriting classsification
setwd('./handwriting')
require(caret)
require(e1071)

# Import data and split train/test/validate
num_data <- read.csv('train.csv')
num_data$label <- as.factor(num_data$label)
set.seed(5485846)
train_crit <- createDataPartition(y=num_data$label, p=0.7, list = FALSE)
training <- num_data[train_crit,]
testing <- num_data[-train_crit,]
test_crit <- createDataPartition(y=testing$label, p=0.7, list=FALSE)
validation <- testing[-test_crit,]
testing <- training[test_crit,]

# quick PCA
num_pca <- prcomp(num_data[,-1])

# See how k-means does
require(class)
k=1 
test_pred <- as.integer(as.character(knn(training, testing, 
                                         training$label, k)))
conf_matrix <- confusionMatrix(test_pred, testing$label)

# Model is perfect on test set with k=1!
valid_pred <- as.integer(as.character(knn(training, validation,
                                          training$label, k)))
cm2 <- confusionMatrix(valid_pred, validation$label)
# accuracy here is 96.8% Not as good :(
# Let's try a few other k values
k <- 3
valid_pred <- as.integer(as.character(knn(training, validation,
                                          training$label, k)))

# probably the best way to choose k, and get a good prediction is through
# k-fold validation
## This is going to be slow.  Like overnight running slow...
## Run directly from knn_analysis.R
set.seed(324987)
folds <- createFolds(num_data$label)
knn_perf <- data.frame()
for (k in seq(1,201,by=10)) {
    k_acc <- vector()
    for (f in folds) {
        train <- num_data[-f,]; test <- num_data[f,]
        preds <- knn(train, test, train$label, k)
        fold_acc <- confusionMatrix(preds, train$label)$overall['Accuracy']
        k_acc <- c(k_acc, fold_acc)
    knn_perf <- rbind(knn_perf, c(k, mean(k_acc)))
    }
}
colnames(knn_perf) <- c('k', 'Accuracy')
knn_perf

# and graphically...
plot(knn_perfk$k, knn_perf$Accuracy, type='l',
     ylab = "Percent Correct",
     xlab = 'k value',
     main = 'Predicted Accuracy of kNN for different k values')




# Let's try a random forest..

rf_model <- train(label ~ ., data=training, method='rf')
