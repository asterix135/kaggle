# Make sure working directory is set to file with data files
#setwd('./handwriting')
require(caret)
require(e1071)
require

num_data <- read.csv('train.csv')
num_data$label <- as.factor(num_data$label)

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

