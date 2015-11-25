# Preliminary stuff for handwriting classsification
setwd('./handwriting')
require(caret)
require(e1071)

# Import data and split train/test/validate
num_data <- read.csv('train.csv')
num_data$label <- as.factor(num_data$label)
train_crit <- createDataPartition(y=num_data$label, p=0.7, list = FALSE)
training <- num_data[train_crit,]
testing <- num_data[-train_crit,]
test_crit <- createDataPartition(y=testing$label, p=0.7, list=FALSE)
validation <- testing[-test_crit,]
testing <- training[test_crit,]


# See how k-means does
# Spoiler Alert - Out of Memory Error :(s

require(RWeka)
correct_pct <- data.frame(k = NULL, pctCorrect = NULL)
for (k in 1:20) {
    set.seed(2465)
    classifier <- IBk(label~., data = training,
                      control = Weka_control(K = k))
    eval_data <- evaluate_Weka_classifier(classifier, numFolds = 5)
    correct_pct[k,1] <- k
    correct_pct[k,2] <- eval_data[[2]][1]
}
colnames(correct_pct) <- c('k', 'pct_correct')
correct_pct

plot(correct_pct$k, correct_pct$pct_correct, type='l',
     ylab = "Percent Correct",
     xlab = 'k value',
     main = 'Predicted Accuracy of kNN for different k values')

# This might be less memory intensive

require(class)
k=1 # need to make 1-20 for loop etc
test_pred <- as.integer(as.character(knn(training, testing, 
                                         training$label, k)))
conf_matrix <- confusionMatrix(test_pred, training$label)
# pull out apropriate stat for comparison

# Let's try a random forest..

rf_model <- train(label ~ ., data=training, method='rf')
