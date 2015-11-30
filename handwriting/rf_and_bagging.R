#####
# Building some alternate models
#####

# Make sure working directory is where the data is saved
if (getwd() != "/Users/christophergraham/Documents/Code/kaggle/handwriting") {
    setwd("/Users/christophergraham/Documents/Code/kaggle/handwriting")
}

require(caret)
require(e1071)

num_data <- read.csv('train.csv')
num_data$label <- as.factor(num_data$label)
set.seed(2943587)
train_crit <- createDataPartition(y=num_data$label, p=0.7, list = FALSE)
training <- num_data[train_crit,]
testing <- num_data[-train_crit,]
test_crit <- createDataPartition(y=testing$label, p=0.7, list=FALSE)
validation <- testing[-test_crit,]
testing <- training[test_crit,]

#PCA pre-processing

# 1. Get rid of zero-variance variables
drop_cols <- nearZeroVar(training)
training <- training[,-drop_cols]
testing <- testing[,-drop_cols]
validation <- validation[,-drop_cols]

# Build Simple RF Model
rf_model <- train(label ~ ., data=training, method='rf')


# PCA
preObj <- preProcess(training[,-1], method=c('pca'), thresh=0.99)
# where do we have all 0s or zero variance?
train_pca <- cbind(label = training$label, num_pca$x[,485])


test_pca <- predict(num_pca, testing)
test_pca <- data.frame(cbind(label = testing$label, test_pca))

test_rf_pred <- predict(rf_model, test_pca)

# Better: use k-fold = ask prof about this

train_control <- trainControl(method = 'cv', number =10)
rf_model2 <- train(label ~ ., data=training, trControl = train_control,
                   method='rf')
