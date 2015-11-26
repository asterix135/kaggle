#####
# Building some alternate models
#####

# Make sure working directory is where the data is saved
# setwd('./handwriting')

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

rf_model <- train(label ~ ., data=training, method='rf')


# Better: use k-fold = ask prof about this

train_control <- trainControl(method = 'cv', number =10)
rf_model2 <- train(label ~ ., data=training, trControl = train_control,
                   method='rf')
