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

model1 <- train(label ~ ., data=testing, method='logreg')
