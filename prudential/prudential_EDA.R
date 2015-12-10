pru <- read.csv('train.csv')

str(pru)

# figure out quantity of NA values

na_data <- vector()
for (i in 1:ncol(pru)) {
    na_data <- c(na_data, sum(is.na(pru[,i])))
}
names(na_data) <- colnames(pru)

na_data[na_data > 0]

# FINAL column, Response, is the response variable
table(pru$Response)
