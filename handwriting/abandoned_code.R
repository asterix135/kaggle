#####
#
# Abandoned Code
#
####

# kmeans via RWeka
# Spoiler Alert - Out of Memory Error :(s

require(RWeka)
classifier <- IBk(label~., data = training,
                  control = Weka_control(K = k))
eval_data <- evaluate_Weka_classifier(classifier, numFolds = 5)
