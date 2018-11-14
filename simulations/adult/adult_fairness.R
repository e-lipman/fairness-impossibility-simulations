source("../../share/make_heatmap.R")
source("../../share/overlay_heatmaps.R")

library(randomForest)

# setup and model
adult = read.csv("adult.txt")
adult <- transform(adult,
                   Male = as.factor(as.integer(Male==levels(adult$Male)[2])),
                   X50K = as.factor(as.integer(X50K==levels(adult$X50K)[2])))

rf <- randomForest(X50K~State.gov+Bachelors+Never.married+Adm.clerical+White+Male, 
                   data=adult)

probs_rf <- predict(rf, type="prob")[,2]

lmod = glm(X50K~State.gov+Bachelors+Never.married+Adm.clerical+White+Male, data=adult, family="binomial")
probs_lm = predict(lmod,type="response")

# make heatmaps
rf_metrics = make_heatmap(adult$X50K, probs_rf, adult$Male)
lm_metrics = make_heatmap(adult$X50K, probs_lm, adult$Male)

# overlay heatmaps
overlay(rf_metrics$fpr, rf_metrics$fnr, rf_metrics$ppv,)
overlay(lm_metrics$fpr, lm_metrics$fnr, lm_metrics$ppv, thresh=.01)