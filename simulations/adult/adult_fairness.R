source("../../share/make_heatmap.R")

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

make_heatmap(adult$X50K, probs_rf, adult$Male)
make_heatmap(adult$X50K, probs_lm, adult$Male)