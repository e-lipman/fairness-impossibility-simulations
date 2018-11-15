source("../../share/make_heatmap.R")
source("../../share/overlay_heatmaps.R")
library(randomForest)

# setup and model
compass = read.csv("propublica_data.csv", stringsAsFactors=F)

compass$White = 1-rowSums(compass[,c("African_American","Asian","Hispanic",
                                     "Native_American","Other")]) 

compass = compass[compass$White+compass$African_American==1,!names(compass)%in%c("Asian",
                                                                                 "Native_American","Hispanic","Other","score_factor")]

compass$Two_yr_Recidivism <- factor(compass$"Two_yr_Recidivism")

rf <- randomForest(Two_yr_Recidivism~.,
                   data = compass[,!names(compass)=="White"])

probs_rf <- predict(rf, type="prob")[,2]

lmod <- glm(Two_yr_Recidivism~., compass[,!names(compass)=="White"], family="binomial")
probs_lm <- predict(lmod,type="response")

# make heatmaps
rf_metrics = make_heatmap(compass$Two_yr_Recidivism, probs_rf, compass$White)
lm_metrics = make_heatmap(compass$Two_yr_Recidivism, probs_lm, compass$White)

# overlay heatmaps
overlay(rf_metrics$fpr, rf_metrics$fnr, rf_metrics$ppv, thresh=.025)
overlay(lm_metrics$fpr, lm_metrics$fnr, lm_metrics$ppv, thresh=.025)