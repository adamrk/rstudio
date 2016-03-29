setwd('data/R/BNP/main')
library(xgboost)
library(caret)
library(plyr)
train <- read.csv('../BNPdata/train.csv', stringsAsFactors = F)
test <- read.csv('../BNPdata/test.csv', stringsAsFactors = F)
test$target <- -1
catcols <- c("v3", "v24", "v30", "v31", "v47", "v52", "v66", "v71", "v74", "v75", "v79", "v91",
             "v107", "v110", "v112", "v113")
all <- rbind(train, test)
all <- onehot(all, catcols)
all <- dumbcat(all)
all <- natoneg(all)
# 107: exact same as 91 
# 75: from table with 71 we loose info on <40 points
#110: completely determined by v47
#34: = 11.83 + .636v10 + .632v40
for.removal <- c("v107","v75", "v110", "v34", # the rest are taken from Tibees code
                 "v8","v23","v25","v36","v37","v46",
                 "v51","v53","v54","v63","v73","v81",
                 "v82","v89","v92","v95","v105",
                 "v108","v109","v116","v117","v118",
                 "v119","v123","v124","v128")

for(col in for.removal){
  all[col] <- NULL
}

train <- all[which(all$target != -1), ]
test <- all[which(all$target == -1), ]
remove(all)


train$newtarget <- rep("False")
train[which(train$target == 1), "newtarget"] <- rep("True")
target <- as.factor(train$newtarget)
train$newtarget <- NULL

set.seed(0)
system.time(model <- caretcv(as.matrix(train[,-(1:2)]), target))

# set.seed(0)
# cvindices <- makeindices(nrow(train), 5)
# basic <- data.frame(round=integer(), index=integer(), 
#                     score=double(), size=character(), stringsAsFactors = F)
# for(i in 1:5){
#   clf<-runxgb(newtrain, cvindices[[i]], 2)
#   inx <- clf$bestInd
#   score <- clf$bestScore
#   size <- as.numeric(object.size(clf))/1000000
#   basic <- rbind(basic, data.frame(round=i, index=inx, score=score, 
#                                    size=size, stringsAsFactors = F))
#   #time <- format(Sys.time(), "%Y%m%d%T%H%M%S")
#   #xgb.save(clf, paste("xgb_", time, ".model", sep = ""))
#   #write.csv(basic, paste("gridres_", time, ".csv", sep = ""))
# }
# 
# time <- format(Sys.time(), "%Y%m%d%T%H%M%S")
# write.csv(basic, paste("gridsearch_", time, ".csv", sep = ""))

# pred1 <- predict(clf, data.matrix(test[,-(1:2)]), ntreelimit=clf$bestInd)
# submission <- data.frame(ID=test$ID, PredictedProb=pred1)
# submission <- submission[order(submission$ID),]
# write.csv(submission, "xgb1500.csv", row.names = F)