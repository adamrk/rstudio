setwd('data/R/BNP')
library(xgboost)
train <- read.csv('train.csv', stringsAsFactors = F)
test <- read.csv('test.csv', stringsAsFactors = F)
test$target <- -1
all <- rbind(train, test)
all <- dumbcat(all)
all <- natoneg(all)
# 107: exact same as 91 
# 75: from table with 71 we loose info on <40 points
#110: completely determined by v47
#34: = 11.83 + .636v10 + .632v40
for.removal <- c('v107', 'v75', 'v110', 'v34')
for(col in for.removal){
  all[col] <- NULL
}

train <- all[which(all$target != -1), ]
test <- all[which(all$target == -1), ]
remove(all)

set.seed(0)
cvindices <- makeindices(nrow(train), 5)
for(i in 1:5){
  clf<-runxgb(train, cvindices[[i]])
  inx <- clf$bestInd
  score <- clf$bestScore
  size <- object.size(clf)/1000000
  basic <- paste( "round", i, "index:", inx, "score:", score, "size:", size, "(really megabytes)")
  time <- format(Sys.time(), "%Y%m%d%T%H%M%S")
  xgb.save(clf, paste("xgb_", time, ".model", sep = ""))
  write(basic, paste("xgb_", time, ".txt", sep = ""))
  remove(clf)
}


# pred1 <- predict(clf, data.matrix(test[,-(1:2)]), ntreelimit=clf$bestInd)
# submission <- data.frame(ID=test$ID, PredictedProb=pred1)
# submission <- submission[order(submission$ID),]
# write.csv(submission, "xgb1500.csv", row.names = F)