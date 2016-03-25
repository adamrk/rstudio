dumbcat <- function(df, leaveout = c()){
  features.names <- names(df)
  for(f in features.names){
    if( class(df[[f]]) == "character" & !(f %in% leaveout)){
      levels = unique(df[[f]])
      df[[f]] = as.integer(factor(df[[f]], levels=levels))
    }
  }
  return(df)
}

natoav <- function(df, leaveout = c()){
  features.names <- names(df)
  for(f in features.names){
    if(!(f %in% leaveout)){
      average <- mean(df[[f]], na.rm=T)
      df[[f]][which(is.na(df[[f]]))] <- average
    }
  }
  return(df)
}

natoneg <- function(df, leaveout = c()){
  features.names <- names(df)
  for(f in features.names){
    if(!(f %in% leaveout)){
      df[[f]][which(is.na(df[[f]]))]<- -10
    }
  }
  return(df)
}

makeindices <- function(nrows, nrounds){
  indices <- list()
  cvsize <- nrows %/% nrounds
  rows <- sample(1:nrows)
  for(i in 1:(nrounds - 1)){
    indices[[i]] <- rows[(1+(i-1)*cvsize):(i*cvsize)]
  }
  indices[[nrounds]] <- rows[(1+(nrounds-1)*cvsize):nrows]
  return(indices)
}

caretcv <- function(x,y){
  xgb_grid <- expand.grid(nrounds = 1000,
                          max_depth = c(8,10),
                          colsample_bytree = c(.8, 1.0),
                          min_child_weight = 3,
                          eta = .01,
                          gamma = 0)
  xgb_train <- trainControl(method = "cv",
                            number = 5,
                            verboseIter = TRUE,
                            returnData = FALSE,
                            returnResamp = "all",
                            classProbs = TRUE,
                            summaryFunction = mnLogLoss,
                            allowParallel = TRUE)
  xgb_model <- train(x,y, 
                     trControl = xgb_train, 
                     tuneGrid = xgb_grid, 
                     method = "xgbTree", 
                     eval_metric = "logloss")
  return(xgb_model)
}
  
  
  
  
runxgb <- function(train, cvinx){  
  md <- 11 # grid search 2, 4, 6, 8, 10
  ss <- .96 # grid search .5, .75, .96
  cs <- 0.45 # grid search .4, .6, .8, 1.0
  mc <- 3 # /% of rare events ????
  np <- 1
  nrounds <- 950
  early.stop.round <- 300
  dval <- xgb.DMatrix(data.matrix(train[cvinx,-(1:2)]), label = train$target[cvinx])
  dtrain <- xgb.DMatrix(data.matrix(train[-cvinx,-(1:2)]), label = train$target[-cvinx])
  watchlist <- list(val=dval, train=dtrain)
  param = list( objective='binary:logistic',
                booster='gbtree',
                eval_metric='logloss',
                eta=0.01,
                max_depth=md,
                subsample=ss,
                colsample_bytree=cs,
                min_child_weight=mc,
                num_parallel_tree=np )
  system.time(clf <- xgb.train(params=param, data=dtrain, nrounds=nrounds, verbose=1,
                               early.stop.round = early.stop.round, watchlist = watchlist, 
                               maximize = F))
  return(clf)
}