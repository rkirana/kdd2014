rfNms <- grrfNms
# Run a weighted random Forest on full data using only the greedy rf names
{
	myTgt <- "is_exciting"
	train <- as.data.frame (train); test <- as.data.frame (test)
	folderName <- "iamnotgoodenoughfolds"
		library (foreach)
	library (randomForest)
library (doParallel)
cl <- makeCluster (8)
registerDoParallel(cl)
	for (cv_fold in c(1:mankraj_number_of_folds)) {
		trainY <- train[train$indices != cv_fold, myTgt]
		actual <- train[train$indices == cv_fold, myTgt]
		mIndex <- 1:length (actual)
		rf <- foreach(ntree=rep (68,8), .combine=RRF::combine, .packages= 'RRF') %dopar% 
		RRF(train[train$indices != cv_fold,rfNms], as.factor (trainY), ntree=ntree, 
		do.trace = 11, classwt = 1-table (trainY)/length(trainY), flagReg=0)
		prediction_valid <- predict (rf, train [train$indices == cv_fold, rfNms], type = "prob")[,2]
		prediction_test <- predict (rf, test [, rfNms], type = "prob")[,2]
		prediction <- c(prediction_valid, prediction_test)

		print (calc_auc (actual, prediction[1:length(actual)]))
		writeBlendFiles (prediction, mIndex, "wrf_grrfNms", folder=folderName, cv_fold) 
	}	
}
# Run a inverse weighted random Forest
{
	myTgt <- "is_exciting"
	train <- as.data.frame (train); test <- as.data.frame (test)
	folderName <- "iamnotgoodenoughfolds"
		library (foreach)
	library (randomForest)
library (doParallel)
cl <- makeCluster (8)
registerDoParallel(cl)
	for (cv_fold in c(1:mankraj_number_of_folds)) {
		trainY <- train[train$indices != cv_fold, myTgt]
		actual <- train[train$indices == cv_fold, myTgt]
		mIndex <- 1:length (actual)
		rf <- foreach(ntree=rep (32,8), .combine=RRF::combine, .packages= 'RRF') %dopar% 
		RRF(train[train$indices != cv_fold,rfNms], as.factor (trainY), ntree=ntree, 
		do.trace = 11, classwt = table (trainY)/length(trainY), flagReg=0)
		prediction_valid <- predict (rf, train [train$indices == cv_fold, rfNms], type = "prob")[,2]
		prediction_test <- predict (rf, test [, rfNms], type = "prob")[,2]
		prediction <- c(prediction_valid, prediction_test)

		print (calc_auc (actual, prediction[1:length(actual)]))
		writeBlendFiles (prediction, mIndex, "iwrf_grrfNms", folder=folderName, cv_fold) 
	}	
}
