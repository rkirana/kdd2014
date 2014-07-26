#################################################################################
###############Run a R RandomForest on  undersampled data ############################
######################################################################################
myTgt <- "is_exciting"
rfNms <- allNms [allNms %notin% c(cartNms, xgNms, posNms, "factor_school_state")]
# Run a random Forest
{
	myTgt <- "is_exciting"
	train <- as.data.frame (train); test <- as.data.frame (test)
	library (doMC)
	library (foreach)
	library (rbenchmark)
	numcores <- 6
	registerDoMC (cores = numcores)
	folderName <- "iamnotgoodenoughfolds"
	library (randomForest)
	for (cv_fold in c(1:mankraj_number_of_folds)) {
		trainY <- train[train$indices != cv_fold, myTgt]
		actual <- train[train$indices == cv_fold, myTgt]
		mIndex <- 1:length (actual)
		rf1 <- foreach(ntree=rep (300,3), .combine=randomForest::combine, .packages= 'RRF') %dopar% randomForest(train[train$indices != cv_fold,rfNms], as.factor (trainY), ntree=ntree, do.trace = 11, sampsize = c(20000, 20000))
		rf2 <- foreach(ntree=rep (300,3), .combine=randomForest::combine, .packages= 'RRF') %dopar% randomForest(train[train$indices != cv_fold,rfNms], as.factor (trainY), ntree=ntree, do.trace = 11, sampsize = c(20000, 20000))
		rf3 <- foreach(ntree=rep (300,3), .combine=randomForest::combine, .packages= 'RRF') %dopar% randomForest(train[train$indices != cv_fold,rfNms], as.factor (trainY), ntree=ntree, do.trace = 11, sampsize = c(20000, 20000))
		rf4 <- foreach(ntree=rep (300,3), .combine=randomForest::combine, .packages= 'RRF') %dopar% randomForest(train[train$indices != cv_fold,rfNms], as.factor (trainY), ntree=ntree, do.trace = 11, sampsize = c(20000, 20000))
		rf <- randomForest::combine (rf1, rf2, rf3, rf4)
		rm (rf1, rf2, rf3, rf4)
		prediction_valid <- predict (rf, train [train$indices == cv_fold, rfNms], type = "prob")[,2]
		prediction_test <- predict (rf, test [, rfNms], type = "prob")[,2]
		prediction <- c(prediction_valid, prediction_test)

		print (calc_auc (actual, prediction[1:length(actual)]))
		writeBlendFiles (prediction, mIndex, "usrf_m_ocpred_catint_pos", folder=folderName, cv_fold) 
	}	
}



