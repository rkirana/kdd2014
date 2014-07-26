
##################################################################################
###############Run a Vowpal Wabbit  ############################
######################################################################################
myTgt <- "is_exciting"
train <- as.data.frame (train)
test <- as.data.frame (test)
vwNms_old <- allNms [allNms %notin% c(facNms_good, "factor_school_state")]
vwNms <- allNms [allNms %notin% c(facNms_good, "factor_school_state", "date_posted_mth")]
vwNms <- vwNms [vwNms %notin% c("date_posted_year", "date_posted_mth", "flag_missing")]
myTrain <- cBind (train_text, facMtx_train, as (as.matrix(train[,vwNms]), "dgCMatrix"))
myTest <- cBind (test_text, facMtx_test,as (as.matrix(test[,vwNms]), "dgCMatrix") )
{
	myTgt <- "is_exciting"
	train <- as.data.frame (train); test <- as.data.frame (test)
	folderName <- "iamnotgoodenoughfolds"
	vwtrainFile <- 'train.vw'; vwvalidFile <- 'valid.vw';
	for (cv_fold in c(1:mankraj_number_of_folds)) {
		trainY <- train[train$indices != cv_fold, myTgt]
		actual <- train[train$indices == cv_fold, myTgt]
		mIndex <- 1:length (actual)
		create_vw_input_files (vwtrainFile, vwvalidFile, ifelse (trainY==0,"-1",trainY), myTrain[train$indices != cv_fold,], rBind (myTrain[train$indices == cv_fold,], myTest), rep (1, length(actual) + nrow(test)), flagOldMethod = 1 )
		bestPrediction <- runVW_all (myLossType = "logistic", vwtrainFile, vwvalidFile, validRows=mIndex, actual, flag_nn = 0, myPasses = 20, learnRateArray = c(0.05, 0.25, 0.01), hiddenNodeArray = c(1), decayLearnRateArray = c(0.05, 1 ),  myWeights=NULL, validIds=NULL, validWeights=NULL, flag_error_metric = "auc", c(0))		
		prediction <- bestPrediction
		print (calc_auc (actual, prediction[1:length(actual)]))
		writeBlendFiles (prediction, mIndex, "vw_all_sparse_txt", folder=folderName, cv_fold)
	}	
}
rm (myTrain, myTest)
gc ()




