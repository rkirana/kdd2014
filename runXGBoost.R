myTgt <- "is_exciting"
vwNms_old <- allNms [allNms %notin% c(facNms_good, "factor_school_state", "date_posted_year", "date_posted_mth")]
vwNms <- allNms [allNms %notin% c(facNms_good, "factor_school_state", "date_posted_mth", cartNms, xgNms, lengthNms, posNms, dampNms, boddaNms, myNms_larko, "date_posted_year")]
myTrain <- cBind (facMtx_train, as (as.matrix(train[,vwNms_old]), "dgCMatrix"))
myTest <- cBind (facMtx_test,as (as.matrix(test[,vwNms_old]), "dgCMatrix") )
{
	myTgt <- "is_exciting"
	train <- as.data.frame (train); test <- as.data.frame (test)
	folderName <- "iamnotgoodenoughfolds"
	for (cv_fold in c(1:mankraj_number_of_folds)) {
		trainY <- train[train$indices != cv_fold, myTgt]
		actual <- train[train$indices == cv_fold, myTgt]
		mIndex <- 1:length (actual)
		
		prediction <- run.xgboost (myTrain[train$indices != cv_fold,], myTrain [train$indices == cv_fold,] , myTest , trainY, actual,conf_file='xgboost.conf', booster_type=0,task="binary:logistic",eval_metric="auc", num_round=60, seed=11, 	  etaArray=c( 0.05, 0.3, 1), gammaArray=c(1), depthArray = c(3, 7, 11), childArray=c(1), subsampleArray=c(1), treeMakerArray=c(1), lambda1Array, lambda2Array,alphaArray, outputModel, inputModel = NULL, model_dir = ".", nthread = 3, trainFile = 'myTrain.libsvm', validFile = 'myValid.libsvm', testFile = 'myTest.libsvm', minpct = 0)


		print (calc_auc (actual, prediction[1:length(actual)]))
		writeBlendFiles (prediction, mIndex, "vw_sparse_txt_glm_only", folder=folderName, cv_fold)
	}	
}
rm (myTrain, myTest)
gc ()

