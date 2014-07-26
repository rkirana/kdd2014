folderName <- "iamnotgoodenoughfolds"

folder <- 'iamnotgoodenoughfolds'
folderName <- folder
for (cv_fold in c(1:mankraj_number_of_folds)) {
	trainY <- train[train$indices != cv_fold, myTgt]
	actual <- train[train$indices == cv_fold, myTgt]
	mIndex <- 1:length (actual)
	mIndex <- 1:length (actual)
	write.csv (actual, file = paste(folder, "/actual_fold_",cv_fold,'.csv', sep = ""), row.names = F)
	write.csv (train[train$indices == cv_fold, "projectid"], file = paste(folder, "/trainidentifiers_fold_",cv_fold,'.csv', sep = ""), row.names = F)
	write.csv (test$projectid, file = paste(folder, "/ids_fold_",cv_fold,'.csv', sep = ""), row.names = F)
}


	xtrain <- ensemble_create_file_new (folderName, mankraj_number_of_folds, flag_train=1, debugMode = 0, start_fold = 1)
	xtest <- ensemble_create_file_new (folderName, mankraj_number_of_folds, flag_train=0, debugMode = 0, start_fold = 1)
	xtrain$gbm_m_nbodda_pos_0.01_11_7 <- xtrain$gbm_m_nbodda_pos_fac_0.01_11_7
	xtest$gbm_m_nbodda_pos_0.01_11_7 <- xtest$gbm_m_nbodda_pos_fac_0.01_11_7

	algs <- names (xtrain) [names (xtrain) %notin% c("actual", "id", "cv_fold")]
	setDT (xtrain)
	xtrain [,algs,with=F] [,lapply(.SD, function (x) calc_auc (xtrain$actual, x))]

	myAUCs <- xtrain [,algs, with=F] [,lapply(.SD, function (x) calc_auc (xtrain$actual, x))]
	myAUCs <- as.numeric (myAUCs)
	temp <- rowSums(xtrain [,algs, with = F] * myAUCs)/sum(myAUCs)
	

	setDT (xtest); setkey (xtest, "id")
	xtest_old <- copy(xtest)	
	xtest [,cv_fold := NULL]
	xtest <- xtest_old[,lapply(.SD, mean,na.rm=TRUE), by = id]
	tempt <- rowSums(xtest [,algs, with = F] * myAUCs)/sum(myAUCs)
	xtest$weighted_sub <- tempt

	setDT (xtest)
	xtest_new <- xtest
	xtest_new [,is_exciting := weighted_sub] 
	submissionDF <- xtest_new[,list(id, is_exciting)]
	setnames (submissionDF, "id", "projectid")
		
	xtest <- as.data.frame (xtest)
	xtrain <- as.data.frame (xtrain)
	submissionDF <- submissionDF[order(submissionDF$projectid),]
	setDT (test)
	submissionDF <- merge (submissionDF, test [,list (projectid, date_posted)], by = "projectid", all.x=TRUE)
	submissionDF <- submissionDF[order(submissionDF$projectid),]
	submissionDF [,usingdates := as.numeric (difftime (date_posted, min(date_posted), units = "days"))]
	submissionDF [,diffdays := floor (usingdates/14)]
	submissionDF_old <- submissionDF
	submissionDF_dates <- data.table( diffdays=sort(unique(submissionDF$diffdays)))
	submissionDF_dates [, degenerate_scaling_factor := 1-diffdays/(nrow(submissionDF_dates)+1)]
	submissionDF <- merge(submissionDF, submissionDF_dates, by="diffdays")
	submissionDF[,is_exciting_old := is_exciting]
	submissionDF[, is_exciting := is_exciting*degenerate_scaling_factor]
	write.csv (submissionDF[,list(projectid, is_exciting)], file = 'weighted_mank_scaled.csv', row.names = F)

