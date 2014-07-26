# glmnet lasso + glmnet ridge + glmnet elasticnet => what features are the union of these => use in a gbm to see what is the result? xxx

#    "reg:linear" --linear regression;     "reg:logistic" --logistic regression;     "binary:logistic" --logistic regression for binary classification, output probability;     "binary:logitraw" --logistic regression for binary classification, output score before logistic transformation;     "multi:softmax" --set XGBoost to do multiclass classification using the softmax objective, you also need to set num_class(number of classes);     "rank:pairwise" --set XGBoost to do ranking task by minimizing the pairwise loss
libLINPath <- '/home/rkirana/oldPC/myDocs/liblinear-1.93/'
liblinPath <- '/home/rkirana/oldPC/myDocs/liblinear-1.93/'
libfm_command <- "/home/rkirana/libfm-1.40.src/bin/libFM"



predict_from_rf <- function (bestGBM, temp, gbmNewVars, bestTrees, start = 1, step = 100000) {
		myPred_myTest <- NULL
		end <- step
		while (end <= nrow (temp)) {
			print (start)
			print (paste (start, end , sep=":"))
			mank <- predict (bestGBM,temp[start:end,gbmNewVars], type = "prob")[,2]
			print (summary (mank))
			myPred_myTest <- c(myPred_myTest, mank)
			rm (mank)
			gc ()

			start <- start + step
			end <- ifelse (end + step > nrow(temp), nrow(temp), end + step)	
			if (start > nrow(temp)) break
			if (start > end ) break	
		}
		return (myPred_myTest)
	}



runLIBLIN <- function (typeArray, trainFile, testFile, actual, tryCosts = c (1000, 100, 10, 1, 0.1, 0.25, 0.5, 0.75, 0.01, 0.001, 0.0001, 0.00001), myTolerance = 0.0001,manktype = "-c", error_metric = "auc") {
if (error_metric == "auc")
	bestAUC <- 0
if (error_metric == "rmsle")
	bestAUC <- 1e7
if (error_metric == "mse")
	bestAUC <- 1e7
	myAUCarray <- NULL
	for (myType in typeArray) {
	for (myCost in tryCosts) {
		system (paste (paste(liblinPath, 'train', sep=''), paste ("-s", myType, sep= " "), paste ("-c", myCost, sep = " "), paste ("-B", -1, sep = " "), paste ("-e", myTolerance, sep = " "), trainFile, sep=" "))
		system (paste (paste(liblinPath, 'predict', sep=''), paste ("-b", 1, sep = " "), testFile, paste(trainFile,  ".model",sep=""), paste ("output",myType, ".csv", sep="_"), sep = " "))
		tbl <- read.table (paste ("output",myType, ".csv", sep="_"), header=TRUE, sep=" ")
		# CREATE A MATRIX WITH THE PREDICTIONS
if (manktype == "-c") {
		myNms_tbl <- names (tbl)
		for (name in 2:length (myNms_tbl)) 
			myNms_tbl[name] <- substr (myNms_tbl[name], 2, nchar (myNms_tbl[name]))
		myNms_tbl [1] <- -1
		prediction <- tbl [,"X1"]
}
if (manktype == "-r") {
		prediction <- tbl$labels

}
if (error_metric == "auc")
		myAUC <- calc_auc (actual[which(!is.na(actual))], prediction [1:length(actual)][which(!is.na(actual))])
if (error_metric == "rmsle")
		myAUC <- rmsle (expm1(actual[which(!is.na(actual))]), expm1(prediction [1:length(actual)][which(!is.na(actual))]))
if (error_metric == "mse")
		myAUC <- mse ((actual[which(!is.na(actual))]), (prediction [1:length(actual)][which(!is.na(actual))]))

		print (paste (myAUC, myCost, sep = ' '))
if (error_metric == "auc") {
		if (myAUC > bestAUC) {
			bestAUC <- myAUC
			bestPrediction <- prediction
			bestType <- myType
			bestCost <- myCost
		}
}
if (error_metric != "auc") {
		if (myAUC < bestAUC) {
			bestAUC <- myAUC
			bestPrediction <- prediction
			bestType <- myType
			bestCost <- myCost
		}
}

		myAUCarray <- c(myAUCarray, myAUC)
	}
	}
	print (tryCosts)
	print (myAUCarray)
	print (bestCost)
	save (bestCost, bestType, file = 'bestLIBLIN.rData')
	return (bestPrediction)
}



write_libsvm_fast <- function (xmain, filename, y, blocksize = 100000, flag_remove=1, flag_dt=0) {
	if (flag_remove == 1) 
		system (paste ("rm -f", filename))
	library (data.table)
	library (Matrix)
	options (scipen = 999)
	print ("Building feature man")
	library (Matrix)
	gc ()
		
	startrow <- 1
	endrow <- min (blocksize, nrow(xmain))

	while (endrow <= nrow(xmain)) {
		print (paste (startrow, endrow, ":"))
if (flag_dt == 1)
		x <- as (as.matrix (as.data.frame (xmain[startrow:endrow,modelNms,with=F])), "dgCMatrix")
if (flag_dt == 0)
		x <- as (as.matrix (as.data.frame (xmain[startrow:endrow,modelNms])), "dgCMatrix")
		rowNums <- x@i + 1
		tempCols <- x@p + 1
		temp_tempCols <- tempCols [2:length(tempCols)] - tempCols [1:length(tempCols)-1]
		rm (tempCols)
		gc ()
		colNums <- rep (1:length(temp_tempCols), temp_tempCols) 
		rm (temp_tempCols)
		gc ()
		myVals <- x@x
		gc ()
		# order by rows
		myVals <- myVals [order (rowNums)]
		colNums <- colNums [order (rowNums)]
		rowNums <- sort (rowNums)

		# for each row 
		myDF <- data.table (rowNums = rowNums, colNums = colNums, vals = myVals)
		#print ("Finished Creating the monster DF")
		rm (rowNums, colNums, myVals)
		gc ()

		myDF [,libsvm_col := paste (colNums, vals, sep = ":")]
		myDF <- myDF[,list(rowNums, libsvm_col)]
		#print ("created Libsvm column")
		myDF <- myDF[,lapply (.SD, paste, collapse = " "),by = rowNums]
		gc ()
		myDF [, output := as.character (y[startrow:endrow])]
		myDF [,libsvm_row := paste (output, libsvm_col, sep = " ")]
		write.table (myDF[,list (libsvm_row)], file=filename, row.names = F, col.names = F, quote=F, append=T)
		rm (myDF)	
		gc ()
		startrow <- startrow + blocksize
		endrow <- endrow + blocksize
		if (endrow >= nrow(xmain)) {
			endrow <- nrow(xmain)
		}
		if (startrow > endrow) 
			break;
		if (startrow > nrow(xmain)) 
			break;
	}
}		



write_libsvm_file <- function (x, fileName = "out.dat", y = NULL) 
{
	library (data.table)
	library (Matrix)
	options (scipen = 999)
	#print ("Building feature man")
	library (Matrix)
#	x <- as (x, "dgCMatrix")

	rowNums <- x@i + 1
	tempCols <- x@p + 1
	temp_tempCols <- tempCols [2:length(tempCols)] - tempCols [1:length(tempCols)-1]
	colNums <- rep (1:length(temp_tempCols), temp_tempCols) 
	myVals <- x@x
	rm (tempCols, temp_tempCols)
	gc ()

	# order by rows
	myVals <- myVals [order (rowNums)]
	colNums <- colNums [order (rowNums)]
	rowNums <- sort (rowNums)
	
	# for each row 
	myDF <- data.frame (rowNums = rowNums, colNums = colNums, vals = myVals)
	#print ("Finished Creating the monster DF")
	rm (rowNums, colNums, myVals)
	gc ()
	myDF <- as.data.table (myDF)

	myDF <- myDF [,libsvm_col := paste (colNums, vals, sep = ":")]
	myDF <- myDF[,list(rowNums, libsvm_col)]
	#print ("created Libsvm column")
	myDF_new <- myDF[,lapply (.SD, paste, collapse = " "),by = rowNums]
	rm (myDF)
	gc ()
	myDF_new <- myDF_new [, output := as.character (y)]
	myDF_new <- myDF_new [,libsvm_row := paste (output, libsvm_col, sep = " ")]
	write.table (myDF_new[,list (libsvm_row)], file=fileName, row.names = F, col.names = F, quote=F)
	#print ("Finished")
}


runLIBFM <- function (trainFile, validFile, metaFile = NULL, type = 'c', actual, stdevArray = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75),numVectorArray = c(1, 2, 4, 7, 11, 16), numiter = 205, testFlag  = 0, myTestFile = 'test.libfm', outFile = 'out.csv', cv_fold) {
	method <- "mcmc"
	task <- paste ("-task", type, sep = ' ')
	
	myMethod <- paste ("-method", "mcmc", sep=" ")
	verbosity <- paste ("-verbosity 1")
	out <- paste("-out", outFile)
	myTrainFile <- paste ("-train", trainFile, sep=" ")
	myValidFile <- paste ("-test", validFile, sep=" ")
	
	iter <- paste ("-iter", numiter, sep=" ")

	bestAUC <- 0

	aucs <- NULL
	stdevs <- NULL
	vectors <- NULL
	# find best stdev and numvectors combo
	for (stdev in stdevArray) {
		for (numvectors in numVectorArray) {
			init_stdev <- paste ("-init_stdev", stdev, sep=" ")
			dims <- paste ("-dim '1,1,", numvectors, "'", sep="")
			system (		paste (libfm_command, task, myTrainFile, myValidFile, dims, iter, myMethod, init_stdev, verbosity, out)			)
			tbl <- read.csv (outFile, header=FALSE)
			prediction <- tbl$V1
			pred <- prediction[1:length(actual)]
			myAUC <- calc_auc (actual, pred)
			print (paste('myAUC', stdev, myAUC))
			if (myAUC > bestAUC) {
				bestAUC <- myAUC
				bestPrediction <- prediction
				bestStdev <- init_stdev
				bestVectors <- dims
				best_stdev <- stdev
				best_vectors <- numvectors
			}
			aucs <- c(aucs, myAUC)
			stdevs <- c(stdevs, stdev)
			vectors <- c(vectors, numvectors)
			print (aucs)
			print (stdevs)
			print (vectors)
		}
	}
	print (paste("Best Stdev", bestStdev))
	print (paste("Best Vectors", bestVectors))
	save (bestVectors, bestStdev, myTrainFile, task, bestVectors, iter, myMethod, verbosity, out, file = paste ('bestLIBFMParams', cv_fold, '.rData',sep=''))
	save (best_stdev, best_vectors, file = paste ('bestLIBFM', cv_fold, '.rData',sep=''))

	iter <- paste ("-iter", numiter*2, sep=" ")
	system (		paste (libfm_command, task, myTrainFile, myValidFile, bestVectors, iter, myMethod, bestStdev, verbosity, out)			)
	tbl <- read.csv (outFile, header=FALSE)
	bestPrediction <- tbl$V1

	print (vectors)
	print (aucs)
	print (stdevs)

	if (testFlag == 1) {
		myTestFile <- paste ("-test", myTestFile, sep=" ")
		system (		paste (libfm_command, task, myTrainFile, myTestFile, bestVectors, iter, myMethod, bestStdev, verbosity, out)			)
		tbl <- read.csv (paste("mankoutput", 1, ".txt", sep=""), header=FALSE)
		bestPrediction_test <- tbl$V1
		bestPrediction <- c(bestPrediction ,bestPrediction_test)
	}
		
	return (bestPrediction)
}




runGLMNET <- function (cv_fold, prefix, folder) {
		trainY <- train[train$indices != cv_fold, myTgt]
		actual <- train[train$indices == cv_fold, myTgt]
		mIndex <- 1:length (actual)
		myList <- runGLMNET_d_g (cBind (as (as.matrix(train[train$indices!=cv_fold, glmnetNms]), "dgCMatrix"), myTrain[train$indices != cv_fold,]), cBind (as (as.matrix (train[train$indices == cv_fold, glmnetNms]), "dgCMatrix"), myTrain[train$indices == cv_fold,]), cBind (as(as.matrix(test[ ,glmnetNms]), "dgCMatrix"),myTest), trainY, actual, validY=rep(0,length(actual)), myMetric = "auc",  alphaArray = c(0.1, 0.25, 0.5, 0.75, 1),validIds=NULL, validWeights=NULL)
		bestPrediction <- myList [[5]][1:length(actual)]
		print (calc_auc (actual, bestPrediction))
		bestPrediction_test <- predict (myList [[1]], cBind(as (as.matrix(test[,glmnetNms]), "dgCMatrix"), myTest)[,myList[[4]]], type = "response",s=myList [[3]])
		save (myList , file = paste ('temp', cv_fold, '.rData', sep =''))
		writeBlendFiles (c(bestPrediction,bestPrediction_test), mIndex, prefix, folder, cv_fold) 
		return (cv_fold)
	}

#myLME <- lmer (as.formula ("ACTION ~ (1|MGR_ID)"), data=train)
# add damped averages
fn_addDampAvg <- function (train, valid, varList, myTgt) {

	train$orig_id <- c(1:nrow(train))
	valid$orig_id <- c(1:nrow(valid))

	for (varName in varList) {
		print (varName)
		
		train[,varName] <- as.factor (train[,varName])
		valid[,varName] <- as.factor (valid[,varName])
		trainLevels <- as.character (levels (train[,varName]))
		validLevels <- as.character (levels (valid[,varName]))

		myForm <- paste (myTgt, " ~ (1|", varName, ")")
		myForm <- as.formula (myForm)
		myLME <- lmer (myForm, train, REML=FALSE, verbose=TRUE)
		myFixEf <- fixef (myLME)
		myRanEf <- unlist (ranef (myLME))

		temp <- as.data.table ( train[,c("id", varName)])
		setnames (temp, varName, "manks")
		temp <- temp [,list (myX = nrow(.SD)), by = manks]
		setnames (temp, "manks", "levelName")
		temp <- as.data.frame (temp)
		#print (paste(length(trainLevels), length(myRanEf), length (myFixEf)))
		myLMERDF <- data.frame (levelName = trainLevels, myDampVal = myRanEf+myFixEf)
		myLMERDF <- merge (myLMERDF, temp, by = "levelName")
		myLMERDF <- as.data.table (myLMERDF)
		setnames (myLMERDF, "levelName", varName)

		myLMERDF <- as.data.frame (myLMERDF)
		rm (temp)

		train[,varName] <- as.character (train[,varName])
		valid[,varName] <- as.character (valid[,varName])

		train <- merge (train, myLMERDF, by = varName, all.x=TRUE)
		valid <- merge (valid, myLMERDF, by = varName, all.x=TRUE) 
		valid$myDampVal <- with (valid, ifelse (is.na(myDampVal), myFixEf, myDampVal))
		valid$myX <- with (valid, ifelse (is.na(myX), 0 ,myX))
		
		train <- as.data.table (train)
		valid <- as.data.table (valid)
		setnames (train, "myDampVal", paste ("ftr_damp", varName, sep="_"))
		setnames (valid, "myDampVal", paste ("ftr_damp", varName, sep="_"))
		setnames (train, "myX", paste ("ftr_dampnum", varName, sep="_"))
		setnames (valid, "myX", paste ("ftr_dampnum", varName, sep="_"))
		train <- as.data.frame (train)
		valid <- as.data.frame (valid)
		rm (myLMERDF)
		gc ()

	}

	train <- train[order(train$orig_id),]
	valid <- valid [order(valid$orig_id),]

	train$orig_id <- NULL
	valid$orig_id <- NULL
	myList <- list()
	myList [[1]] <- train
	myList [[2]] <- valid
	
	return (myList)
}

KorpusCount <- function (x)  {
	myTbl <- table (x@TT.res$wclass)


	return (c (x@desc$all.chars, x@desc$lines, x@desc$normalized.space, x@desc$chars.no.space,
x@desc$punct, x@desc$digits, x@desc$letters.only, x@desc$words, x@desc$sentences, x@desc$avg.sentc.length, x@desc$avg.word.length
,ifelse (length(myTbl[names (myTbl) %in% "noun"]) > 0, myTbl[names (myTbl) %in% "noun"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "pronoun"]) > 0, myTbl[names (myTbl) %in% "pronoun"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "verb"]) > 0, myTbl[names (myTbl) %in% "verb"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "adverb"]) > 0, myTbl[names (myTbl) %in% "adverb"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "conjunction"]) > 0, myTbl[names (myTbl) %in% "conjunction"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "interjection"]) > 0, myTbl[names (myTbl) %in% "interjection"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "preposition"]) > 0, myTbl[names (myTbl) %in% "preposition"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "adjective"]) > 0, myTbl[names (myTbl) %in% "adjective"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "possessive"]) > 0, myTbl[names (myTbl) %in% "possessive"], 0)
,ifelse (length(myTbl[names (myTbl) %in% c("determiner", "predeterminer")]) > 0, myTbl[names (myTbl) %in% c("determiner", "predeterminer")], 0)
,ifelse (length(myTbl[names (myTbl) %in% "modal"]) > 0, myTbl[names (myTbl) %in% "modal"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "comma"]) > 0, myTbl[names (myTbl) %in% "comma"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "particle"]) > 0, myTbl[names (myTbl) %in% "particle"], 0)
))
}


# Run GBM
runGBM_time <- function (cv_fold, prefix, weighted = FALSE, folder="mankfolds", myNms, myTgt, bag.fraction = 0.8, train, test, dampDF, dampDF_test, myTrees=150, myMaxTrees = 1500, myDistribution = "bernoulli") {
	trainY <- train[train$indices < cv_fold, myTgt]
	actual <- train[train$indices == cv_fold, myTgt]
	mIndex <- 1:length (actual)
	trainWeights <- NULL
	validWeights <- NULL
	if (weighted == TRUE) {
		trainWeights <- train [train$indices < cv_fold, "flag_covariate_glmnet"]
		validWeights <- train [train$indices == cv_fold, "flag_covariate_glmnet"]
	}
	write.csv (actual, file = paste(folder, "/actual_fold_",cv_fold,'.csv', sep = ""), row.names = F)
	write.csv (train[train$indices == cv_fold, "projectid"], file = paste(folder, "/trainidentifiers_fold_",cv_fold,'.csv', sep = ""), row.names = F)
	write.csv (test$projectid, file = paste(folder, "/ids_fold_",cv_fold,'.csv', sep = ""), row.names = F)

	dampDF_test <- dampDF_test [dampDF_test$cv_fold == cv_fold,]; dampDF_test$cv_fold <- NULL
	setDT (test); setDT (dampDF_test); setkey (test, "id"); setkey (dampDF_test, "id")
	test <- merge (test, dampDF_test, by = "id", all.x=TRUE)
	test <- as.data.frame (test)
	
	dampDF <- dampDF [dampDF$cv_fold == cv_fold,]; dampDF$cv_fold <- NULL
	setDT (train); setDT (dampDF); setkey (train, "id"); setkey (dampDF, "id")
	train <- merge (train, dampDF, by = "id", all.x=TRUE)
	train <- as.data.frame (train)
	



	library (gbm)
	bestPrediction <- runGBM_all (train[train$indices < cv_fold, myNms], train[train$indices == cv_fold, myNms], myScore=test[,myNms], trainY, myInteractionDepth=7, myShrinkage=0.01, myMinObsInNode=10, actual_new = actual, myTrees=myTrees, myMaxTrees=myMaxTrees, myDistribution = myDistribution, minImprovement = 0.002, flag_error_metric = "auc", myWeights=trainWeights, validIds=NULL, validWeights=validWeights, myVerbose = FALSE, cv_fold, bag.fraction)
	print (calc_auc (actual, bestPrediction[1:length(actual)]))
	load ( paste ('kiranGBM', cv_fold, '.rData', sep=''))
	bestPrediction_test <- predict (bestGBM, test [,myNms], type = "response", bestTrees)
	writeBlendFiles (c(bestPrediction,bestPrediction_test), mIndex, prefix, folder, cv_fold) 
	return (cv_fold)

}


# Run GBM
runGBM <- function (cv_fold, prefix, weighted = FALSE, folder="mankfolds", myNms, myTgt, bag.fraction = 0.5,myInteractionDepth=7, myDistribution = "bernoulli", myShrinkage=0.01,myTrees=150, myMaxTrees=2000, myMinObsInNode=10, myVerbose=TRUE) {
	trainY <- train[train$indices != cv_fold, myTgt]
	actual <- train[train$indices == cv_fold, myTgt]
	mIndex <- 1:length (actual)
	trainWeights <- NULL
	validWeights <- NULL
	if (weighted == TRUE) {
		trainWeights <- train [train$indices != cv_fold, "flag_covariate_glmnet"]
		validWeights <- train [train$indices == cv_fold, "flag_covariate_glmnet"]
	}
	write.csv (actual, file = paste(folder, "/actual_fold_",cv_fold,'.csv', sep = ""), row.names = F)
	write.csv (train[train$indices == cv_fold, "projectid"], file = paste(folder, "/trainidentifiers_fold_",cv_fold,'.csv', sep = ""), row.names = F)
	write.csv (test$projectid, file = paste(folder, "/ids_fold_",cv_fold,'.csv', sep = ""), row.names = F)


	library (gbm)
	bestPrediction <- runGBM_all (train[train$indices != cv_fold, myNms], train[train$indices == cv_fold, myNms], myScore=test[,myNms], trainY, myInteractionDepth=myInteractionDepth, myShrinkage=myShrinkage, myMinObsInNode=myMinObsInNode, actual_new = actual, myTrees=myTrees, myMaxTrees=myMaxTrees, myDistribution = myDistribution, minImprovement = 0.002, flag_error_metric = "auc", myWeights=trainWeights, validIds=NULL, validWeights=validWeights, myVerbose = myVerbose, cv_fold, bag.fraction)
	print (calc_auc (actual, bestPrediction[1:length(actual)]))
	load ( paste ('kiranGBM', cv_fold, '.rData', sep=''))
	bestPrediction_test <- predict (bestGBM, test [,myNms], type = "response", bestTrees)
	#submissionDF <- data.frame (projectid = test$projectid, is_exciting = bestPrediction_test, stringsAsFactors=F)
	writeBlendFiles (c(bestPrediction,bestPrediction_test), mIndex, prefix, folder, cv_fold) 
	return (cv_fold)

}

run.xgboost <- function (myTrain, myValid, myTest, trainY, actual,conf_file='xgboost.conf', booster_type=0, task="binary:logistic",eval_metric="auc", num_round=2, seed=11, etaArray=c(0.3, 1, 0.05), gammaArray=c(1), depthArray = c(6, 4, 10, 20), 
childArray=c(1), subsampleArray=c(1), treeMakerArray=c(1), lambda1Array, lambda2Array,alphaArray, outputModel, inputModel = NULL, model_dir = "xgmodels", nthread = 2, trainFile = 'myTrain.libsvm', validFile = 'myValid.libsvm', testFile = 'myTest.libsvm', minpct = 0.001) {
    system ("rm -f *.libsvm *.buffer *.model pred.txt")
    write.libsvm.file (myTrain, file = trainFile, trainY)
    write.libsvm.file (rBind (myValid, myTest), file = validFile, c(actual, rep (0, nrow(myTest))))

    bestRounds <- num_round
    stepRounds <- num_round
    numRounds <- num_round
    aucArray <- NULL
    bestAUC <- 0
    for (eta in etaArray) {
    for (gamma in gammaArray) {
    for (min_child_weight in childArray) {
    for (subsample in subsampleArray) {
    for (tree_maker in treeMakerArray) {
    for (depth in depthArray) {
	k<- 1

    while (1) {   
	if (k==1)
		write.xgboost.params (conf_file=conf_file, booster_type=booster_type, task='train', nthread = nthread, trainFile=trainFile, testFile=validFile, eval_metric=eval_metric, num_round=stepRounds,seed=mankraj_seed, tree_booster_params = list (eta=eta, gamma=gamma, min_child_weight = min_child_weight, subsample = subsample, tree_maker = tree_maker, tree_depth = depth),linear_booster_params = list (), model_in = NULL, outputModel='xgboost.model', inputModel = NULL, silent = 0)
	if (k>1)
		write.xgboost.params (conf_file=conf_file, booster_type=booster_type, task='train', nthread = nthread, trainFile=trainFile, testFile=validFile, eval_metric=eval_metric, num_round=stepRounds,seed=mankraj_seed, tree_booster_params = list (eta=eta, gamma=gamma, min_child_weight = min_child_weight, subsample = subsample, tree_maker = tree_maker, tree_depth = depth),linear_booster_params = list (), model_in = 'xgboost.model', outputModel='xgboost.model', inputModel = NULL, silent = 0)
	k <- k+1

	system ("rm -f pred.txt")
	
	system (paste (xgboost_path, "/xgboost.conf > log.txt", sep = ""))
	system (paste (xgboost_path, "/xgboost",  "xgboost.conf model_in=xgboost.model  task=pred > log.txt", sep=""))
	prediction_valid <- read.csv ("pred.txt", stringsAsFactors=F, header=F)[,1]
        numRounds <- numRounds + stepRounds
        myAUC <- calc_auc (actual, prediction_valid[1:length(actual)])
	print (myAUC)
	aucArray <- c(aucArray, myAUC)
        if (myAUC <= bestAUC ) 
		break
	if (myAUC > bestAUC) {
		bestAUC <- myAUC
		bestETA <- eta
		bestGamma <- gamma
		bestChildWeight <- min_child_weight
		bestSubsample <- subsample
		bestTreemaker <- tree_maker
		bestDepth <- depth
		bestRounds <- numRounds 
		bestPrediction <- prediction_valid     
	}
    }
    }
    }
    }
    }
    }
    }
	save (bestETA, bestGamma, bestChildWeight, bestSubsample, bestTreemaker, bestDepth, bestRounds, aucArray, file = 'xgboostbest.rData')
	system ("rm -f pred.txt")

	return (bestPrediction)
   
}


runPyGBM <- function (cv_fold, train, test, pyGBMNms, prefix = "gbm_catincl_0.01_15", folderName = "pyfolds") {
	print (cv_fold)
	trainY <- train[train$indices != cv_fold, myTgt]
	actual <- train[train$indices == cv_fold, myTgt]
	mIndex <- 1:length (actual)
	trainFile <- paste ('trainXall', cv_fold, '.csv', sep='')
	validFile <- paste ('validXall', cv_fold, '.csv', sep='')
	testFile <- paste ('testXall', cv_fold, '.csv', sep='')
	trainYFile <-  paste ('trainY', cv_fold, '.csv', sep='')
	validYFile <- paste ('validY', cv_fold, '.csv', sep='')
	dumpFile <- paste ('gbm', cv_fold, '.pkl', sep='')
	outputFile <- paste ('gbm', cv_fold, '.csv', sep='')
	write.csv (train[train$indices != cv_fold,pyGBMNms], file = trainFile, row.names = F)
	write.csv (train[train$indices == cv_fold,pyGBMNms], file = validFile, row.names = F)
	write.csv (test[,pyGBMNms], file = testFile , row.names = F)
	write.table (train[train$indices != cv_fold,myTgt], file =trainYFile, row.names = F, col.names = F)
	write.table (train[train$indices == cv_fold,myTgt], file = validYFile, row.names = F, col.names = F)
	setwd (scikitLearnPath)
	system (paste ("python pyGBM.py ", trainFile, trainYFile, validFile, validYFile, testFile, "150 150 0.01 11 7 0", dumpFile, outputFile, "15000"))
	#system (paste ("python pyGBM.py ", trainFile, trainYFile, validFile, validYFile, testFile, "10 10 0.005 15 7 0", dumpFile, outputFile, "20"))
	setwd (current_working_dir)
	
	prediction <- as.numeric (read.csv (outputFile, header=F, stringsAsFactors = F)[,1])
	print (paste ('auc', calc_auc (actual, prediction [1:length(actual)])))
	writeBlendFiles (prediction, mIndex, prefix, folderName, cv_fold)
}

write.xgboost.params <- function (conf_file, booster_type, task, nthread = 1, trainFile, testFile, eval_metric="auc", num_round, seed, tree_booster_params, linear_booster_params, model_in=NULL, outputModel, inputModel = NULL, objective = "binary:logistic",  model_dir =".", use_buffer=0, silent = 1) {
	trainFile <- paste ('"', trainFile, '"', sep="")
	testFile <- paste ('"', testFile,'"', sep="")
	model_dir <- paste ('"', model_dir, '"', sep="")
    on.exit (sink())
    sink (conf_file)
    cat ("booster_type", booster_type, sep =" = "); cat ("\n")
    cat ("silent", silent, sep = " = ");     cat ("\n")
    if (booster_type == 0) {
        cat ("bst:eta", tree_booster_params$eta, sep = " = ");     cat ("\n")
        cat ("bst:gamma", tree_booster_params$gamma, sep = " = ");     cat ("\n")
        cat ("bst:min_child_weight", tree_booster_params$min_child_weight,sep =" = ");     cat ("\n")
        cat ("bst:subsample", tree_booster_params$subsample, sep = " = ");     cat ("\n")
        cat ("bst:tree_maker", tree_booster_params$tree_maker, sep = " = ");     cat ("\n")
	cat ("bst:max_depth", tree_booster_params$tree_depth, sep = " = "); cat("\n")
    }
    if (booster_type == 1) {
        cat ("bst:lambda", linear_booster_params$lambda);     cat ("\n")
        cat ("bst:alpha", linear_booster_params$alpha);     cat ("\n")
        cat ("bst:lambda_bias", linear_booster_params$lambda_bias);     cat ("\n")
    }
    cat ("objective", objective, sep = " = ");     cat ("\n")
    cat ("num_round", num_round, sep = " = ");     cat ("\n")
    cat ("data", trainFile, sep = " = ");     cat ("\n")
    cat ("test:data", testFile, sep = " = ");     cat ("\n")
    cat ("eval_metric", eval_metric, sep = " = ");     cat ("\n")
    cat ("save_period", 0, sep = " = "); cat ("\n")
    cat ("nthread", nthread, sep = " = "); cat ("\n")
    if (length(model_in)>0)  { 
		cat ("model_in", inputModel, sep = " = ");     cat ("\n") 
	}
	cat ("task", task, sep = " = "); cat ("\n")
    cat ("model_out", outputModel, sep = " = ");     cat ("\n")
    cat ("model_dir", model_dir, sep = " = ");     cat ("\n")
    cat ('use_buffer', use_buffer, sep = " = "); cat ("\n")
}
write.libsvm.file <- function (x, fileName = "out.dat", y = NULL) 
{
	library (data.table)
	library (Matrix)
	options (scipen = 999)
	#print ("Building feature man")
	library (Matrix)
#	x <- as (x, "dgCMatrix")

	rowNums <- x@i + 1
	tempCols <- x@p + 1
	temp_tempCols <- tempCols [2:length(tempCols)] - tempCols [1:length(tempCols)-1]
	colNums <- rep (1:length(temp_tempCols), temp_tempCols) 
	myVals <- x@x
	rm (tempCols, temp_tempCols)
	gc ()

	# order by rows
	myVals <- myVals [order (rowNums)]
	colNums <- colNums [order (rowNums)]
	rowNums <- sort (rowNums)
	
	# for each row 
	myDF <- data.frame (rowNums = rowNums, colNums = colNums, vals = myVals)
	#print ("Finished Creating the monster DF")
	rm (rowNums, colNums, myVals)
	gc ()
	myDF <- as.data.table (myDF)

	myDF <- myDF [,libsvm_col := paste (colNums, vals, sep = ":")]
	myDF <- myDF[,list(rowNums, libsvm_col)]
	#print ("created Libsvm column")
	myDF_new <- myDF[,lapply (.SD, paste, collapse = " "),by = rowNums]
	rm (myDF)
	gc ()
	myDF_new <- myDF_new [, output := as.character (y)]
	myDF_new <- myDF_new [,libsvm_row := paste (output, libsvm_col, sep = " ")]
	write.table (myDF_new[,list (libsvm_row)], file=fileName, row.names = F, col.names = F, quote=F)
	#print ("Finished")
}
# vowpal wabbit based feature selection => input to gbm
# vowpal wabbit based submission xxx
# SVD feature
# global_indices <- which (colnames (x) %in% glmnetNms)



write_svd_feature_file <- function (x, fileName = "out.dat", y = NULL, global_indices, user_indices, item_indices) 
{
	options (scipen = 999)
	
	rowNums <- x@i + 1
	tempCols <- x@p + 1
	temp_tempCols <- tempCols [2:length(tempCols)] - tempCols [1:length(tempCols)-1]
	colNums <- rep (1:length(temp_tempCols), temp_tempCols) 
	myVals <- x@x
	rm (tempCols, temp_tempCols); 	gc ()

	# order by rows
	myVals <- myVals [order (rowNums)]
	colNums <- colNums [order (rowNums)]
	rowNums <- sort (rowNums)
	
	# for each row 
	myDF <- data.frame (rowNums = rowNums, colNums = colNums, vals = myVals)
	rm (rowNums, colNums, myVals); 	gc ()
	setDT (myDF)
	myDF <- myDF [order (myDF$rowNums, myDF$colNums),]
	myDF [,type_indices := ifelse (colNums %in% global_indices, "global", ifelse (colNums %in% user_indices, "user", "item"))]
	myDF [,colNums := as.integer (colNums)]
	myDF [myDF$type_indices == "global",colNums := colNums - 1]
	myDF [myDF$type_indices ==  "user",colNums := colNums - length (global_indices) - 1]
	myDF [myDF$type_indices ==  "item",colNums := colNums - length (global_indices) - length(user_indices) - 1]
	myDF <- myDF [,libsvm_col := paste (colNums, vals, sep = ":")]
	myDF_new <- myDF[,list(rowNums, libsvm_col)]
	myDF_new <- myDF[,lapply (.SD, paste, collapse = " "),by = rowNums]
	myDF <- myDF [,list (num = nrow(.SD)),by = list (rowNums, type_indices)]
	myDF <- dcast.data.table (myDF, rowNums ~ type_indices, fun.aggregate = sum, value.var = "num")
	for (i in c("global", "user", "item"))
        	myDF [is.na(get(i)),i:=0,with=FALSE]

	setkey (myDF, rowNums)
	setkey (myDF_new, rowNums)
	myDF_new <- merge (myDF_new, myDF, by = "rowNums", all.x=TRUE)
	rm (myDF)
	gc ()
	myDF_new [, output := as.character (y)]
	myDF_new [, output := paste (output, global, user, item)]
	myDF_new [,libsvm_row := paste (output, libsvm_col, sep = " ")]
	write.table (myDF_new[,list (libsvm_row)], file=fileName, row.names = F, col.names = F, quote=F)
	#print ("Finished")
}


# vw-varinfo -q AB -c --passes 20 prot.dat
# !A
runVW_all <- function (myLossType = "logistic", vw_trainFile, vw_validFile,validRows, actual, flag_nn = 0, myPasses = 50, learnRateArray = c(0.001, 0.01, 0.1), hiddenNodeArray = c(1, 2, 4, 7, 11, 16, 25, 32), decayLearnRateArray = c(0.001, 0.01, 0.1, 0.05, 0.2 ),  myWeights=NULL, validIds=NULL, validWeights=NULL, flag_error_metric = "auc", l1Array = c(0), l2Array=c(0)) {
	bestAUC <- 0
	paramsDF <- NULL
	# find best Learning Rate
	for (myDecayLearningRate in decayLearnRateArray) {
		for (myLearningRate in learnRateArray) {
			for (l1reg in l1Array) {
			for (l2reg in l2Array) {
			system ("rm -f myTrain.vw.model")
			trainCmd <- paste (paste ('vw', sep = ''), "--loss_function",  myLossType, "-d", vw_trainFile, "-f myTrain.vw.model -l", myLearningRate, "--passes", myPasses, " -c -k", " -b26 ", "--decay_learning_rate", myDecayLearningRate, "--l1", l1reg, "--l2", l2reg, "--holdout_off", sep = " ")
			system (trainCmd)

#vw boston.data.vw --invert_hash boston.model
# vw boston.data.vw --readable_model boston.model
			system ("rm -f vwpredictions.txt")
			predCmd <- paste (paste ( 'vw', sep = ''), "-i myTrain.vw.model -t -d", vw_validFile, "-p vwpredictions.txt ", sep = " ")
			system (predCmd)
			myMtx <- read.csv ('vwpredictions.txt', stringsAsFactors= F, header=F)
			#myMtx <- matrix (data =myMtx$V1, nrow = validRows, ncol = myPasses)
			#prediction <- rowMeans (myMtx)
			prediction <- myMtx [,1]
			prediction <- 1/(1+exp(-prediction))
if (flag_error_metric == "auc")
			myAUC <- calc_auc (actual, prediction [1:length(actual)])

if (flag_error_metric == "d_g")  {
	solution <- data.frame (Id = validIds, Expected = actual, Weight = validWeights , stringsAsFactors = F)
	submission <- data.frame (Id = validIds, Predicted = prediction[1:length(actual)], stringsAsFactors = F)
	d <- TopFivePercentCaptured (solution, submission) #0.726
	g <- NormalizedWeightedGini (solution$Expected, validWeights, submission$Predicted) 
	myAUC <- 0.5 * (g+d)
        print (paste (d, g, myAUC))

}
if (flag_error_metric == "acc") {
        myAUC <- length (which (actual == prediction[1:length(actual)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mse") {
        myAUC <- mse (actual, prediction[1:length(actual)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmse") {
        myAUC <- rmse (actual, prediction[1:length(actual)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mae") {
        myAUC <- mae (actual, prediction[1:length(actual)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmsle") {
        myAUC <- rmsle (actual, prediction[1:length(actual)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmsle_exp") {
        myAUC <- rmsle (expm1(actual), expm1(prediction[1:length(actual)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mse_exp") {
        myAUC <- mse (expm1(actual), expm1(prediction[1:length(actual)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmse_exp") {
        myAUC <- rmse (expm1(actual), expm1(prediction[1:length(actual)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mae_exp") {
        myAUC <- mae (expm1(actual), expm1(prediction[1:length(actual)]))
        print (paste (myTrees, myAUC))
}
			print (paste (myLearningRate, myDecayLearningRate, myAUC))
			if (myAUC > bestAUC) {
				bestAUC <- myAUC
				bestLearningRate <- myLearningRate
				bestPrediction <- prediction
				bestDecayLearningRate <- myDecayLearningRate
				bestL1 <- l1reg
				

			}
				myDF <- data.frame (myAUC = myAUC, myLearningRate = myLearningRate, myDecayLearningRate = myDecayLearningRate, l1reg = l1reg, stringsAsFactors = F)
				paramsDF <- rbind (paramsDF, myDF)
				write.csv (paramsDF, file = 'vwParamsDF.csv', row.names = F)
			}
			}
		}		
	}
	print (paste('bestLearningRate' , bestLearningRate, 'bestDecayLearningRate' ,bestDecayLearningRate, 'bestL1', bestL1))
	save (bestLearningRate, bestDecayLearningRate, bestL1, file = 'vwParams.rData')
	if (flag_nn == 0) 
		return (bestPrediction)

	
}

create_vw_df <- function (myDT, numNms, facNms, myTgt, filename='vw.vw', importance=1) {
	# [label] [importance]
	setnames (myDT, myTgt, "target")
	myDT [,importance := importance]
	myDT [,vw_label_imp := paste (target, importance, sep = " ")]
	# String[:Value] for all numeric features
	temp <- myDT[,numNms,with=F]
	for (i in numNms)
		temp [,i:=paste (i, get(i), sep=":"),with=F]
	temp <- apply (as.data.frame (temp), 1, function (x) paste (x, collapse = " "))
	temp <- setDT (as.data.frame (temp))
	setnames (temp, c("numeric"))
	temp [,numeric := paste (" |numeric", numeric, sep = " ")]
	myDT [,numeric := temp$numeric]
	rm (temp)
	gc ()
	# Namespace Feature
	temp <- myDT[,facNms,with=F]
	for (i in facNms) {
		temp [,i:=gsub (" ", "_", get(i)),with=F]
		temp [,i:=paste (i, get(i), sep=" "),with=F]
	}	
	tempnew <- apply (as.data.frame (temp)[,-1], 1, function (x) paste (x, collapse = " |"))
	tempnew <- setDT (as.data.frame(tempnew))
	setnames (tempnew, c("charSpace"))
	temp [,charSpace := tempnew$charSpace]
	rm (tempnew)
	setnames (temp, facNms[1], "firstmank")
	temp [,firstmank := paste (" |", firstmank, sep="")]
	temp [,charSpace := paste (firstmank, charSpace, sep= " |")]
	myDT [,charSpace := temp$charSpace]
	rm (temp)
	
	myDT [,vw_string := paste (paste(vw_label_imp, numeric, sep=""), charSpace, sep=" ")]
	write.table (myDT$vw_string, file = filename, row.names = F, col.names = F, quote=F)
	
}

sparse_auc <- function (actual, myMtx.csc)
{
	# triplet format
	rowNums <- myMtx.csc@i + 1
	tempCols <- myMtx.csc@p + 1
	temp_tempCols <- tempCols [2:length(tempCols)] - tempCols [1:length(tempCols)-1]
	colNums <- rep (1:length(temp_tempCols), temp_tempCols)
	myVals <- myMtx.csc@x

	# rownums, colnums, myVals
    	n_pos <- sum(actual == 1)
    	n_neg <- length(actual) - n_pos
	n_mult <- as.double(n_pos) * as.double(n_neg)
	n_prod <- n_pos * (n_pos + 1)/2

	j <- 1
	start <- 1
	aucCols <- NULL
	while (j <= ncol (myMtx.csc))
	{
		end <- tempCols [j+1] - 1
		if (start <= end)
		{
			tempVals <- myVals [start:end]
			r <- rank (tempVals)
			myAct <- actual [rowNums[start:end]]
			offset <- ((nrow (myMtx.csc) -length(myAct)))
			mySum <- sum(r[myAct==1]+offset) + (offset+1) * (n_pos - sum(myAct))/2
			myAUC <- (mySum - n_prod)/n_mult
			start <- end+1
		}
		else
			myAUC <- 0
		aucCols <- c(aucCols, myAUC)
		j <- j+1
	}
    	return (aucCols)   
}

# Normalize a sparse matrix by dividing each value by its maximum

# takes in a csc matrix
# returns a csc matrix
RangeNormalize <- function (myMtx.csc, myMtx_valid.csc) {
	#myMtx.csc <- as (as.matrix(myMtx), "sparseMatrix")
	myList <- list()

	# triplet format
	rowNums <- myMtx.csc@i + 1
	tempCols <- myMtx.csc@p + 1
	temp_tempCols <- tempCols [2:length(tempCols)] - tempCols [1:length(tempCols)-1]
	colNums <- rep (1:length(temp_tempCols), temp_tempCols) 
	myVals <- myMtx.csc@x

	print ('Finding Max')
	# maximum value in each column
	maxCols <- rep (0, ncol(myMtx.csc))
	
	mySortVals <- myVals [order(colNums)]
	mySortCols <- colNums [order (colNums)]
	zeroCols <- which (colNums %notin% c(1:ncol(myMtx.csc)))
	zeroVals <- rep (0, length (zeroCols))
	mySortVals <- c(mySortVals, zeroVals)
	mySortCols <- c(mySortCols, zeroCols)

	myDF <- data.frame (cols = mySortCols, vals = mySortVals)
	myDF <- myDF [order(myDF$cols),]
	library (data.table)
	myDF <- as.data.table (myDF)
	maxCols <- as.data.frame (myDF [,list (vals = max (.SD)),by = cols])[,2]
	print ('found Max')

	# divide each value by the maximum value of the column
	myVals <- myVals/ (maxCols[colNums])

	myMtx.csc@x <- myVals

	myVals_valid <- myMtx_valid.csc@x
	tempCols <- myMtx_valid.csc@p + 1
	temp_tempCols <- tempCols [2:length(tempCols)] - tempCols [1:length(tempCols)-1]
	colNums <- rep (1:length(temp_tempCols), temp_tempCols) 
	myVals_valid <- myVals_valid /(maxCols [colNums])


	myList[[1]] <- myMtx.csc
	myList [[2]] <- myMtx_valid.csc
	return (myList)
}



ensemble_create_file_new <- function (folderName, mankraj_number_of_folds, flag_train=1, debugMode = 0, start_fold = 1) {
	library (stringr)
	xtrain <- NULL
	xtest <- NULL
	for (ff in start_fold:mankraj_number_of_folds)
	{
		print (paste ('for Fold:', ff))
		if (flag_train == 1) {
			xmat <- read.table(file = paste(folderName, "/actual_fold_",ff,'.csv', sep = ""), header = TRUE, sep = ",")
			names (xmat) [1] <- "actual"
			xid <- read.table(file = paste(folderName, "/trainidentifiers_fold_",ff,'.csv', sep = ""), header = TRUE, sep = ",", row.names = NULL) 
			xmat$id <- xid[,1]
			flist <- dir(paste(folderName, "/", sep=""), pattern = glob2rx(paste("modelValid*fold_",ff,".csv", sep = "")), full.names = TRUE)
		}
		if (flag_train == 0) {
			xmat <- read.table(file = paste(folderName, "/ids_fold_",ff,'.csv', sep = ""), header = TRUE, sep = ",") 
			flist <- dir(paste(folderName, "/", sep=""), pattern = glob2rx(paste("modelFull*fold_",ff,".csv", sep = "")), full.names = TRUE)
		}		
		for (ii in seq(flist))
		{
			fname <- paste (str_split(flist[ii], "_")[[1]][-c(1,length(str_split(flist[ii], "_")[[1]]))], collapse="_")
			print (fname)
			fname <- gsub ("_fold", "", fname)
			xf <- read.table(file = flist[ii], header = TRUE, sep = ",")
			colnames(xf) <- fname
			if (debugMode == 1)  print (fname)
			xmat <- data.frame(xmat, xf)
			rm(fname, xf)
		}
		xmat$cv_fold <- ff

		#names (xmat) <- gsub (paste ("_", ff, sep = ""), "", names (xmat))

		
		print (names (xmat))
		

		if (flag_train == 1) {
			print (names (xtrain))
			print (paste ("folderName ff nrow (xtrain), ncol(xtrain, nrow (xmat), ncol(xmat)", folderName, ff, nrow (xtrain),nrow (xmat), ncol(xtrain), ncol(xmat)))
			
			print ('before error')
			print ('missing in train')
			print (names (xtrain)[names (xtrain) %notin% names (xmat)])
			print ('missing in mat')
			print (names (xmat)[names (xmat) %notin% names (xtrain)])
			xtrain <- rbind(xtrain, xmat)
		}
		if (flag_train == 0) {
			print (names (xtest))
			print (paste ("folderName ff dim (xtest),dim (xmat)", folderName, ff, dim (xtest),dim (xmat)))

		
			print ('missing in test')
			print (names (xtest)[names (xtest) %notin% names (xmat)])
			print ('missing in mat')
			print (names (xmat)[names (xmat) %notin% names (xtest)])
			xtest <- rbind (xtest, xmat)
		}
	
	}
	if (flag_train == 1)
		return (xtrain)
	if (flag_train == 0) {
		names (xtest) [1] <- "id"
		return (xtest)
	}
}	




writeBlendFiles <- function (prediction, mIndex, model0, folder, cv_fold) {
	# xf is predictions for valid and xt is predictions for test
	xf <- data.frame (label = prediction [mIndex] )
	xt <- data.frame (label = prediction[-mIndex])			
	name0 <- paste(paste (folder, "modelValid_", sep="/"), model0, "_fold_", cv_fold, ".csv", sep = "")
	write.table(xf, file = name0, row.names = FALSE, col.names= TRUE, sep = ",")
	name0 <- paste(paste (folder, "modelFull_", sep="/"), model0, "_fold_", cv_fold, ".csv", sep = "")
	write.table(xt, file = name0, row.names = FALSE, col.names= TRUE, sep = ",")
}

runGBM_all <- function (myTrain, myTest, myScore, trainY, myInteractionDepth, myShrinkage, myMinObsInNode, actual_new, myTrees, myMaxTrees, myDistribution = "bernoulli", minImprovement = 0.002, flag_error_metric = "auc", myWeights=NULL, validIds=NULL, validWeights=NULL, myVerbose = TRUE, cv_fold, bag.fraction) {
	library (gbm)
	library (Metrics)


        bestAUC <- 0
        bestTrees <- 0
	myGBM <- gbm.fit (x = myTrain, y = trainY, distribution = myDistribution, n.trees = myTrees, n.minobsinnode = myMinObsInNode, shrinkage = myShrinkage, interaction.depth = myInteractionDepth, verbose=myVerbose, w = myWeights, bag.fraction=bag.fraction)
        myTrees <- myTrees
        treeStep <- myTrees
	
        prediction <- predict (myGBM, myTest, type = "response", n.trees= myTrees)
if (flag_error_metric == "auc") {
        myAUC <- calc_auc (actual_new, prediction[1:length(actual_new)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "acc") {
        myAUC <- length (which (actual_new == prediction[1:length(actual_new)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mse") {
        myAUC <- mse (actual_new, prediction[1:length(actual_new)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmse") {
        myAUC <- rmse (actual_new, prediction[1:length(actual_new)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mae") {
        myAUC <- mae (actual_new, prediction[1:length(actual_new)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmsle") {
        myAUC <- rmsle (actual_new, prediction[1:length(actual_new)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmsle_exp") {
        myAUC <- rmsle (expm1(actual_new), expm1(prediction[1:length(actual_new)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mse_exp") {
        myAUC <- mse (expm1(actual_new), expm1(prediction[1:length(actual_new)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmse_exp") {
        myAUC <- rmse (expm1(actual_new), expm1(prediction[1:length(actual_new)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mae_exp") {
        myAUC <- mae (expm1(actual_new), expm1(prediction[1:length(actual_new)]))
        print (paste (myTrees, myAUC))
}

if (flag_error_metric == "d_g") {
	solution <- data.frame (Id = validIds, Expected = actual_new, Weight = validWeights , stringsAsFactors = F)
	submission <- data.frame (Id = validIds, Predicted = prediction[1:length(actual_new)], stringsAsFactors = F)
	d <- TopFivePercentCaptured (solution, submission) #0.726
	g <- NormalizedWeightedGini (solution$Expected, validWeights, submission$Predicted) 
	myAUC <- 0.5 * (g+d)
        print (paste (myTrees, d, g, myAUC))
}
        while (1)       {       
		
		if (!is.na (myAUC)) {
		        if (myAUC - bestAUC) {
		                bestAUC <- myAUC 
		                bestPrediction <- prediction
		                bestTrees <- myTrees
				bestGBM <- myGBM
		        }
		}
		if (is.na(myAUC)) break
                myTrees <- myTrees + treeStep
		if (myTrees > myMaxTrees) break
                myGBM <- gbm.more (myGBM, n.new.trees = treeStep, w =myWeights)
                prediction <- predict (myGBM, myTest, type = "response", n.trees= myTrees)
                lastAUC <- myAUC
if (flag_error_metric == "auc") {
                myAUC <- calc_auc (actual_new, prediction[1:length(actual_new)])
                print (paste (myTrees, myAUC))   
}
if (flag_error_metric == "d_g") {
		solution <- data.frame (Id = validIds, Expected = actual_new, Weight = validWeights , stringsAsFactors = F)
		submission <- data.frame (Id = validIds, Predicted = prediction[1:length(actual_new)], stringsAsFactors = F)
		d <- TopFivePercentCaptured (solution, submission) #0.726
		g <- NormalizedWeightedGini (solution$Expected, validWeights, submission$Predicted) 
		myAUC <- 0.5 * (g+d)
        	print (paste (myTrees, d, g, myAUC))
}
if (flag_error_metric == "acc") {
        myAUC <- length (which (actual_new == prediction[1:length(actual_new)]))
        print (paste (myTrees, myAUC))
}

if (flag_error_metric == "mse") {
        myAUC <- mse (actual_new, prediction[1:length(actual_new)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmse") {
        myAUC <- rmse (actual_new, prediction[1:length(actual_new)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mae") {
        myAUC <- mae (actual_new, prediction[1:length(actual_new)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmsle") {
        myAUC <- rmsle (actual_new, prediction[1:length(actual_new)])
        print (paste (myTrees, myAUC))
}

if (flag_error_metric == "rmsle_exp") {
        myAUC <- rmsle (expm1(actual_new), expm1(prediction[1:length(actual_new)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mse_exp") {
        myAUC <- mse (expm1(actual_new), expm1(prediction[1:length(actual_new)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmse_exp") {
        myAUC <- rmse (expm1(actual_new), expm1(prediction[1:length(actual_new)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mae_exp") {
        myAUC <- mae (expm1(actual_new), expm1(prediction[1:length(actual_new)]))
        print (paste (myTrees, myAUC))
}


	
              
                if (myAUC < bestAUC) break
		gc ()
		
        }
 
	save (bestGBM,bestPrediction , bestTrees, file = paste ('kiranGBM', cv_fold, '.rData', sep=''))
        return (bestPrediction)
}



`%notin%` <- function(x,y) !(x %in% y)
calc_auc <- function (actual, predicted)
{
    r <- rank(predicted)
    n_pos <- as.numeric (sum(actual == 1))
    n_neg <- as.numeric (length(actual) - n_pos)
    denom <- as.double (as.double (n_pos) *         as.double(n_neg))
    auc <- (sum(r[actual == 1]) - n_pos * (n_pos + 1)/2)/(denom)
    auc
}


runGLMNET_d_g <- function (myTrain, myValid, myTest, trainY, actual, validY, myMetric = "auc",  alphaArray = c(0.1, 0.25, 0.5, 0.75, 1),validIds=NULL, validWeights=NULL) {
	library (glmnet)
	bestAUC <- 0
	print (dim(myTrain))
	trainNms <- Matrix::colSums (myTrain)
	validNms <- Matrix::colSums (myValid)
	testNms <- Matrix::colSums (myTest)
	trainNms <- colnames (myTrain) [which (trainNms > 0)]
	validNms <- colnames (myValid) [which (validNms > 0)]
	testNms <- colnames (myTest) [which (testNms > 0)]
	commonNms <- intersect (trainNms, validNms)
	commonNms <- intersect (commonNms, testNms)
	myTrain <- myTrain [,commonNms]
	myValid <- myValid [,commonNms]
	myTest <- myTest [,commonNms]
	rm (myTest)
	print (dim(myTrain))
	

	for (myAlpha in alphaArray) {
		myGLMNET <- glmnet (x=myTrain, y= as.factor (trainY), family = "binomial", alpha = myAlpha)
		triedLambda <- myGLMNET$lambda		
		mySeq <- c(triedLambda[length(triedLambda)], triedLambda[length(triedLambda)-10], triedLambda[length(triedLambda)-25], triedLambda [length(triedLambda)-50],triedLambda [length(triedLambda)-100], triedLambda[round(length(triedLambda)/2)], triedLambda[round(length(triedLambda)*2/3)], triedLambda[round(length(triedLambda)*3/4)], triedLambda[round(length(triedLambda)*4/5)])
		predictionsMtx <- predict (myGLMNET, myValid, type = "response", s=mySeq)
		k <- 1
		for (myLambda in mySeq) {
			prediction <- predictionsMtx[,k]

			if (myMetric == "d_g") {
				solution <- data.frame (Id = validIds[1:length(actual)], Expected = actual, Weight = validWeights[1:length(actual)] , stringsAsFactors = F)
				submission <- data.frame (Id = validIds[1:length(actual)], Predicted = prediction[1:length(actual)], stringsAsFactors = F)
				d <- TopFivePercentCaptured (solution, submission) #0.726
				g <- NormalizedWeightedGini (solution$Expected, validWeights, submission$Predicted) 
				myAUC <- 0.5 * (g+d)
			}
			if (myMetric == "auc") 
				myAUC <- calc_auc (actual, prediction[1:length(actual)])			
			if (myAUC > bestAUC) {
				print (myAUC)
				bestAUC <- myAUC
				bestLambda <- myLambda
				bestPrediction <- prediction
				bestAlpha <- myAlpha
				bestGLMNET <- myGLMNET
			}
			k <- k+1
		}
			print (myAUC)
	}

	myList <- list ()
	myList [[1]] <- bestGLMNET
	myList [[2]] <- bestAlpha
	myList [[3]] <- bestLambda
	myList [[4]] <- commonNms
	myList [[5]] <- bestPrediction
	print (paste ('bestAlpha', bestAlpha, 'bestLambda', bestLambda))
	return (myList)
}




makeSparseMtxFromFactorCommon <- function (train, test, factorNms) {
	library (Matrix)
	k <- 1

	train <- as.data.frame (train)
	test <- as.data.frame (test)
	for (varName in factorNms) {
		commonLevels <- unique (intersect (train[,varName], test[,varName]))
		train [,varName] <- ifelse (train[,varName] %in% commonLevels, train[,varName], "other")
		test [,varName] <- ifelse (test[,varName] %in% commonLevels, test[,varName], "other")
	}
	setDT (train)
	setDT (test)

	train <- train[,factorNms,with=F][,lapply(.SD, as.factor)]
	test <- test[,factorNms,with=F][,lapply(.SD, as.factor)]

	big_train <- rbind (train[,factorNms,with=F], test [,factorNms,with=F])
	big_train <- big_train [,lapply(.SD, factor)]
	big_train <- as.data.frame (big_train)

	for (varName in factorNms) {
		fMtx <- as (big_train [,varName], "sparseMatrix")
		rownames (fMtx) <- paste (varName, rownames(fMtx), sep = "_")
		fMtx <- t(fMtx)

		if (k==1) bigSparseMtx <- fMtx
		else 	bigSparseMtx <- cBind (bigSparseMtx, fMtx)
		k <- k+1
	}
	return (bigSparseMtx)
}
makeSparseMtxFromFactor <- function (big_train, factorNms) {
	library (Matrix)
	k <- 1
	for (varName in factorNms) {
		fMtx <- as (big_train [,varName], "sparseMatrix")
		rownames (fMtx) <- paste (varName, rownames(fMtx), sep = "_")
		fMtx <- t(fMtx)

		if (k==1) bigSparseMtx <- fMtx
		else 	bigSparseMtx <- cBind (bigSparseMtx, fMtx)
		k <- k+1
	}
	return (bigSparseMtx)
}

# Returns the # of missing values in a vector
countNA <- function (x){ 
length (which(is.na(x)==TRUE))/length (x)
}

countInf <- function (x){ 
length (which(x==Inf))/length (x)
}


generateStratifiedRandomSample <- function (temp_train, mankraj_number_of_folds, num_levels, stratifyVariable) {
	  for (m in 0:num_levels) {	# generate a stratified sample 
	    indices <- rep ( c(1:mankraj_number_of_folds), each = nrow(temp_train[temp_train[,stratifyVariable]==m,])/mankraj_number_of_folds)
	    indices <- c(indices, rep ( 1, each = nrow(temp_train[temp_train[,stratifyVariable]==m,])-length (indices)))
	    set.seed (mankraj_seed)
	    temp_train[temp_train[,stratifyVariable]==m,"indices"] <- sample (indices, length(indices))
	  }
	return (temp_train)
  }


replaceNA <- function(DT, myNms = names (DT), val=-1) {
    for (i in myNms)
        DT[is.na(get(i)),i:=val,with=FALSE]
	
    return (DT)
}
specificCreateDictionary <- function (files, num_files) 
{
	words <- c ("hello")
	for (k in 1:num_files) {
		temp <- scan(files[k], what = character())
		words <-c(words, temp)
	}
	words <- strip.text(words)
	counts <- table(words)
	dictionary <- names (counts)

	return (dictionary)
}


# Word Lengths
# Parts of speech for different title, description, need_statement and essay of the student
my_parts_of_speech <- function (s) {
	
	s <- removeBlank (s)
if (nchar (s) < 2) {
	myDF <- data.frame (tags = c("num_sent", "num_chars_in_sent", "num_words", "num_chars_in_word"), Freq = c(0, 0, 0, 0), stringsAsFactors = F)
   	return (myDF)
}


	## Need sentence and word token annotations.
	sent_token_annotator <- Maxent_Sent_Token_Annotator()
	word_token_annotator <- Maxent_Word_Token_Annotator()
	a2 <-annotate(s, list( sent_token_annotator, word_token_annotator))
	pos_tag_annotator <- Maxent_POS_Tag_Annotator()
	a3 <- annotate(s, pos_tag_annotator, a2)
	a3 <- subset(a3, type == "word")
	tags <- sapply(a3$features, `[[`, "POS")

	myDF <- data.frame (table (tags))
	myDF$tags <- as.character (myDF$tags)
	temp <- setDT (as.data.frame (a2))
	temp [,len := end - start]
	num_sentences <- nrow (temp [temp$type == "sentence",])
	num_chars_in_sentence <- sum (temp [temp$type == "sentence", "len",with=F][,len])/num_sentences
	num_words <- nrow (temp [temp$type == "word",])
	num_chars_in_word <- sum (temp [temp$type == "word", "len",with=F][,len])/num_words 
	myDF <- rbind (myDF, data.frame (tags = c("num_sent", "num_chars_in_sent", "num_words", "num_chars_in_word"), Freq = c(num_sentences, num_chars_in_sentence, num_words, num_chars_in_word),stringsAsFactors = F))
	#if (k==1000) print (k)
	#k<<-k+1
   	return (myDF)
}          




my_parts_of_speech_parallel <- function (x, myfold, myVar) {
	k <<- 1
	lapply (x[x$myfold == myfold, myVar], my_parts_of_speech)
}
library (stringr)
removeBlank <- function (myVar) {
	myVar <- gsub ("[[:space:]]+", " ", myVar)
	myVar <- str_trim (myVar)
	return (myVar)
}


runLIBFMPar <- function (trainFile, validFile, metaFile = NULL, type = 'c', trainY, actual, stdevArray = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75),numVectorArray = c(1, 2, 4, 7, 11, 16), numiter = 205, testFlag  = 0, testFile = 'test.libfm', outFile = 'out.csv', cv_fold, prefix, folder ) {
	
	method <- "mcmc"; 	task <- paste ("-task", type, sep = ' '); 	myMethod <- paste ("-method", "mcmc", sep=" "); 	verbosity <- paste ("-verbosity 0"); 	out <- paste("-out", outFile)
	myTrainFile <- paste ("-train", trainFile, sep=" "); 	myValidFile <- paste ("-test", validFile, sep=" "); 	iter <- paste ("-iter", numiter, sep=" "); 	bestAUC <- 0
	aucs <- NULL; 	stdevs <- NULL; 	vectors <- NULL
	# find best stdev and numvectors combo
	for (stdev in stdevArray) {
		for (numvectors in numVectorArray) {
			init_stdev <- paste ("-init_stdev", stdev, sep=" ")
			dims <- paste ("-dim '1,1,", numvectors, "'", sep="")
			system (		paste (libfm_command, task, myTrainFile, myValidFile, dims, iter, myMethod, init_stdev, verbosity, out)			)
			tbl <- read.csv (outFile, header=FALSE)
			prediction <- tbl$V1
			pred <- prediction[1:length(actual)]
			myAUC <- calc_auc (actual[which(!is.na(actual))], pred[which(!is.na(actual))])
			print (paste('myAUC', stdev, myAUC))
			if (myAUC > bestAUC) {
				bestAUC <- myAUC
				bestPrediction <- prediction
				bestStdev <- init_stdev
				bestVectors <- dims
				best_stdev <- stdev
				best_vectors <- numvectors
			}
			aucs <- c(aucs, myAUC)
			stdevs <- c(stdevs, stdev)
			vectors <- c(vectors, numvectors)
			print (aucs)
			print (stdevs)
			print (vectors)
		}
	}
	print (paste("Best Stdev", bestStdev))
	print (paste("Best Vectors", bestVectors))
	save (bestVectors, bestStdev, myTrainFile, task, bestVectors, iter, myMethod, verbosity, out, file = paste ('bestLIBFMParams', cv_fold, '.rData',sep=''))
	save (best_stdev, best_vectors, file = paste ('bestLIBFM', cv_fold, '.rData',sep=''))
	print (calc_auc (actual[which(!is.na(actual))], bestPrediction[1:length(actual)][which(!is.na(actual))]))
	if (testFlag == 1) {
		myTestFile <- paste ("-test", testFile, sep=" ")
		system (		paste (libfm_command, task, myTrainFile, myTestFile, bestVectors, iter, myMethod, bestStdev, verbosity, out)			)
		tbl <- read.csv (paste("mankoutput", 1, ".txt", sep=""), header=FALSE)
		bestPrediction_test <- tbl$V1
		bestPrediction <- c(bestPrediction ,bestPrediction_test)
	}
	writeBlendFiles (bestPrediction, mIndex, prefix, folder, cv_fold) 
	return (cv_fold)
}


nnet_auc_ensemble <- function (xtrain, xtest, myNms, myTgt, ignoreFold = NULL, flag_ignore=0, sizeVector=c(1)) {
	if (flag_ignore == 1) {
		xtrain <- xtrain[xtrain$cv_fold != ignoreFold,]
		xtest <- xtest [xtest$cv_fold != ignoreFold,]
	}

	# settings for the training process
	set.seed(1); 
	myForm <- as.formula (paste (myTgt, paste (myNms, collapse= "+", sep=" "), sep = "~"))
	bigxf <- NULL
	bigxt <- NULL
	k <- 1
		
	for (cv_fold in 1:mankraj_number_of_folds) {
		print (paste ("For Fold", cv_fold))
		tempTest <- rbind (xtrain [xtrain$cv_fold == cv_fold, myNms], xtest[,myNms])
		tempTrain <- xtrain [xtrain$cv_fold != cv_fold, ]
		actual <- xtrain[xtrain$cv_fold == cv_fold, myTgt]
		mIndex <- 1:length (actual)
		tempTrain[,myTgt] <- as.factor (tempTrain[,myTgt])
	
		trainY <- xtrain [xtrain$cv_fold != cv_fold,myTgt]
		validY <- rep (0, length(which(xtrain$cv_fold == cv_fold))+nrow(xtest))
		{
			if (cv_fold == 1)
				prediction <- runNNET (tempTrain, tempTest, actual, trainY, myTgt, myForm, sizeVector)
			if (cv_fold != 1) {
				load ('bestnnetparams.rData')
				prediction <- runNNET (tempTrain, tempTest, actual, trainY, myTgt, myForm, sizeVector=c(bestSize), decayVector = c(bestDecay))
			}

			print (paste ("auc", calc_auc (actual, prediction [1:length(actual)])))
			xf <- data.frame (label = prediction [mIndex] )
			xt <- data.frame (label = prediction[-mIndex])	
			xf$cv_fold <- cv_fold
			xt$cv_fold <- cv_fold
			xf[,myTgt] <- actual
			if (k==1) {
				bigxf <- xf
				bigxt <- xt					
			}

			else {
				bigxf <- rbind (bigxf, xf)
				bigxt <- rbind (bigxt, xt)
			}
			k <- k+1
			gc()
		}
	}
	myXT <- bigxt [which(bigxt$cv_fold == 1),"label"]
	for (kk in 2:mankraj_number_of_folds) {
		myXT <- cbind (myXT, bigxt [which (bigxt$cv_fold == kk),"label"])
	}

	myPred <- rowSums (myXT)/mankraj_number_of_folds
	myPred_valid <- bigxf$label

	myList <- list ()
	myList [[1]] <- myPred
	myList [[2]] <- myPred_valid
	return (myList)
}


#c(1, 2, 3, 4, 7)
runNNET <- function (myTrain, myValid, actual, trainY, myTgt, myForm,  sizeVector = c(1),decayVector = c ( 1e-3, 0.5, 1, 1.5, 2)) { 
	
	
	bestAUC <- 0
	
	for (mySize in sizeVector) {
		for (myDecay in decayVector) {
			myNNET <- nnet (as.formula (myForm), data = myTrain, MaxNWts = 1e7, decay = myDecay, na.action = na.omit, size= mySize, maxit = 10^5, trace = TRUE, logistic= TRUE, softmax = FALSE, abstol = 1e-4)
			prediction <- predict (myNNET, newdata = myValid , type= "raw")[,1]		
			myAUC <- calc_auc (actual, prediction[1:length(actual)])
			if (length(myAUC) != 0 ) {
			if (!is.na(myAUC)) {
			if (myAUC > bestAUC) {
				bestAUC <- myAUC
				bestSize <- mySize
				bestDecay <- myDecay
				bestPrediction <- prediction
			}
			}
			}
			print (paste ('Neural Network', mySize, myDecay, myAUC, 'bestAUC' ,bestAUC))
			save (bestDecay, bestSize, file = 'bestnnetparams.rData')
		}
	}	
	return (bestPrediction)
}


KorpusCount <- function (x)  {
	myTbl <- table (x@TT.res$wclass)


	return (c (x@desc$words, x@desc$lines, x@desc$normalized.space, x@desc$chars.no.space,
x@desc$punct, x@desc$digits, x@desc$letters.only, x@desc$words, x@desc$sentences, x@desc$avg.sentc.length, x@desc$avg.word.length
,ifelse (length(myTbl[names (myTbl) %in% "noun"]) > 0, myTbl[names (myTbl) %in% "noun"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "pronoun"]) > 0, myTbl[names (myTbl) %in% "pronoun"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "verb"]) > 0, myTbl[names (myTbl) %in% "verb"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "adverb"]) > 0, myTbl[names (myTbl) %in% "adverb"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "conjunction"]) > 0, myTbl[names (myTbl) %in% "conjunction"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "interjection"]) > 0, myTbl[names (myTbl) %in% "interjection"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "preposition"]) > 0, myTbl[names (myTbl) %in% "preposition"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "adjective"]) > 0, myTbl[names (myTbl) %in% "adjective"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "possessive"]) > 0, myTbl[names (myTbl) %in% "possessive"], 0)
,ifelse (length(myTbl[names (myTbl) %in% c("determiner", "predeterminer")]) > 0, myTbl[names (myTbl) %in% c("determiner", "predeterminer")], 0)
,ifelse (length(myTbl[names (myTbl) %in% "modal"]) > 0, myTbl[names (myTbl) %in% "modal"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "comma"]) > 0, myTbl[names (myTbl) %in% "comma"], 0)
,ifelse (length(myTbl[names (myTbl) %in% "particle"]) > 0, myTbl[names (myTbl) %in% "particle"], 0)
))
}



create_vw_input_files <- function (vwTrainFile, vwValidFile, trainY, myTrain, myValid, validY, flagOldMethod = 1 ) {
	options ("scipen"=100, "digits"=4)
	trainFile <- "tempT.libsvm"
	validFile <- "tempV.libsvm"
	# Create libsvm files 
	if (flagOldMethod == 0) {
		AnnaBond (x=as (myTrain, "matrix.csr"), file = trainFile, y = trainY)
		AnnaBond (x=as (myValid, "matrix.csr"), file = validFile, y = validY)
	}
	if (flagOldMethod == 1) {
		write_libsvm_file (x=myTrain, file = trainFile, y = trainY)
		write_libsvm_file (x=myValid, file = validFile, y = validY)
	}

	# Create vw files
	trainFileCmd <- paste ("perl -pe 's/\\s/ | /' ", trainFile, ">", vwTrainFile, sep = " ")
	system (trainFileCmd)
	testFileCmd <- paste ("perl -pe 's/\\s/ | /' ", validFile, ">", vwValidFile, sep = " ")
	system (testFileCmd)
	system (paste ("rm -f ", trainFile, validFile))
}
# vw-varinfo -q AB -c --passes 20 prot.dat
# !A
# vw-varinfo -q AB -c --passes 20 prot.dat
# !A
runVW_all <- function (myLossType = "logistic", vw_trainFile, vw_validFile,validRows, actual, flag_nn = 0, myPasses = 50, learnRateArray = c(0.001, 0.01, 0.1), hiddenNodeArray = c(1, 2, 4, 7, 11, 16, 25, 32), decayLearnRateArray = c(0.001, 0.01, 0.1, 0.05, 0.2 ),  myWeights=NULL, validIds=NULL, validWeights=NULL, flag_error_metric = "auc", l1Array = c(0), l2reg=0) {
	bestAUC <- 0
	# find best Learning Rate
	for (myDecayLearningRate in decayLearnRateArray) {
		for (myLearningRate in learnRateArray) {
			for (l1reg in l1Array) {
			trainCmd <- paste (paste ('vw', sep = ''), "--loss_function",  myLossType, "-k", "-d", vw_trainFile, "-f model1 -l", myLearningRate, "--passes", myPasses, " --quiet -c ", "--decay_learning_rate", myDecayLearningRate, "--l1", l1reg, "--l2", l2reg, "--holdout_off", sep = " ")
			system (trainCmd)

#vw boston.data.vw --invert_hash boston.model
# vw boston.data.vw --readable_model boston.model
			predCmd <- paste (paste ( 'vw', sep = ''), "-i model1 -t -d", vw_validFile, "-p vwpredictions.txt -l ", myLearningRate,  "--quiet", sep = " ")
			system (predCmd)
			prediction <- read.csv ('vwpredictions.txt', stringsAsFactors= F, header=F)[,1]
			#myMtx <- matrix (data =myMtx$V1, nrow = validRows, ncol = myPasses)
		
			#prediction <- rowMeans (myMtx)
			prediction <- 1/(1+exp(-prediction))
if (flag_error_metric == "auc")
			myAUC <- calc_auc (actual, prediction [1:length(actual)])

if (flag_error_metric == "d_g")  {
	solution <- data.frame (Id = validIds, Expected = actual, Weight = validWeights , stringsAsFactors = F)
	submission <- data.frame (Id = validIds, Predicted = prediction[1:length(actual)], stringsAsFactors = F)
	d <- TopFivePercentCaptured (solution, submission) #0.726
	g <- NormalizedWeightedGini (solution$Expected, validWeights, submission$Predicted) 
	myAUC <- 0.5 * (g+d)
        print (paste (d, g, myAUC))

}
if (flag_error_metric == "acc") {
        myAUC <- length (which (actual == prediction[1:length(actual)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mse") {
        myAUC <- mse (actual, prediction[1:length(actual)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmse") {
        myAUC <- rmse (actual, prediction[1:length(actual)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mae") {
        myAUC <- mae (actual, prediction[1:length(actual)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmsle") {
        myAUC <- rmsle (actual, prediction[1:length(actual)])
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmsle_exp") {
        myAUC <- rmsle (expm1(actual), expm1(prediction[1:length(actual)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mse_exp") {
        myAUC <- mse (expm1(actual), expm1(prediction[1:length(actual)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "rmse_exp") {
        myAUC <- rmse (expm1(actual), expm1(prediction[1:length(actual)]))
        print (paste (myTrees, myAUC))
}
if (flag_error_metric == "mae_exp") {
        myAUC <- mae (expm1(actual), expm1(prediction[1:length(actual)]))
        print (paste (myTrees, myAUC))
}
			print (paste (myLearningRate, myDecayLearningRate, myAUC))
			if (myAUC > bestAUC) {
				bestAUC <- myAUC
				bestLearningRate <- myLearningRate
				bestPrediction <- prediction
				bestDecayLearningRate <- myDecayLearningRate
				bestL1 <- l1reg
			}
			}
		}		
	}
	print (paste('bestLearningRate' , bestLearningRate, 'bestDecayLearningRate' ,bestDecayLearningRate, 'bestL1', bestL1))
	save (bestLearningRate, bestDecayLearningRate, bestL1, file = 'vwParams.rData')
	bestPrediction <- bestPrediction
	bestPrediction <- 1/(1+exp(-bestPrediction))
	if (flag_nn == 0) 
		return (bestPrediction)

	
}

