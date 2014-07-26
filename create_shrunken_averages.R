
#############################################################################################
##########################DAMPED FEATURES ##################################################
###############################################################################################
# get the damped averages in place for time based CV
# Damp feature engineering
{
	train [,indices := indices + 1]

	train_bkp <- copy (train); test_bkp <- copy (test)
	for (i in facNms) {
	    train [,i:=as.character (get(i)),with=FALSE]
	    test [,i:=as.character (get(i)), with = FALSE]
	}
	dampDF <- NULL; 	dampDF_test <- NULL
	for (cv_fold in c(1:mankraj_number_of_folds)) {
		print (cv_fold)
	    	myList <- fn_addDampAvg ( train = as.data.frame (train[train$indices != cv_fold, c(facNms, "id", myTgt),with=F]), valid  = as.data.frame (rbind (train[train$indices == cv_fold, c(facNms, "id", myTgt),with=F],test[,c(facNms, "id", myTgt),with=F])), facNms, myTgt)
		myList[[1]]$cv_fold <- cv_fold
		myList[[2]]$cv_fold <- cv_fold
	  	dampDF <- rbind (dampDF, myList [[2]] [c(1:length(which(train$indices == cv_fold))),])
		dampDF_test <- rbind (dampDF_test, myList [[2]] [-c(1:length(which(train$indices == cv_fold))),])
		print (table (dampDF$cv_fold))

		rm (myList); gc ()
	}
	cv_fold <- mankraj_number_of_folds +1
	myList <- fn_addDampAvg ( train = as.data.frame (train[, c(facNms, "id", myTgt),with=F]), valid  = as.data.frame (test[,c(facNms, "id", myTgt),with=F]), facNms, myTgt)
	myList [[2]]$cv_fold <- cv_fold
	dampDF_test <- rbind (dampDF_test, myList [[2]]); rm (myList)
	dampDF <- dampDF [,c("id", "cv_fold", names (dampDF)[grep("damp", names(dampDF))])]
	dampDF_test <- dampDF_test [,c("id", "cv_fold", names (dampDF_test)[grep("damp", names(dampDF_test))])]
	rm (train, test)
	train <- copy (train_bkp); 	test <- copy (test_bkp); 
	rm (train_bkp, test_bkp); gc ()
	dampDF <- dampDF [,names (dampDF)[-grep ("dampnum", names (dampDF))]]
	dampDF_test <- dampDF_test [,names (dampDF_test)[-grep ("dampnum", names (dampDF_test))]]
	dampNms <- names (dampDF) [grep ("damp", names (dampDF))]

	setDT (dampDF); setDT (dampDF_test)
	setkey (dampDF, "id"); setkey (dampDF_test, "id")
	dampDF_test <- dampDF_test [dampDF_test$cv_fold == mankraj_number_of_folds + 1,]
	dampDF [,cv_fold := NULL]
	dampDF_test [,cv_fold := NULL]

	setkey (train, "id"); setkey (test, "id")
	train <- merge (train, dampDF, by = "id")
	test <- merge (test, dampDF_test, by= "id")
}



