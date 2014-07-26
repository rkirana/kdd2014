
#####################################################################################
###############OUTCOME BASED DAMP VARIABLES ###################################################
##################################################################################

{
	setDT (train); setDT (test)
	train_bkp <- copy (train); test_bkp <- copy (test)
	for (i in facNms_ignore) {
	    train [,i:=as.character (get(i)),with=FALSE]
	    test [,i:=as.character (get(i)), with = FALSE]
	}
	
	big_dampDF <- NULL
	big_dampDF_test <- NULL
	tgts_outcomes <- c(outcomeNms, "flag_missing")
	tgts_outcomes <- tgts_outcomes [tgts_outcomes %notin% c("projectid", myTgt)]

	for (myTgt in tgts_outcomes [tgts_outcomes %notin% c("great_messages_proportion", "teacher_referred_count", "non_teacher_referred_count")]) {
		dampDF <- NULL; 	dampDF_test <- NULL
		test <- as.data.frame (test)
		test [,myTgt] <- 0
		setDT (test)
		for (cv_fold in c(1:mankraj_number_of_folds)) {
			print (myTgt)		
			print (cv_fold)
		    	myList <- fn_addDampAvg ( train = as.data.frame (train[train$indices != cv_fold, c(facNms_ignore, "id", myTgt),with=F]), valid  = as.data.frame (rbind (train[train$indices == cv_fold, c(facNms_ignore, "id", myTgt),with=F],test[,c(facNms_ignore, "id", myTgt),with=F])), facNms_ignore, myTgt)
			myList[[1]]$cv_fold <- cv_fold
			myList[[2]]$cv_fold <- cv_fold
		  	dampDF <- rbind (dampDF, myList [[2]] [c(1:length(which(train$indices == cv_fold))),])
			dampDF_test <- rbind (dampDF_test, myList [[2]] [-c(1:length(which(train$indices == cv_fold))),])

			rm (myList); gc ()
		}
		cv_fold <- mankraj_number_of_folds +1
		myList <- fn_addDampAvg ( train = as.data.frame (train[, c(facNms_ignore, "id", myTgt),with=F]), valid  = as.data.frame (test[,c(facNms_ignore, "id", myTgt),with=F]), facNms_ignore, myTgt)
		myList [[2]]$cv_fold <- cv_fold
		dampDF_test <- rbind (dampDF_test, myList [[2]]); rm (myList)
		dampDF <- dampDF [,c("id", "cv_fold", names (dampDF)[grep("damp", names(dampDF))])]
		dampDF_test <- dampDF_test [,c("id", "cv_fold", names (dampDF_test)[grep("damp", names(dampDF_test))])]
		dampDF$target <- myTgt
		dampDF_test$target <- myTgt

		big_dampDF <- rbind (big_dampDF, dampDF)
		big_dampDF_test <- rbind (big_dampDF_test, dampDF_test)
		rm (dampDF, dampDF_test)
	}
	big_dampDF <- big_dampDF [,names (big_dampDF) [-grep ("dampnum", names (big_dampDF))]]
	big_dampDF_test <- big_dampDF_test [,names (big_dampDF_test) [-grep ("dampnum", names (big_dampDF_test))]]
	setDT (big_dampDF)
	mankDF <- NULL
	k <- 1
	dampNms_oc <- names (big_dampDF)[ grep ("damp", names (big_dampDF))]
	for (myVar in dampNms_oc) {	
		temp <- dcast.data.table (big_dampDF, id ~ target, value.var = myVar)
		
		setnames (temp, c("id", paste (gsub ("ftr_damp_", "bodda_", myVar), names (temp)[-1], sep="")))
		if (k==1) mankDF <- copy (temp)
		else mankDF <- merge (mankDF, temp, by ="id", all.x=TRUE)
		rm (temp)
		k <- k+1
	}

	setDT (big_dampDF_test)
	big_dampDF_test <- big_dampDF_test [big_dampDF_test$cv_fold == 4,]
	mankDF_test <- NULL
	k <- 1
	for (myVar in dampNms_oc) {	
		temp <- dcast.data.table (big_dampDF_test, id ~ target, value.var = myVar)
		
		setnames (temp, c("id", paste (gsub ("ftr_damp_", "bodda_", myVar), names (temp)[-1], sep="")))
		if (k==1) mankDF_test <- copy (temp)
		else mankDF_test <- merge (mankDF_test, temp, by ="id", all.x=TRUE)
		rm (temp)
		k <- k+1
	}
	rm (big_dampDF, big_dampDF_test); gc ()

	setDT (train); setDT (test)
	setkey (train, "id"); setkey (test, "id")
	train <- merge (train, mankDF, by = "id")
	test <- merge (test, mankDF_test, by = "id")
	rm (mankDF, mankDF_test)
	gc ()
	boddaNms <- names (train) [grep ("bodda", names (train))]

	
	


}
