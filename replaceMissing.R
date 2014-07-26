	{
		setDT (train); setDT (test)
		for (i in c(posNms, myNms_larko, glmnetNms)) {
		        train [is.na(get(i)),i:=-1,with=FALSE]
		        train [which(get(i)==Inf),i:=-1,with=FALSE]
		        test [is.na(get(i)),i:=-1,with=FALSE]
		        test [which(get(i)==Inf),i:=-1,with=FALSE]
		}

		temp <- c(train$date_posted_mth, test$date_posted_mth)
		temp <- as.factor(temp)
		train [,date_posted_mth := temp [1:nrow(train)]]
		test [,date_posted_mth := temp [-c(1:nrow(train))]]

		train <- as.data.frame (train); test <- as.data.frame (test)
		rm (donations, dampDF, dampDF_test)
		gc ()
		rm (resources)
	}
