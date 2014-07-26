# Make sparse features
	{

		setDT (train)
		setDT (test)

		train <- train [order(train$id),]
		test <- test [order(test$id),]
		facNms_good <- facNms [facNms %notin% facNms_ignore]
		train <- as.data.frame (train); test <- as.data.frame (test)
		facNms_good <- facNms [facNms %notin% facNms_ignore]
		big_train <- rbind (train[,facNms_good], test[,facNms_good])
		setDT (big_train)
		big_train <- big_train [,lapply(.SD, as.factor)]
		big_train <- as.data.frame (big_train)
		facMtx <- makeSparseMtxFromFactor (big_train, facNms_good)

		setDT (train); setDT (test)
		train [,(facNms_good):=NULL]
		test [,(facNms_good):=NULL]
		train <- cbind (train, big_train[ 1:nrow(train),])
		test <- cbind (test, big_train [-c(1:nrow(train)),])
		train <- as.data.frame (train); test <- as.data.frame (test)

		rm (big_train)

		big_train <- rbind (train[,facNms_ignore], test[,facNms_ignore])
		setDT (big_train)
		big_train <- big_train [,lapply(.SD, as.factor)]
		big_train <- as.data.frame (big_train)
		facMtx_ignore <- makeSparseMtxFromFactor (big_train, facNms_ignore)
setDT (train); setDT (test)
		train [,(facNms_ignore):=NULL]
		test [,(facNms_ignore):=NULL]
		train <- cbind (train, big_train[ 1:nrow(train),])
		test <- cbind (test, big_train [-c(1:nrow(train)),])
		train <- as.data.frame (train); test <- as.data.frame (test)

	
		rm (big_train)

	
		facMtx_train <- facMtx [1:nrow(train),]
		facMtx_test <- facMtx [-c(1:nrow(train)),]
		facMtx_ignore_train <- facMtx_ignore [1:nrow(train),]
		facMtx_ignore_test <- facMtx_ignore [-c(1:nrow(train)),]
		rm (facMtx, facMtx_ignore)
		rm (donations, donations_old)
		gc ()
	}
