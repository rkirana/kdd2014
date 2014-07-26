#############################################################################################
##########################FREQ FEATURES ##################################################
###############################################################################################
# freq variables
{
	train <- as.data.frame (train); test <- as.data.frame (test)
	big_train <- rbind (train[,facNms], test[,facNms])
	# Start binarising
	for (var in facNms) {
		all <- c(as.character(big_train[,var]))
		freqvar <- paste(var, '.freq', sep='');
		freqs <- table(all);   
		big_train[,freqvar] <- freqs[as.character(big_train[,var])];
		big_train[,freqvar] <- as.numeric(big_train[,freqvar])/(nrow(big_train))
		big_train[,var] <- NULL
	}
	myNms_larko <- names (big_train)
	train<-cbind (train, big_train[1:nrow(train), ])
	test <- cbind (test, big_train [-c(1:nrow(train)),])
	rm (big_train)
	gc ()
	setDT (train)
	setDT (test)

	temp <- as.data.table (train)
	setkey (temp, "projectid")
	setDT (donations)
	setkey (donations, "projectid")
	donations_old <- copy (donations)
	donations <- donations [,paste (donation_message, collapse = " "),by = projectid]
	temp <- merge (temp, donations, by = "projectid", all.x=TRUE)
	temp <- temp [order(temp$id),]	
	temp <- as.data.frame (temp[,list(projectid, V1)])
	donations <- temp
	setDT (donations)
	setnames (donations, "V1", "donation")
}
