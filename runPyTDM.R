train <- train [order(train$id),]
test <- test [order(test$id),]

essays <- read.csv ('essays.csv', header=T, stringsAsFactors = F)
setDT (essays);setkey (essays, "projectid"); 
setkey (essays, "projectid");
names (essays)
essays [,teacher_acctid := NULL]

train <- merge (train, essays, by = "projectid", all.x=TRUE)
test <- merge (test, essays, by = "projectid", all.x=TRUE)
rm (essays)
gc ()



	train <- as.data.frame (train)
	test <- as.data.frame (test)


	system (paste ("cp create_binary_matrix.py ", scikitLearnPath))


	for (textVar in c( "title", "short_description", "need_statement", "essay")) {
		print ('-------')
		print (textVar)
		print ('-------')
		write.csv (train[,c ("projectid", textVar)], file = 'train.csv', row.names = F)
		setwd ("tdms")
		system (paste ("python ", scikitLearnPath, "/create_binary_matrix.py", sep=""))
		system (paste ("cp *.csv *.mtx ", textVar, sep=" "))
		system ("rm *.csv *.mtx")
		setwd ("..")
	}
