
	train_need_bin_1gram <- as (readMM ('tdms/need_statement/x_train_bin_1gram.mtx'), "dgCMatrix")[-1,]
	train_title_bin_1gram <- as (readMM ('tdms/title/x_train_bin_1gram.mtx'), "dgCMatrix")[-1,]
	test_desc_bin_1gram <- as (readMM ('tdms/short_description/x_test_bin_1gram.mtx'), "dgCMatrix")[-1,]
	test_need_bin_1gram <- as (readMM ('tdms/need_statement/x_test_bin_1gram.mtx'), "dgCMatrix")[-1,]
	test_title_bin_1gram <- as (readMM ('tdms/title/x_test_bin_1gram.mtx'), "dgCMatrix")[-1,]
	train_desc_bin_1gram <- as (readMM ('tdms/short_description/x_train_bin_1gram.mtx'), "dgCMatrix")[-1,]
	train_desc_bin_1gram_names <- read.csv ('tdms/short_description/bin_1gram.csv', header=F, stringsAsFactors = F)[,1] [-1]
	train_need_bin_1gram_names <- read.csv ('tdms/need_statement/bin_1gram.csv', header=F, stringsAsFactors = F)[,1] [-1]
	train_title_bin_1gram_names <- read.csv ('tdms/title/bin_1gram.csv', header=F, stringsAsFactors = F)[,1] [-1]
	
	
	colnames (train_desc_bin_1gram) <- train_desc_bin_1gram_names
	colnames (train_need_bin_1gram) <- train_need_bin_1gram_names
	colnames (train_title_bin_1gram) <- train_title_bin_1gram_names
	colnames (test_desc_bin_1gram) <- train_desc_bin_1gram_names
	colnames (test_need_bin_1gram) <- train_need_bin_1gram_names
	colnames (test_title_bin_1gram) <- train_title_bin_1gram_names

	train_essay_bin_1gram <- as (readMM ('tdms/essay/x_train_bin_1gram.mtx'), "dgCMatrix")[-1,]
	train_essay_bin_1gram_names <- read.csv ('tdms/essay/bin_1gram.csv', header=F, stringsAsFactors = F)[,1] [-1]
	test_essay_bin_1gram <- as (readMM ('tdms/essay/x_test_bin_1gram.mtx'), "dgCMatrix")[-1,]
	test_essay_bin_1gram_names <- read.csv ('tdms/essay/bin_1gram.csv', header=F, stringsAsFactors = F)[,1] [-1]
	colnames (train_essay_bin_1gram) <- train_essay_bin_1gram_names
	colnames (test_essay_bin_1gram) <- train_essay_bin_1gram_names

	setDT (train); setDT (test);
	train [,(c( "title", "short_description", "need_statement", "essay")):=NULL]
	test [,(c( "title", "short_description", "need_statement", "essay")):=NULL]
	donations_bin_1gram <- as (readMM ('tdms/donations/donations_1gram.mtx'), "dgCMatrix")[-1,]
	donations_1gram_names <- read.csv ('tdms/donations/donations_1gram.csv', header=F, stringsAsFactors = F)[,1] [-1]

	# Kill the useless variables
	train_title_bin_1gram <- train_title_bin_1gram [,which(colSums (train_title_bin_1gram) > 10),]
	train_need_bin_1gram <- train_need_bin_1gram [,which(colSums (train_need_bin_1gram) > 10),]
	train_essay_bin_1gram <- train_essay_bin_1gram [,which(colSums (train_essay_bin_1gram) > 64),]
	test_title_bin_1gram <- test_title_bin_1gram [,which(colSums (test_title_bin_1gram) > 0),]
	test_need_bin_1gram <- test_need_bin_1gram [,which(colSums (test_need_bin_1gram) > 0),]
	test_essay_bin_1gram <- test_essay_bin_1gram [,which(colSums (test_essay_bin_1gram) > 0),]
	train_desc_bin_1gram <- train_desc_bin_1gram [,which(colSums (train_desc_bin_1gram) > 10),]
	test_desc_bin_1gram <- test_desc_bin_1gram [,which(colSums (test_desc_bin_1gram) > 0),]




	train_title_bin_1gram <- train_title_bin_1gram [,colnames (train_title_bin_1gram) %in% colnames (test_title_bin_1gram)]
	train_need_bin_1gram <- train_need_bin_1gram [,colnames (train_need_bin_1gram) %in% colnames (test_need_bin_1gram)]
	train_essay_bin_1gram <- train_essay_bin_1gram [,colnames (train_essay_bin_1gram) %in% colnames (test_essay_bin_1gram)]
	test_title_bin_1gram <- test_title_bin_1gram [,colnames (test_title_bin_1gram) %in% colnames (train_title_bin_1gram)]
	train_desc_bin_1gram <- train_desc_bin_1gram [,colnames (train_desc_bin_1gram) %in% colnames (test_desc_bin_1gram)]	
	test_desc_bin_1gram <- test_desc_bin_1gram [,colnames (test_desc_bin_1gram) %in% colnames (train_desc_bin_1gram)]
	test_need_bin_1gram <- test_need_bin_1gram [,colnames (test_need_bin_1gram) %in% colnames (train_need_bin_1gram)]
	test_essay_bin_1gram <- test_essay_bin_1gram [,colnames (test_essay_bin_1gram) %in% colnames (train_essay_bin_1gram)]
	train_desc_bin_1gram <- train_desc_bin_1gram [,colnames (train_desc_bin_1gram) %in% colnames (test_desc_bin_1gram)]	
	test_desc_bin_1gram <- test_desc_bin_1gram [,colnames (test_desc_bin_1gram) %in% colnames (train_desc_bin_1gram)]

	train_text <- cBind (train_desc_bin_1gram, train_title_bin_1gram, train_need_bin_1gram, train_essay_bin_1gram)
	test_text <- cBind (test_desc_bin_1gram, test_title_bin_1gram, test_need_bin_1gram, test_essay_bin_1gram)

	big_train <- rBind (train_text, test_text)

	{
		rowNums <- big_train@i + 1
		tempCols <- big_train@p + 1
		temp_tempCols <- tempCols [2:length(tempCols)] - tempCols [1:length(tempCols)-1]
		colNums <- rep (1:length(temp_tempCols), temp_tempCols) 
		colDF <- data.frame (colNums = 1:ncol(big_train), colNames = colnames (big_train), stringsAsFactors = F)
		myVals <- big_train@x
		myDF <- data.frame (rowNums = rowNums, colNums = colNums, myVals = myVals, stringsAsFactors = F)
		setDT (myDF); setDT (colDF)
		myDF <- merge (myDF, colDF, by = "colNums")
		myDF <- myDF [,list (myVals = sum(myVals)),by = list (rowNums, colNames)]
		setkey (myDF, "colNames")
		colDF <- data.frame (colNames = unique (myDF$colNames), stringsAsFactors = F)
		setDT (colDF)
		colDF [,colNums := 1:nrow(colDF)]
		myDF <- merge (myDF, colDF, by = "colNames")
		rm (rowNums, tempCols, temp_tempCols, colNums, myVals, colDF)
		myDF <- myDF [order(myDF$rowNums),]
		big_train <- sparseMatrix (i = myDF$rowNums, j = myDF$colNums, x=myDF$myVals)
		myDF <- myDF [order(myDF$colNums),]
		colDF <- unique (myDF[,list(colNames, colNums)])
		colDF <- colDF [order(colDF$colNums),]
		myColNames <- myDF$colNames
		

		train_text <- big_train [1:nrow(train),]
		test_text <- big_train [-c(1:nrow(train)),]
		rm (big_train, colDF, myDF)



	}

	
	
	rm (train_desc_bin_1gram, train_title_bin_1gram, train_need_bin_1gram, test_desc_bin_1gram, test_title_bin_1gram, test_need_bin_1gram, train_essay_bin_1gram, test_essay_bin_1gram)
	
