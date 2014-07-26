essays <- read.csv ('essays.csv', header=T, stringsAsFactors = F)
setDT (essays);setkey (essays, "projectid"); 
	setkey (essays, "projectid");
	names (essays)
	essays [,teacher_acctid := NULL]

	train <- merge (train, essays, by = "projectid", all.x=TRUE)
	test <- merge (test, essays, by = "projectid", all.x=TRUE)
	rm (essays)
	gc ()


	
	library (stringr)
	titleCaps <- sapply (train$title, function (x) length (unlist (str_locate_all(x, "[A-Z]")))/2)
	descCaps <- sapply (train$short_description, function (x) length (unlist (str_locate_all(x, "[A-Z]")))/2)
	needCaps <- sapply (train$need_statement, function (x) length (unlist (str_locate_all(x, "[A-Z]")))/2)
	train [,titleCaps := titleCaps]
	train [,needCaps := needCaps]
	train [,descrCaps := descCaps]
	rm (titleCaps, descCaps, needCaps)

	titleCaps <- sapply (test$title, function (x) length (unlist (str_locate_all(x, "[A-Z]")))/2)
	descCaps <- sapply (test$short_description, function (x) length (unlist (str_locate_all(x, "[A-Z]")))/2)
	needCaps <- sapply (test$need_statement, function (x) length (unlist (str_locate_all(x, "[A-Z]")))/2)
	test [,titleCaps := titleCaps]
	test [,needCaps := needCaps]
	test [,descrCaps := descCaps]
	rm (titleCaps, descCaps, needCaps)

