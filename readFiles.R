essays <- read.csv ('essays.csv', header=T, stringsAsFactors = F)
resources <- read.csv ('resources.csv', header=T, stringsAsFactors = F)
projects <- read.csv ('projects.csv', header=T, stringsAsFactors = F)
outcomes <- read.csv ('outcomes.csv', header=T, stringsAsFactors = F)
submission <- read.csv ('sampleSubmission.csv', header=T, stringsAsFactors = F)
donations <- read.csv ('donations.csv', header=T, stringsAsFactors = F)

# create train and test
setDT (essays); setDT (resources); setDT (outcomes); setDT (projects); setDT (submission); 
setkey (essays, "projectid"); setkey (resources, "projectid"); setkey (projects, "projectid"); setkey (submission, "projectid")
outcomeNms <- names (outcomes) [names (outcomes) %notin% c("projectid", "is_exciting")]
train <- merge (projects, outcomes, by = "projectid", all.x=TRUE)
test <- train [is.na(train$is_exciting),]; train <- train[!is.na(train$is_exciting),]
test [,(names (outcomes) [3:ncol(outcomes)]):=NULL]; test [,is_exciting:=0]


# is there a pattern to missing values of outcomes
train [,flag_missing:=ifelse (is.na(non_teacher_referred_count), 1, 0)]
train [,flag_missing := ifelse (is.na(great_messages_proportion), 1, flag_missing)]
# large number of names are true/false - set to 1/0
tfNms <- c('school_charter','school_magnet','school_year_round','school_nlns','school_kipp','school_charter_ready_promise','teacher_teach_for_america','teacher_ny_teaching_fellow','eligible_double_your_impact_match','eligible_almost_home_match','at_least_1_teacher_referred_donor','fully_funded','at_least_1_green_donation','great_chat','three_or_more_non_teacher_referred_donors','one_non_teacher_referred_donor_giving_100_plus','donation_from_thoughtful_donor')
temp <- train [,tfNms,with=F]; train [,(tfNms):=NULL]; temp <- temp [,lapply(.SD, function (x) ifelse (x == "t", 1, 0))]; train <- cbind (train, temp); rm (temp);
tfNms <- intersect (tfNms, names (test))
temp <- test [,tfNms,with=F]; test [,(tfNms):=NULL]; temp <- temp [,lapply(.SD, function (x) ifelse (x == "t", 1, 0))]; test <- cbind (test, temp); rm (temp)
train [,is_exciting := ifelse (is_exciting == "t", 1, 0)]
# date posted should be a date
train [,date_posted := as.POSIXct (date_posted, format = "%Y-%m-%d", tz = "UTC", origin = "1970-1-1")]; test [,date_posted := as.POSIXct (date_posted, format = "%Y-%m-%d", tz = "UTC", origin = "1970-1-1")]
train [,date_posted_year := year (date_posted)]
train [,date_posted_mth := month (date_posted)]
train [,list (length(which(is_exciting == 1))/length (is_exciting), length(is_exciting)), by = date_posted_year][order(date_posted_year),]
# all data prior to 2010-08 is all is_exciting = 0 and hence can be ignored
train <- train [train$date_posted_year > 2009,]
test [,date_posted_year := year (date_posted)]
test [,date_posted_mth := month (date_posted)]
train <- train [!(train$date_posted_year == 2010 & train$date_posted_mth <= 8),]
# given essays is a big table we only want relevant stuff over there - merge with essays and remove essays
essays <- essays [essays$projectid %in% unique (c(train$projectid, test$projectid))]
essays [,nchar_description := nchar (short_description)]
essays [,nchar_need_statement := nchar (need_statement)]
essays [,nchar_title := nchar (title)]
essays [,nchar_essay := nchar (essay)]
train <- merge (train, essays [,list (projectid, nchar_description, nchar_need_statement, nchar_title, nchar_essay)], by = "projectid", all.x=TRUE)
test <- merge (test, essays [,list (projectid, nchar_description, nchar_need_statement, nchar_title, nchar_essay)], by = "projectid", all.x=TRUE)
sparseNms <- c("teacher_acctid", "schoolid", "school_ncesid","school_district", "school_county","school_city", "school_zip")
factorNms <- c( "school_state","school_metro",  "teacher_prefix", "primary_focus_area", "primary_focus_subject",  "secondary_focus_area", "secondary_focus_subject", "resource_type", "poverty_level", "grade_level")
factorNms <- c(factorNms, sparseNms)
temp <- rbind (train[,factorNms,with=F], test[,factorNms,with=F]); temp <- temp[,lapply(.SD, as.factor)]; train[,(factorNms) := NULL]; test [,(factorNms):=NULL]; setnames (temp, paste ("factor", names (temp), sep="_")); train <- cbind (train, temp [1:nrow(train),]); test <- cbind (test, temp [-c(1:nrow(train)),])
rm (submission); rm (outcomes); rm (projects); gc ()
# donor charges - feature
train [,donor_charges := total_price_including_optional_support- total_price_excluding_optional_support]
test [,donor_charges := total_price_including_optional_support- total_price_excluding_optional_support]
train[,donor_charges_pct := donor_charges/total_price_including_optional_support]
test[,donor_charges_pct := donor_charges/total_price_including_optional_support]


# create a stratified sample
train <- train [order(train$date_posted),]
temp <- copy (train)
temp [,cnt := 1]
temp <- temp [,list (seq = cumsum(cnt)),by = date_posted]
temp <- temp [order(temp$date_posted),]
temp [,indices := seq %% 3]
setkey (train, "projectid")
train [,indices := temp$indices]
rm (temp)
gc ()
facNms <- names (train) [grep ("factor", names (train))]
	myTgt <- "is_exciting"
	train [,id:= 1:nrow(train)]
	test [,id:=(nrow(train)+1): (nrow(train)+nrow(test))]
facNms_ignore <- paste ("factor", sparseNms, sep = "_")
	mankraj_number_of_folds <- 3



