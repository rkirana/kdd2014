glmnetNms <- names (train) [c(1:37, 55:56)]
glmnetNms <- glmnetNms [glmnetNms %notin% c("id", "projectid", "school_latitude", "school_longitude", "date_posted", "is_exciting", outcomeNms)  ]
		cartNms
		xgNms
		dampNms
		boddaNms
		facNms 
		glmnetNms
		myNms_larko
		resourceNms
		resource_irlbaNms
tgts_outcomes <- outcomeNms
outcomeNms <- unique (c(outcomeNms, "flag_missing"))
vwNms <- glmnetNms [glmnetNms %notin% c("nchar_title", "nchar_description", "nchar_need_statement", "nchar_essay" )]

lengthNms <- names(train) [grep ("nchar", names (train))]
glmnetNms <- glmnetNms [glmnetNms %notin% "flag_missing"]
glmnetNms <- glmnetNms [glmnetNms %notin% essayNms]
facNms_good <- facNms_good [facNms_good %notin% "factor_school_state"]
allNms <- c(glmnetNms, facNms_good, essayNms, "factor_school_state", cartNms, xgNms, dampNms, "date_posted_mth", lengthNms, boddaNms, myNms_larko, posNms)

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

allNms <- unique (allNms)

