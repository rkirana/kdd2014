# category as integer
	{
		setDT (train); setDT( test)
		for (i in facNms_good) {
			train [,paste ("cart", i, sep="_"):=as.integer (get(i)),with=FALSE]
			test [,paste ("cart", i, sep="_"):=as.integer (get(i)),with=FALSE]
		}
		for (i in facNms_ignore) {
			train [,paste ("xg", i, sep="_"):=as.integer (get(i)),with=FALSE]
			test [,paste ("xg", i, sep="_"):=as.integer (get(i)),with=FALSE]
		}

		cartNms <- names (train) [grep ("cart", names (train))]
		xgNms <- names (train) [grep ("xg", names (train))]

		for (i in c(cartNms, xgNms)) {
			train[is.na(get(i)),i:=0,with=FALSE]
			test[is.na(get(i)),i:=0,with=FALSE]
		}

		train <- train [order(train$id),]
		test <- test [order(test$id),]
		train <- as.data.frame (train); test <- as.data.frame (test)

	}


lengthNms <- names(train) [grep ("nchar", names (train))]
glmnetNms <- c('fulfillment_labor_materials','total_price_excluding_optional_support','total_price_including_optional_support','students_reached','school_charter','school_magnet','school_year_round','school_nlns','school_kipp','school_charter_ready_promise','teacher_teach_for_america','teacher_ny_teaching_fellow','eligible_double_your_impact_match','eligible_almost_home_match','date_posted_year','date_posted_mth','donor_charges','donor_charges_pct')



facNms_good <- facNms_good [facNms_good %notin% "factor_school_state"]
allNms <- c(glmnetNms, facNms_good, "factor_school_state", cartNms, xgNms, dampNms, "date_posted_mth", lengthNms, boddaNms, myNms_larko, posNms)
allNms <- unique (allNms)


