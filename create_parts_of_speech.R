# create tags_pos_header_need and tags_pos_header_title_desc- using treetagger
{
	
	



	# create pos_need_df and pos_title_desc_df from tags data
	{
		KorpusCount <- function (x)  {
			myTbl <- table (x@TT.res$wclass)


			return (c (x@desc$words, x@desc$lines, x@desc$normalized.space, x@desc$chars.no.space,
		x@desc$punct, x@desc$digits, x@desc$letters.only, x@desc$words, x@desc$sentences, x@desc$avg.sentc.length, x@desc$avg.word.length
		,ifelse (length(myTbl[names (myTbl) %in% "noun"]) > 0, myTbl[names (myTbl) %in% "noun"], 0)
		,ifelse (length(myTbl[names (myTbl) %in% "pronoun"]) > 0, myTbl[names (myTbl) %in% "pronoun"], 0)
		,ifelse (length(myTbl[names (myTbl) %in% "verb"]) > 0, myTbl[names (myTbl) %in% "verb"], 0)
		,ifelse (length(myTbl[names (myTbl) %in% "adverb"]) > 0, myTbl[names (myTbl) %in% "adverb"], 0)
		,ifelse (length(myTbl[names (myTbl) %in% "conjunction"]) > 0, myTbl[names (myTbl) %in% "conjunction"], 0)
		,ifelse (length(myTbl[names (myTbl) %in% "interjection"]) > 0, myTbl[names (myTbl) %in% "interjection"], 0)
		,ifelse (length(myTbl[names (myTbl) %in% "preposition"]) > 0, myTbl[names (myTbl) %in% "preposition"], 0)
		,ifelse (length(myTbl[names (myTbl) %in% "adjective"]) > 0, myTbl[names (myTbl) %in% "adjective"], 0)
		,ifelse (length(myTbl[names (myTbl) %in% "possessive"]) > 0, myTbl[names (myTbl) %in% "possessive"], 0)
		,ifelse (length(myTbl[names (myTbl) %in% c("determiner", "predeterminer")]) > 0, myTbl[names (myTbl) %in% c("determiner", "predeterminer")], 0)
		,ifelse (length(myTbl[names (myTbl) %in% "modal"]) > 0, myTbl[names (myTbl) %in% "modal"], 0)
		,ifelse (length(myTbl[names (myTbl) %in% "comma"]) > 0, myTbl[names (myTbl) %in% "comma"], 0)
		,ifelse (length(myTbl[names (myTbl) %in% "particle"]) > 0, myTbl[names (myTbl) %in% "particle"], 0)
		))
		}




		pos_need_df <- sapply (tags_pos_header_need, KorpusCount)
		pos_title_desc_df <- sapply (tags_pos_header_title_desc, KorpusCount)
		pos_need_df <- t(pos_need_df)
		pos_title_desc_df <- t(pos_title_desc_df)

		colnames (pos_need_df) <- c("allchars", "lines", "normalized.space", "chars.no.space", "punct", "digits", "letters.only", "words", "sentences", "avg.sentc.length", "avg.word.length", "noun", "pronoun", "verb", "adverb", "conjunction", "interjection", "preposition", "adjective", "possessive", "determiner", "modal", "comma", "particle")
		colnames (pos_title_desc_df) <- c("allchars", "lines", "normalized.space", "chars.no.space", "punct", "digits", "letters.only", "words", "sentences", "avg.sentc.length", "avg.word.length", "noun", "pronoun", "verb", "adverb", "conjunction", "interjection", "preposition", "adjective", "possessive", "determiner", "modal", "comma", "particle")

		rownames (pos_need_df) <- NULL
		rownames (pos_title_desc_df) <- NULL


		create_pos_ratios <- function (myDT) {
			myDT [,pos_words_per_line := words/lines]
			myDT [,pos_chars_per_line := chars.no.space/lines]
			myDT [,pos_chars_per_words := chars.no.space/words]
			myDT [,pos_chars_nospacechars_ratio := chars.no.space/allchars]
			myDT [,pos_punct_per_line := punct/lines]
			myDT [,pos_punct_per_word := punct/words]
			myDT [,pos_pct_noun := noun/words]
			myDT [,pos_pct_verb := verb/words]
			myDT [,pos_pct_adverb := adverb/words]
			myDT [,pos_pct_pronoun := pronoun/words]
			myDT [,pos_pct_conjunction := conjunction/words]
			myDT [,pos_pct_interjection := interjection/words]
			myDT [,pos_pct_preposition := preposition/words]
			myDT [,pos_pct_adjective := adjective/words]
			myDT [,pos_pct_determiner := determiner/words]
			myDT [,pos_pct_possessive := possessive/words]
			myDT [,pos_pct_modal := modal/words]
			myDT [,pos_pct_comma := comma/words]
			myDT [,pos_pct_particle := particle/words]
			setnames (myDT, "avg.word.length", "pos_avg.word.length")
			setnames (myDT, "avg.sentc.length", "pos_avg.sentc.length")
			setnames (myDT, "lines", "pos_lines")
			return (myDT)
		}

		pos_need_df <- as.data.frame (pos_need_df)
		pos_title_desc_df <- as.data.frame (pos_title_desc_df)
		setDT (pos_need_df)
		setDT (pos_title_desc_df)
		pos_need_df <- create_pos_ratios  (pos_need_df)
		pos_title_desc_df <- create_pos_ratios  (pos_title_desc_df)



		id_projectid_pos_df <- rbind (train[,list(id, projectid)], test [,list(id,projectid)])

		pos_need_df <- as.data.frame (pos_need_df)
		pos_title_desc_df <- as.data.frame (pos_title_desc_df)
		pos_need_df <- cbind (pos_need_df, id_projectid_pos_df)
		pos_title_desc_df <- cbind (pos_title_desc_df, id_projectid_pos_df)

		pos_need_df <- as.data.frame (pos_need_df)
		pos_title_desc_df <- as.data.frame (pos_title_desc_df)

		colnames (pos_need_df) <- paste ( colnames (pos_need_df), "need", sep = "_")
		colnames (pos_title_desc_df) <- paste (colnames (pos_title_desc_df), "title_desc", sep = "_")
		setDT (pos_need_df)
		setDT (pos_title_desc_df)
		setnames (pos_need_df, "id_need", "id"); setnames (pos_need_df, "projectid_need", "projectid")
		setnames (pos_title_desc_df, "id_title_desc", "id"); setnames (pos_title_desc_df, "projectid_title_desc", "projectid")

		rm (id_projectid_pos_df)





	}



	posNms <- c (names (pos_need_df)[ grep("pos", names (pos_need_df))],
names (pos_title_desc_df)[ grep("pos", names (pos_title_desc_df))])
	
setkey (pos_need_df, "projectid")
	setkey (pos_title_desc_df, "projectid")

	bigPOSDF <- merge (pos_need_df, pos_title_desc_df, by = "projectid", all.x=TRUE)
	bigPOSDF <- bigPOSDF[,c("projectid", posNms), with= F]
	train <- merge (train, bigPOSDF, by = "projectid", all.x=TRUE)
	test <- merge (test, bigPOSDF, by = "projectid", all.x=TRUE)
	rm (bigPOSDF)
	rm (pos_need_df, pos_title_desc_df)
	gc ()
	rm (tags_pos_header_need, tags_pos_header_title_desc)


	train [,pos_titleDescCaps := (titleCaps + descrCaps)/(pos_lines_title_desc * pos_chars_per_line_title_desc)]
	train [,pos_needCaps := (needCaps)/(pos_lines_need * pos_chars_per_line_need)]

	test [,pos_titleDescCaps := (titleCaps + descrCaps)/(pos_lines_title_desc * pos_chars_per_line_title_desc)]
	test [,pos_needCaps := (needCaps)/(pos_lines_need * pos_chars_per_line_need)]

	posNms <- c(posNms, "pos_titleDescCaps", "pos_needCaps")



	train <- train [order(train$id),]; 	test <- test [order(test$id),]
	gc ()
	train [,essay:=NULL]
	train [,title:=NULL]
	train [,short_description:=NULL]
	train [,need_statement := NULL]
	test [,essay:=NULL]
	test [,title:=NULL]
	test [,short_description:=NULL]
	test [,need_statement := NULL]
}
