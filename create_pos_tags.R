{

		library(Korpus)
		library (data.table)
		library (stringr)
		set.kRp.env(TT.cmd="/home/kiran/treetag/bin/tree-tagger", lang="en")
		treeTagger <- paste ("/home/kiran/treetag/bin/tree-tagger")
		paramFile <- paste ("english-utf8.par")
		big_train <- rbind (train [,list (title, short_description, need_statement)], test [,list (title, short_description, need_statement)])
		big_train [,title_desc] <- apply (big_train, 1, function (x) paste (x["title"], x["short_description"], sep = " "))]

		processKorpus <- function(this.text) {
		  this.text.file <- tempfile()
		  cat(this.text, file=this.text.file)
		  tag.results <- treetag(file=this.text.file, treetagger = "manual", lang="en", TT.options = list(path = "/home/kiran/treetag", params = paramFile, preset="en"))
		  file.remove(this.text.file) 
		  return (tag.results)
		}

		library (doMC)
		library (foreach)
		library (rbenchmark)
	
		registerDoMC (cores = numcores)
		indices <- c(rep (1:numcores, each = nrow(big_train)/numcores)
		indices <- c(indices, rep (numcores, nrow (big_train) - length (indices)))
		big_train$indices <- indices; rm (indices)

		tags_pos_header_title_desc <- foreach (i=1:numcores, .combine = c) %dopar% lapply (big_train[big_train$indices == i,"title_desc"], processKorpus)
		tags_pos_header_need <- foreach (i=1:numcores, .combine = c) %dopar% lapply (big_train[big_train$indices == i,"need_statement"], processKorpus)
		rm(big_train)
	}
