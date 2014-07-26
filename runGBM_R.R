 gbmNms <- unique (c(rrfNms, grrfNms, sel_glmnetNms))
	library (doMC)
	library (foreach)
	library (rbenchmark)
	numcores <- 3
	registerDoMC (cores = numcores)

# Want to make the GBM run - it fails usually
	gbmNms <- gbmNms [gbmNms %notin% "date_posted_mth"]
	rfNms <- gbmNms
library (foreach)
library (doParallel)
cl <- makeCluster (3)
registerDoParallel(cl)

	foreach (cv_fold=1:3, .combine = c) %dopar% runGBM (cv_fold, prefix = "rgbm_greedy_7_10_0.01", FALSE, 
	"iamnotgoodenoughfolds", gbmNms, myTgt, bag.fraction = 1,myInteractionDepth=7, myDistribution = "bernoulli", 
	myShrinkage=0.01,myTrees=150, myMaxTrees=15000, myMinObsInNode=10)

foreach (cv_fold=1:3, .combine = c) %dopar% runGBM (cv_fold, prefix = "rgbm_greedy_nofacxg_7_10_0.01", FALSE, 
	"iamnotgoodenoughfolds", gbmNms [gbmNms %notin% c(facNms, "factor_school_state", xgNms)], myTgt, bag.fraction = 1,myInteractionDepth=7, myDistribution = "bernoulli", 
	myShrinkage=0.01,myTrees=150, myMaxTrees=15000, myMinObsInNode=10)

	


