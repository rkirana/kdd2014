
myTgt <- "is_exciting"
folderName <- "iamnotgoodenoughfolds"
library (doMC)
	library (foreach)
	library (rbenchmark)
	numcores <- 3
	registerDoMC (cores = numcores)

	system (paste ("cp pyGBM.py ", scikitLearnPath))
pyGBMNms <- allNms [allNms %notin% c(facNms, xgNms, posNms, boddaNms[-grep("teacher_acctid", boddaNms)], "factor_school_state")]
foreach (cv_fold=1:3, .combine = c) %dopar% runPyGBM (cv_fold, train, test, pyGBMNms, prefix = "gbm_m_nbodda_pos_fac_0.01_11_7","iamnotgoodenoughfolds")

pyGBMNms <- allNms [allNms %notin% c(xgNms, posNms, "date_posted_mth", boddaNms[-grep("teacher_acctid", boddaNms)], facNms_good, "factor_school_state")]
foreach (cv_fold=1:3, .combine = c) %dopar% runPyGBM (cv_fold, train, test, pyGBMNms, prefix = "gbm_m_mth_nbodda_pos_0.01_11_7","iamnotgoodenoughfolds")

pyGBMNms <- allNms [allNms %notin% c(xgNms, "date_posted_mth", facNms_good, "factor_school_state")]
foreach (cv_fold=1:3, .combine = c) %dopar% runPyGBM (cv_fold, train, test, pyGBMNms, prefix = "gbm_m_mth_factor_xg_0.01_11_7","iamnotgoodenoughfolds")



