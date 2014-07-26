# Set these variables to the right paths - these are paths on my system
current_working_dir <- '/home/kiran/kdd2014'
	# note: you have to install the development version of scikit-learn because we use GBMs with early stopping to prevent overfitting + we want to randomly choose the number of trees at each split
scikitLearnPath <- "/home/kiran/scikit-learn"
	# vowpal wabbit must be installed and must be in your .bashrc file so that we can call as vw
# set the path to xgboost library here
xgboost_path <- "/home/kiran/xgboost-master/xgboost"

# make sure the following packages are installed
#  data.table
#  Matrix
#  glmnet
#  doMC
#  foreach
#  rbenchmark
#  Metrics
#  gbm
#  RRF
#  lme4
# current working directory
setwd (current_working_dir)
source ('functions.R')
library (data.table);library (Matrix); library (glmnet); library (doMC); library (foreach); library (rbenchmark); library (Metrics); library (gbm); library (RRF); library (lme4)

# make directories to store outputs
system ('mkdir iamnotgoodenoughfolds')
system ('mkdir outcomefolds')
setwd ('tdms')
for (textVar in c( "title", "short_description", "need_statement", "essay")) {
		system (paste ("mkdir", textVar))
	}
setwd ('..')

# read files and create train and test; + create stratified sample
source ('readFiles.R')

# create frequency features: For every categorical variable get the count of the # of times it occurs
source ('create_freq_features.R')

# create shrunken averages for every categorical feature - this is to prevent "overfitting" of normal averages
source ('create_shrunken_averages.R')


# TEXT MINING: create features for percentage of capital letters in the essay written requesting donations 
source ('create_pct_caps.R')

# TEXT MINING: create 'parts of speech' variables for the title, description and need_statement of the essays of the donors -> using the treetagger library
if (1==0) {
	numcores <- 8 # set numcores to max # of cores in your system; parts of speech takes a long time to run
	source ('create_pos_tags.R')
}
# above takes a lot of time to run; recommend you just use what I completed from the run
else {
	load ('tags_pos_header_title_desc.rData')
	load ('tags_pos_header_need.rData')
}
source ('create_parts_of_speech.R')

# Create shrunken averages for all the outcome variables
source ('create_damp_alloutcomevars.R')

# TEXT MINING: create binary TDMs for title, description and need_statement
source ('runPyTDM.R')
source ('create_tdm.R')

# HIGH DIMENSIONALITY: create sparse features
source ('create_sparse.R')

# Write actual and identifiers in the current working directory
source ('create_ids_acts.R')
source ('facasint.R')

# Replace Missing values
source ('replaceMissing.R')

# Run a vowpal wabbit
source ('runVW.R')

# Run a xgboost
source ('runXGBoost.R')

# Run a GBM in parallel
source ('runPYGBM.R')

# Run a R Randomforest on undersampled data - undersampling gives good results for imbalanced datasets
source ('runRF_undersampled.R')

# Run a R GBM on greedy data
source ('mining_vars.R')
source ('runGBM_R.R')

# Run WRF - weighted random forest - to help in handling imbalanced data
source ('wrf.R')


# ensemble the outputs
source ('create_ensemble.R')



