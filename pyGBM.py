#! /usr/bin/env python

import pandas as pd
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import ExtraTreesClassifier
from sklearn.externals import joblib
from sklearn import metrics
import numpy as np
from sklearn import metrics, cross_validation, linear_model
import os
from numpy import array, hstack
from itertools import combinations
from scipy import sparse
import sys


os.chdir ('/home/kiran/kdd')
#from kddbusinesseval import kdd_metrics
#validFileWeights='/home/kiran/kdd/validWeights.csv',
def main (trainXFile='/home/kiran/kdd/trainXall.csv',trainYFile='/home/kiran/kdd/trainY.csv',validXFile='/home/kiran/kdd/validXall.csv',validYFile='/home/kiran/kdd/validY.csv',testXFile='/home/kiran/kdd/testXall.csv', n_estimators = 150, n_estimators_step = 150,
learning_rate = 0.005,max_features = 30,max_depth = 11,verbose = 0, dump_file = '/home/kiran/kdd/pymodels/gbm_all_0.005_30_11.pkl', outputFile = 'prediction.txt', max_trees = 300, random_state = 11):
	actual = np.loadtxt (validYFile, delimiter = ",")
	trainY = np.loadtxt (trainYFile, delimiter = ",")
	train = pd.read_csv (trainXFile)
	valid = pd.read_csv (validXFile)
	#trainY = pd.read_csv (trainYFile)
	#trainY = trainY.ix [:,'x']
	#validY = pd.read_csv (validYFile)
	#actual = validY.ix [:,'x']
	#validWeights = pd.read_csv (validFileWeights)
	#validWeights = validWeights.ix [:,'x']
	#actual = actual.get_values ()

	gbm = GradientBoostingClassifier (n_estimators = n_estimators, learning_rate = learning_rate, max_features = max_features, max_depth = max_depth, random_state = random_state, verbose = verbose)
	gbm.fit (train, trainY)
	prediction_valid = gbm.predict_proba(valid)[:,1]

	#gbm = joblib.load ( '/home/kiran/kdd/pymodels/gbmmore.pkl')
	bestAUC = 0
	#myAUC = kdd_metrics (actual, prediction_valid, validWeights)
	myAUC = metrics.roc_auc_score(actual, prediction_valid)
	bestAUC = 0

	while myAUC >= bestAUC:
		n_estimators = n_estimators + n_estimators_step
		gbm.set_params(n_estimators=n_estimators, warm_start=True)  
		gbm.fit (train, trainY)
		prediction_valid = gbm.predict_proba(valid)[:,1]
		myAUC = metrics.roc_auc_score(actual, prediction_valid)
		print "bestAUC: %f myAUC: %f" % (bestAUC, myAUC)
		improvement = myAUC - bestAUC 
		if improvement < 0.0000000001:
			break
		if n_estimators > max_trees:
			break
		bestAUC = myAUC
		bestPrediction = prediction_valid
		joblib.dump (gbm, dump_file)
		print "bestAUC: %f improvement: %f" % (bestAUC, improvement)

	myAUC = metrics.roc_auc_score(actual, bestPrediction)
	print "AUC: %f bestPrediction: %f" % (myAUC, improvement)
	test = pd.read_csv (testXFile)
	prediction_test1 = gbm.predict_proba(test)[:,1]
	prediction_total = np.concatenate ((bestPrediction, prediction_test1), axis=0)
	np.savetxt (outputFile, prediction_total, delimiter=",")
	#mank = np.concatenate ((actual, bestPrediction), axis=1)
	#np.savetxt ('/home/kiran/temp.txt', mank, delimiter=",")


    
if __name__ == "__main__":
    #args = {trainXFile:'/home/kiran/kdd/trainXall.csv',trainYFile:'/home/kiran/kdd/trainY.csv',validXFile:'/home/kiran/kdd/validXall.csv',validYFile:'/home/kiran/kdd/validY.csv',predictionFile:'/home/kiran/kdd/prediction_test_all_0.005_30_11.csv',testXFile:'/home/kiran/kdd/testXall.csv', n_estimators : 150, n_estimators_step : 150,learning_rate : 0.005,max_features : 30,max_depth : 11,random_state : 11,verbose : 0  }
    trainXFile = sys.argv[1]
    trainYFile = sys.argv[2]
    validXFile = sys.argv[3]
    validYFile = sys.argv[4]
    testXFile = sys.argv[5]
    n_estimators = int (sys.argv[6])
    n_estimators_step = int (sys.argv[7])
    learning_rate = float (sys.argv[8])
    max_features = int (sys.argv[9])
    max_depth = int (sys.argv[10])
    #random_state = sys.argv[11]
    verbose = int (sys.argv [11])
    dump_file = sys.argv[12]
    outputFile = sys.argv[13]
    max_trees = int (sys.argv [14])
    print trainXFile
    print trainYFile
    print validXFile
    print validYFile
    print testXFile
    print n_estimators
    print n_estimators_step
    print learning_rate
    print max_features
    print max_depth
    #print random_state
    #print verbose
    main(trainXFile, trainYFile, validXFile, validYFile,  testXFile, n_estimators, n_estimators_step, learning_rate, max_features, max_depth,  verbose, dump_file, outputFile, max_trees, random_state=11)

	

