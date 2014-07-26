import numpy as np
from sklearn import metrics,preprocessing,cross_validation
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.feature_extraction.text import CountVectorizer
import sklearn.linear_model as lm
import pandas as p
import csv
from scipy import io
import scipy
from numpy import array, hstack
from sklearn import linear_model
from scipy import sparse
from itertools import combinations
from sklearn.linear_model import SGDClassifier
import nltk
from nltk import clean_html
from nltk import SnowballStemmer
from nltk import PorterStemmer
from nltk.stem.lancaster import LancasterStemmer
from nltk.stem import WordNetLemmatizer
from nltk.tokenize import word_tokenize, sent_tokenize
from nltk.corpus import stopwords
import re
import multiprocessing
import random
import random
import time
from pandas import DataFrame


# Tokenizing (Document to list of sentences. Sentence to list of words.)
def tokenize(str):
	'''Tokenizes into sentences, then strips punctuation/abbr, converts to lowercase and tokenizes words'''
	return 	[word_tokenize(" ".join(re.findall(r'\w+', t,flags = re.UNICODE | re.LOCALE)).lower()) 
			for t in sent_tokenize(str.replace("'", ""))]

#Removing stopwords. Takes list of words, outputs list of words.
def remove_stopwords(l_words, lang='english'):
	l_stopwords = stopwords.words(lang)
	content = [w for w in l_words if w.lower() not in l_stopwords]
	return content
		
#Clean HTML / strip tags TODO: remove page boilerplate (find main content), support email, pdf(?)
def html2text(str):
	return clean_html(str)
		
#Stem all words with stemmer of type, return encoded as "encoding"
def stemming(words_l, type="PorterStemmer", lang="english", encoding="utf8"):
	supported_stemmers = ["PorterStemmer","SnowballStemmer","LancasterStemmer","WordNetLemmatizer"]
	if type is False or type not in supported_stemmers:
		return words_l
	else:
		l = []
		if type == "PorterStemmer":
			stemmer = PorterStemmer()
			for word in words_l:
				l.append(stemmer.stem(word).encode(encoding))
		if type == "SnowballStemmer":
			stemmer = SnowballStemmer(lang)
			for word in words_l:
				l.append(stemmer.stem(word).encode(encoding))
		if type == "LancasterStemmer":
			stemmer = LancasterStemmer()
			for word in words_l:
				l.append(stemmer.stem(word).encode(encoding))
		if type == "WordNetLemmatizer": #TODO: context
			wnl = WordNetLemmatizer()
			for word in words_l:
				l.append(wnl.lemmatize(word).encode(encoding))
		return l

#The preprocess pipeline. Returns as lists of tokens or as string. If stemmer_type = False or not supported then no stemming.		
def preprocess_pipeline(str, lang="english", stemmer_type="PorterStemmer", return_as_str=False, 
						do_remove_stopwords=False, do_clean_html=False):
	l = []
	words = []
	if do_clean_html:
		sentences = tokenize(html2text(str))
	else:
		sentences = tokenize(str)
	for sentence in sentences:
		if do_remove_stopwords:
			words = remove_stopwords(sentence, lang)
		else:
			words = sentence
		words = stemming(words, stemmer_type)
		if return_as_str:
			l.append(" ".join(words))
		else:
			l.append(words)
	if return_as_str:
		return " ".join(l)
	else:
		return l


def csv_extract_col(csvinput, colname, key):
    """ extract a named column from a csv stream into a dictionary
          colname:  name of columm to extract
          key:  name of another columm to use as keys in returned dict
    """
    col = {}
    for row in csv.DictReader(csvinput):
        col[row[key]] = row[colname]
    return col

import pandas as pd
# consider using a dictionary to get a better score where terms from both train and test are present
def main(train_file, test_file):
  #print "loading data.."
  csv.field_size_limit(1310720)
  trainreader = csv.reader (open( '/home/kiran/kdd/train.csv' ))
  projectid, traindata_old = zip (*trainreader)  

  testreader = csv.reader (open ('/home/kiran/kdd/test.csv'))
  projectid, testdata_old = zip (*testreader)


  # remove stopwords
  traindata = []
  testdata = []
  for observation in traindata_old:
      traindata.append(preprocess_pipeline(observation, "english", "PorterStemmer", True, True, False))
  for observation in testdata_old:
      testdata.append(preprocess_pipeline(observation, "english", "PorterStemmer", True, True, False))

  tfv = CountVectorizer (binary=1,ngram_range=(1, 1))
  X_all = traindata + testdata
  lentrain = len(traindata)
  tfv.fit(X_all)
  X_all = tfv.transform(X_all)
  X = X_all[:lentrain]
  X_test = X_all[lentrain:]
  scipy.io.mmwrite ('x_train_bin_1gram.mtx', X, field = 'real')
  scipy.io.mmwrite ('x_test_bin_1gram.mtx', X_test, field = 'real')
  myCols = tfv.get_feature_names ()
  myCols = DataFrame (myCols)
  myCols.to_csv ('bin_1gram.csv', index=False)

      
if __name__=="__main__":
    args = { 'train_file':  'train.csv',
             'test_file':   'test.csv' }
    model = main(**args)
