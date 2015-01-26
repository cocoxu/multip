# MultiP

Authors: Wei Xu and Alan Ritter
Contact: xwe@cis.upenn.edu

Source code of the Multiple-instance Learning Paraphrase (MultiP) Model in the following paper:

     "Extracting Lexically Divergent Paraphrases from Twitter"
     Wei Xu, Alan Ritter, Chris Callison-Burch, William B. Dolan and Yangfeng Ji
     In Transactions of the Association for Computational Linguistics (TACL) 2014
     http://tacl2013.cs.columbia.edu/ojs/index.php/tacl/article/view/498
  
  
## PACKAGE 

The package contains the following folders and scripts:

     ./src/          source code for MultiP, in Scala and Java
     ./data/         the train/dev/test data & word significance data used in topical features in the paper
     build.sbt       the config file for Simple Build Tool (sbt)
     run.sh   		the script that compiles and runs MultiP

The package requires sbt and Scalala.
  
To install sbt:

     download it from http://www.scala-sbt.org/ and follow the instructions on its website to install
    
To install Scalala:

     download it by "git clone https://github.com/scalala/Scalala.git"
     then type "sbt publish-local" under the /Scalala/ directory
 
## To run MultiP:
     either type "sbt run" or "./run.sh"     only difference is that run.sh allocates memory space

## CODE 

The directory ./src/main/ contains the source code for MultiP organized as follows:
    
     ./src/main/java/*   external packages from MIT/Sussex/UW for stemming, string similarity etc.
     ./src/main/scala/*  core code for MultiP
          Main.scala       the entrance of the program that trains and tests (main evaluation function) the MultiP model 
          MultiP.scala     the main class of the MultiP algorithms and parameters
          Eval.scala       the secondary evaluation functions that monitors the training progress
          WordSentencePairs.scala  the main data structure, which reads in from data files and extract features.      
          Vocab.scala      mapping tables for converting string type of data to compact vector representation     
          Helper.scala     some minor functions to assist preprocessing the data
          Common.scala     some minor functions to assist computing efficiency
          PINC.scala       additional evaluation metric PINC (not required to run MultiP)
          feature/*.scala  functions to extract various features
          com/*            external package for string metrics

## DATA (email us)

  The dataset contains the following files:
  
    ./data/train.labeled.data  (13063 sentence pairs)
    ./data/dev.labeled.data    ( 4727 sentence pairs)
    ./data/test.labeled.data   (  972 sentence pairs)
    ./data/trends.test.wordsig (precomputed to create topic-level features)
    ./data/trends.train.wordsig (precomputed to create topic-level features)

  Notice that the train and dev data is collected from the same time period and
  same trending topics. In the evaluation later, we will test the system on data
  collected from a different time period.

  Both train/dev data files come in the tab-separated format. Each line contains 6 columns:
    
    Topic_Id | Topic_Name | Sent_1 | Sent_2 | Label | Sent_1_tag | Sent_2_tag |
 
  The "Trending_Topic_Name" are the names of trends provided by Twitter, which are
  not hashtags.
  
  The "Sent_1" and "Sent_2" are the two sentences, which are not necessarily full 
  tweets. Tweets were tokenized by Brendan O'Connor et al.'s toolkit (ICWSM 2010) 
  and split into sentences. 

  The "Sent_1_tag" and "Sent_2_tag" are the two sentences with part-of-speech 
  and named entity tags by Alan Ritter et al.'s toolkit (RANLP 2013, EMNLP 2011). 
 
  The "Label" column is in a format such like "(1, 4)", which means among 5 votes 
  from Amazon Mechanical turkers only 1 is positive and 4 are negative. We would 
  suggest map them to binary labels as follows:
    
    paraphrases: (3, 2) (4, 1) (5, 0)
    non-paraphrases: (1, 4) (0, 5)
    debatable: (2, 3)  which were discard in our experiments
    
  The test data contains an extra column of expert labels. It is a single digit between 
  between 0 (no relation) and 5 (semantic equivalence), annotated by expert.  
  We would suggest map them to binary labels as follows:
    
    paraphrases: 4 or 5
    debatable: 0 or 1 or 2  
    non-paraphrases: 3   (which we discarded in Paraphrase Identification evaluation)



