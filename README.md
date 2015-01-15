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

