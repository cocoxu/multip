# MultiP

Source code for Multiple-instance Learning Paraphrase (MultiP) Model 

Authors: Wei Xu and Alan Ritter

Contact: xwe@cis.upenn.edu

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


## Relevant Papers

    "Extracting Lexically Divergent Paraphrases from Twitter"
    Wei Xu, Alan Ritter, Chris Callison-Burch, William B. Dolan and Yangfeng Ji
    In Transactions of the Association for Computational Linguistics (TACL) 2014
    http://tacl2013.cs.columbia.edu/ojs/index.php/tacl/article/view/498
  
