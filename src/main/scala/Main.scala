package multip

import multip.feature._
import scala.math._

import edu.washington.cs.knowitall.morpha._
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import collection.immutable.ListMap 


object Constants {
  var DEBUG = false
  var TIMING = true
}

object Main {

	// THE entrance function
	def main(args: Array[String]) {
	
		runMultiP()
		
	}

	
	def runMultiP() {
		//number of iterations. normally set to 30 or 50 for a full run. And something like 1 or 5 for debugging. 
		val nIter = 50

		// read in training data, then test data. 
		// the order matters, since test data has to create the features that exist in 
		// the training data and use the same mapping to convert features into vector representations. 
		val trainlabel = false // false - use Turker's label; true - use expert's label
		
		val trainfile = "./data/train.labeled.data"
		val traindata = new SentPairsData(trainfile, trainlabel, null) 
		println("size of train data = " + traindata.data.length)
		
		val testlabel = true // false - use Turker's label; true - use expert's label
		val testfile = "./data/test.labeled.data"
		val testdata = new SentPairsData(testfile, testlabel, traindata) 
		println("size of test data = " + testdata.data.length)
		
		// build the MultiP (Multiple-instances Learning Paraphrase Model)
		val multip = new MultiP(traindata)
		EvalIterations(multip, nIter, testdata, testlabel)
      
	}
	
	
	// The main process for training and testing the MultiP model 
	def EvalIterations(model:Parameters, nIter:Int, test:SentPairsData, againstexpertlabel:Boolean) {

		///  Training  ///
	
		for(i <- 0 to nIter) {

    		println("****************** iteration " + i + "******************")

    		model.train(1, null)
    		
    		// This Eval(uation) instance is mainly used for monitoring the performance of trained model at each iteration
    		Eval.useAveragedParameters = true
    		
    		//Test on the training dataset
    		println("Evaluate on the training data:")
    		Eval.AggregateEval(model, model.data)
    		//Test on the test dataset
    		println("Evaluate on the test data:")
    		Eval.AggregateEval(model, test)
    		
    		
    	}
    	
    	
    	///  Testing #0   showing the weights of each feature   ///

    	model.printTheta
		
		
		
		///  Testing #1   showing the model outputs for each input test sentence pair   ///

		// use the parameters averaged from all iterations
		val useAveragedParameters = true  
		// or use only the parameter of the last iteration
		//val useAveragedParameters = false
		
				
		val OUTPUT_TOP_PHRASE_PAIR = 10    	
		
    	//val datadata = model.data  // uncomment if want to do close test (test on training data)
    	val datadata = test          // uncomment if want to do open test (test on the test data)
    	
    	var sysoutputs:Map[VectorSentencePair, Double] = Map() 
    	
    	var totalgoldpos = 0.0
    	
    	var sysscores = new ArrayBuffer[Double]()
    	
    	for(j <- 0 until datadata.data.length) {
    		val datapoint:VectorSentencePair = datadata.data(j)
    		  		
    		val (yespairscores, nopairscores ) = model.inferAllDebug(datapoint, useAveragedParameters)
    		var output = ""

			val predicted = model.inferAll(datapoint, useAveragedParameters)
			val r = model.data.IS_PARAPHRASE
			val prediction = predicted.rel(r)
			var score = 0.0
			
			val goldlabel = if(againstexpertlabel) datapoint.expertjudge.getOrElse(false) else datapoint.amtjudge.getOrElse(false)

			
			if (goldlabel == true) {
				totalgoldpos += 1.0
			}
			
			
			if (prediction == 1.0) {
				print("SYS = YESPARA | " )
			} else {
				print("SYS = NOTPARA | " )
			}
			
			print(datapoint.toString)
 
			//for (k <- 0 until OUTPUT_TOP_PHRASE_PAIR) {
			for (k <- 0 until datapoint.features.length) {
   				val top = yespairscores.argmax
   				
   				if (k == 0) {
   					score = exp(yespairscores(top))		
    			}
    			
				output += "WordPair #" + top + " : " + exp(yespairscores(top)) + " | "  + exp(nopairscores(top)) + " | " + datadata.wordVocab(datapoint.w1ids(top)) + " | " + datadata.wordVocab(datapoint.w2ids(top)) + " | " 
			
				val strfeatures = Utils.bin2int(datapoint.features(top).toArray).map((f) => model.data.featureVocab(f))
				output += strfeatures.mkString(" ")
				output += "\n"
				yespairscores(top) = Double.NegativeInfinity
    		}
	
			sysoutputs += ( datapoint -> score)
    		println(output)
    		
    		sysscores += score
    		    
    	}
    	
 		///  Testing #2   showing the system outputs in SemEval 2015 PIT shared task format   ///
   	
    	val dff = new java.text.DecimalFormat("#.####")
    	val sysscorearray = sysscores.toArray
    	for (j <- 0 until datadata.data.length) {
    		val sysscore = sysscorearray(j)
    		val datapoint:VectorSentencePair = datadata.data(j)
    		if (sysscore > 0.0001d) {
    			println("true\t" + dff.format(sysscore) + "\t" + datapoint.origsent + "\t" + datapoint.candsent)
    		} else {
    			println("false\t" + dff.format(sysscore) + "\t" + datapoint.origsent + "\t" + datapoint.candsent)
    		}
    	}


    	///  Testing #3   model performance aggregated on the test dataset   ///
    	///               showing Precision/Recall curve, and max F1 point, and PINC scores etc  ///
    	
    	
    	// PRINT PR Curve with PINC score
    	var tp = 0.0
    	var fp = 0.0
    	var tn = 0.0
    	var fn = 0.0
    	var totalpinc = 0.0
    	
    	
    	
    	println("size of test data (count only unique) = " + sysoutputs.toList.length)
    	println( "RANK\t\tPRECIS.\tRECALL\tF1\tHIT-PINC|||\tPINC\tMultiP\tSENT1\tSENT2")
    	
    	val sortedoutoputs = ListMap(sysoutputs.toList.sortBy{-_._2}:_*)
    	var i = 0
    	
    	var maxfscore = 0.0
    	var maxfoutput = ""
    	
    	val df = new java.text.DecimalFormat("#.###")
    	
    	for ((paradata, parascore) <- sortedoutoputs) {
    		
    		var strhit = "HIT"
    		
    		val sent1 = paradata.origsent
    		val sent2 = paradata.candsent
    		
    		if (sent1 != sent2) {
    			i += 1
    		
    		
				val predicted = model.inferAll(paradata, useAveragedParameters)
				val prediction = predicted.rel(model.data.IS_PARAPHRASE)
			
			
				val goldlabel = if(againstexpertlabel) paradata.expertjudge.getOrElse(false) else paradata.amtjudge.getOrElse(false)
			
				val pincscore = PINC.getRawScore(sent1, sent2)
			
				if (goldlabel == true) {
					tp += 1
					totalpinc += pincscore
				} else {
					fp += 1
					strhit = "ERR"
				}
			
			
				val precision = tp / (tp + fp)
				val recall = tp / totalgoldpos
				val fscore = 2 * precision * recall / (precision + recall)
							
				val avgpinc = totalpinc / tp
						
				var output = i + "\t" + strhit + "\t" + df.format(precision) + "\t" + df.format(recall) + "\t" + df.format(fscore) + "\t" + df.format(avgpinc) + "\t|||\t"
				output += df.format(pincscore) + "\t"+ df.format(parascore) + "\t" + sent1 + "\t" + sent2
				println(output)

				if (fscore > maxfscore) {
					maxfscore  = fscore
					maxfoutput = output
				}
				    		
    		}

    		
    	}
      		
    	println()
    	println ("MAX" + maxfoutput)  	
    	
    }
    

    
}


