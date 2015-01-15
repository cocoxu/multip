package multip

import scala.util.Random

import java.io._

import math.abs

import scalala.scalar._;
import scalala.tensor.::;
import scalala.tensor.mutable._;
import scalala.tensor.dense._;
import scalala.tensor.sparse._;
import scalala.library.Library._;
import scalala.library.LinearAlgebra._;
import scalala.library.Statistics._;
import scalala.library.Plotting._;
import scalala.operators.Implicits._;


// There will be one vector parameter (and one parameter for each unique feature) used in the entire MultiP model
// MultiP uses online learning technique, that the parameters would be updated (if needed to be updated) for each single training example. 
abstract class Parameters(val data:SentPairsData) {
	val nRel  = data.nRel
	val nFeat = data.nFeature

	//THETA
	val theta         = DenseMatrix.zeros[Double](nRel,nFeat+1)
	val theta_sum     = DenseMatrix.zeros[Double](nRel,nFeat+1)

	var theta_average = DenseMatrix.zeros[Double](nRel,nFeat+1)

	var nUpdates = 1.0



	def inferAll(dsp:VectorSentencePair):VectorSentencePair
	def inferAll(dsp:VectorSentencePair, useAverage:Boolean):VectorSentencePair
	def inferAllDebug(dsp:VectorSentencePair, useAverage:Boolean):(DenseVector[Double],DenseVector[Double])
	def train(iterations:Int)
	def train(iterations:Int, fw:FileWriter)

	var trainSimple = false
	var updateTheta = true
  
	def computeThetaAverage {
    	if(Constants.TIMING) {
      		Utils.Timer.start("computeThetaAverage")
    	}

    	theta_average = theta - (theta_sum / nUpdates)

    	if(Constants.TIMING) {
      		Utils.Timer.stop("computeThetaAverage")
    	}
	}

	def resetTheta {
		theta         := 0.0
		theta_sum     := 0.0
		theta_average := 0.0
		nUpdates      = 0.0
	}

	def printTheta {
		if(Constants.TIMING) {
			Utils.Timer.start("printTheta")
		}

		val thetaSorted = theta.argsort.reverse
		println("**** THETA ******")

		val n = nFeat
		for(i <- 0 until n) {
			val (r,f) = thetaSorted(i)
			println(data.relVocab(r) + "\t" + data.featureVocab(f) + "\t" + theta(r,f))
		}

		if(Constants.TIMING) {
			Utils.Timer.stop("printTheta")
		}
	}

	def dumpTheta(outFile:String) {
		if(Constants.TIMING) {
			Utils.Timer.start("dumpTheta")
		}
    
		val fw = new FileWriter(outFile)

		val NON_PARAPHRASE = 0
		var thetaSorted = theta.argsort.reverse.filter((rf) => rf._1 != NON_PARAPHRASE)
		thetaSorted = thetaSorted.sortBy((rf) => -math.abs(theta(rf._1,rf._2)))

    	for(i <- 0 until 10000) {
      		val (r,f) = thetaSorted(i)
      		fw.write(data.relVocab(r) + "\t" + data.featureVocab(f) + "\t" + theta(r,f) + "\n")
    	}

		fw.close()

		if(Constants.TIMING) {
      		Utils.Timer.stop("dumpTheta")
    	}
	}

	def updateTheta(iAll:VectorSentencePair, iHidden:VectorSentencePair) {
    	if(Constants.TIMING) {
			Utils.Timer.start("updateTheta")
		}

    	//Update le weights
    	for(m <- 0 until iAll.features.length) {
      		if(iAll.z(m) != iHidden.z(m)) {
				theta(iHidden.z(m),::)     :+= iHidden.features(m)
				theta(iAll.z(m),   ::)     :-= iAll.features(m)

				theta_sum(iHidden.z(m),::) :+= (nUpdates :* iHidden.features(m))
				theta_sum(iAll.z(m),   ::) :-= (nUpdates :* iAll.features(m))
			}
		}

    	nUpdates += 1.0

    	if(Constants.TIMING) {
      		Utils.Timer.stop("updateTheta")
    	}
 	}

}




class MultiP(data:SentPairsData) extends Parameters(data) {

	
	def train(nIter:Int) = {
		train(nIter, null)
	}

	def train(nIter:Int, outFile:FileWriter) = { 
		//Randomly permute the training data
		val training = Random.shuffle((0 until data.data.length).toList).filter((s12) => true)
		
		for(i <- 0 until nIter) {
			//println("iteration " + i)
			for(s12 <- training) {
				//Run the two inference algorithms
				val iAll = inferAll(data.data(s12))
				val iHidden = inferHidden(data.data(s12))

				data.data(s12).z = iHidden.z
				updateTheta(iAll, iHidden)
				

			}
		}
	}

	def inferHidden(dsp:VectorSentencePair):VectorSentencePair = {
		if(Constants.TIMING) {
			Utils.Timer.start("inferHidden")
		}

		val z      = DenseVector.zeros[Int](dsp.features.length)
		val zScore = DenseVector.zeros[Double](dsp.features.length)
		val postZ  = DenseMatrix.zeros[Double](dsp.features.length, data.nRel)

		//First pre-compute postZ
		for(i <- 0 until dsp.features.length) {
			postZ(i,::) := (theta * dsp.features(i)).toDense
			
			//normalize (note: this isn't necessary, except for analysis purposes and generating P/R curve on training data)
			val logExpSum = MathUtils.LogExpSum(postZ(i,::).toArray)
			postZ(i,::) -= logExpSum
		}
		dsp.postZ = postZ

		val REL_NON_PARAPHRASE = 0
		val REL_PARAPHRASE = 1
		
		var best:Int = 0
		
		val covered = DenseVector.zeros[Boolean](dsp.features.length) 
		
		if (dsp.rel(REL_PARAPHRASE) == 1.0) {
			val scores = postZ(::,REL_PARAPHRASE)
			val topscore = scores.max
			
			for(i <- 0 until dsp.features.length) {
				if (scores(i) == topscore) {
					z(i) = REL_PARAPHRASE
					zScore(i) = topscore
				} else {

					z(i)      = postZ(i,::).argmax
					zScore(i) = postZ(i,::).max
      			}
    		}
    		
    		
		} else if (dsp.rel(REL_NON_PARAPHRASE) == 1.0) {
			for(i <- 0 until dsp.features.length) {
				z(i)      = REL_NON_PARAPHRASE
				zScore(i) = postZ(i,REL_NON_PARAPHRASE)
			}
		} else {
			println("ERROR: MultiP.inferHidden - label missing ")
		}

    	
		if(Constants.DEBUG) {
			println("constrained result.z=" + z.toList.map((r) => data.relVocab(r)))
		}

		val result = new VectorSentencePair(dsp, dsp.w1ids, dsp.w2ids, dsp.features, dsp.rel, z, zScore)

		if(Constants.TIMING) {
			Utils.Timer.stop("inferHidden")
		}
		
		result
	}

	def inferAll(dsp:VectorSentencePair):VectorSentencePair = {

		inferAll(dsp, false)

	}

	// The inference algorithm that compute the vector parameter z' as described in our TACL paper
	//    z' is the parameter vector that corresponds to the most likely assignment for each word pair
	def inferAll(dsp:VectorSentencePair, useAverage:Boolean):VectorSentencePair = {
		if(Constants.TIMING) {
			Utils.Timer.start("inferAll")
		}
	
		val z      = DenseVector.zeros[Int](dsp.features.length)
		val postZ  = new Array[SparseVector[Double]](dsp.features.length)
		val zScore = DenseVector.zeros[Double](dsp.features.length)
		val rel    = DenseVector.zeros[Double](data.nRel).t

		if(useAverage) {
			this.computeThetaAverage
		}
		
		for(i <- 0 until dsp.features.length) {
			if(useAverage) {
				postZ(i) = theta_average * dsp.features(i)	
			} else {
				postZ(i) = theta * dsp.features(i)
			}

			z(i) = postZ(i).argmax
			zScore(i) = postZ(i).max

			//Set the aggregate variables
			rel(z(i)) = 1.0
		}

		if(Constants.DEBUG) {
			println("unconstrained result.z=" + z.toList.map((r) => data.relVocab(r)))
		}
    	
		val result = new VectorSentencePair(dsp, dsp.w1ids, dsp.w2ids, dsp.features, rel, z, zScore)

    	if(Constants.TIMING) {
      		Utils.Timer.stop("inferAll")
    	}
    	
    	result
  	}
  	

	// The inference algorithm that compute the vector parameter z* as described in our TACL paper
	//    z* is the parameter vector that corresponds to the most likely assignment for each word pair that respects the sentence-level training label 	
  	def inferAllDebug(dsp:VectorSentencePair, useAverage:Boolean):(DenseVector[Double], DenseVector[Double]) = {
		if(Constants.TIMING) {
			Utils.Timer.start("inferAllDebug")
		}
	
		val z      = DenseVector.zeros[Int](dsp.features.length)
		val postZ  = new Array[SparseVector[Double]](dsp.features.length)
		val zScore = DenseVector.zeros[Double](dsp.features.length)
		val zYesScore =  DenseVector.zeros[Double](dsp.features.length)
		val zNoScore =  DenseVector.zeros[Double](dsp.features.length)
		
		val rel    = DenseVector.zeros[Double](data.nRel).t

		if(useAverage) {
			this.computeThetaAverage
		}
		
		
		for(i <- 0 until dsp.features.length) {
			if(useAverage) {
				postZ(i) = theta_average * dsp.features(i)	
			} else {
				postZ(i) = theta * dsp.features(i)
			}
			
			zNoScore(i) = postZ(i)(0)   
			zYesScore(i) = postZ(i)(1)

			val logExpSum = MathUtils.LogExpSum(List(zNoScore(i), zYesScore(i)).toArray)
			zNoScore(i) -= logExpSum
			zYesScore(i) -= logExpSum

			z(i) = postZ(i).argmax
			zScore(i) = postZ(i).max

			//Set the aggregate variables
			rel(z(i)) = 1.0
		}

		if(Constants.DEBUG) {
			println("unconstrained result.z=" + z.toList.map((r) => data.relVocab(r)))
		}

    	if(Constants.TIMING) {
      		Utils.Timer.stop("inferAll")
    	}
    	
    	return (zYesScore, zNoScore)
    	
  	}
}

