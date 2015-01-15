package multip.feature


import scala.io.Source
import scala.collection.immutable.HashMap

object WordSig {
	
	
	var tmpSigDict = new HashMap[String, Double]()
	
	val sigFile = "./data/trends.train.wordsig"
	
	for(line <- Source.fromFile(sigFile).getLines()) {
		val cols = line.trim().split('\t')
		if (cols.length == 4) {
			val keyword = cols(0) + "\t" + cols(2)
			val wordsig = cols(3).toDouble
			tmpSigDict += (keyword -> wordsig)
		}
	}

	val sigFile2 = "./data/trends.test.wordsig"
	
	for(line <- Source.fromFile(sigFile2).getLines()) {
		val cols = line.trim().split('\t')
		if (cols.length == 4) {
		    val trendid = cols(0).toInt - 1
			val keyword = "train" + trendid.toString + "\t" + cols(2)
			val wordsig = cols(3).toDouble
			tmpSigDict += (keyword -> wordsig)
		}
	}
	val sigDict = tmpSigDict	

	
	def getWordSiginTrend(word:String, trendid:String): Double = {
		val keyword = trendid + "\t" + word
		if (sigDict.contains(keyword) ){
			return sigDict(keyword)
		} 
		
		return -1.0	
	}

}


