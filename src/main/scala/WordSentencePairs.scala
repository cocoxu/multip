package multip;

import multip.feature._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import Array._

import java.util.regex.Pattern

import edu.washington.cs.knowitall.morpha._

import scalala.tensor.dense._;
import scalala.tensor.sparse._;


/** Class RawWordPair:
 *    a class that extracts features for a word pair (iword1 and iword2) of sentence pair (rawspair)
 */
class RawWordPair (iword1:Int, iword2:Int, rawspair:RawSentencePair) {

	var rawfeatures:Array[String] = new Array[String](0)

	val len1 = rawspair.owords.length
	val len2 = rawspair.cwords.length
	
	
	val word1 = rawspair.owords(iword1)
	val word2 = rawspair.cwords(iword2)
	val pos1 = StringSim.specialPOS(word1, rawspair.oposs(iword1))
	val pos2 = StringSim.specialPOS(word2, rawspair.cposs(iword2))
	val stem1 = rawspair.ostems(iword1)
	val stem2 = rawspair.cstems(iword2)
	
	val npos1 = if(pos1.length() >= 2) {pos1.substring(0,2)} else {pos1}
	val npos2 = if(pos2.length() >= 2) {pos2.substring(0,2)} else {pos2}
	

	val nword1 = rawspair.onwords(iword1)
	val nword2 = rawspair.cnwords(iword2)
	val nstem1 = rawspair.onstems(iword1)
	val nstem2 = rawspair.cnstems(iword2)
		
	val lexicalized = false
	

	this.extract_Unlexicalized_Complex_Features()

	def extract_Unlexicalized_Complex_Features() {
	
		val wordfeatures = this.extract_Word_Features()
		val posfeatures = this.extract_POS_Features()		
		val word_posfeatures = this.combineFeatures(wordfeatures, posfeatures)
				
		//val sig_features = this.extract_WordSig_Features()
		//val word_sigfeatures = this.combineFeatures(wordfeatures, sig_features)
		//val word_pos_sigfeatures = this.combineFeatures(word_posfeatures, sig_features)

		this.rawfeatures = concat (this.rawfeatures, word_posfeatures)
		//this.rawfeatures = concat (this.rawfeatures, word_sigfeatures)
		//this.rawfeatures = concat (this.rawfeatures, word_pos_sigfeatures)
	
	}

	def combineFeatures (fset1:Array[String], fset2:Array[String]) : Array[String] = {	
		val rawfeaturesbuffer = new ArrayBuffer[String]()
		
		for (f1 <- fset1) {
			for (f2 <- fset2) {
				val combinedfeature = f1 + "_" + f2
				rawfeaturesbuffer += combinedfeature			
			}
		}
	
		return rawfeaturesbuffer.toArray
	}
	
	def extract_Word_Features(): Array[String] = {
		val rawfeaturesbuffer = new ArrayBuffer[String]()
		if (word1 == word2) {
			rawfeaturesbuffer += "sameword" 	
		} else if (stem1 == stem2) {
			rawfeaturesbuffer += "samestem"				
		} else if (nword1 == nword2) {
			rawfeaturesbuffer += "samenword"			
		} else if (nstem1 == nstem2) {
			rawfeaturesbuffer += "samenstem"	
		} else if (StringSim.similarWords(word1, word2)) {
			rawfeaturesbuffer += "similarword"					
		} else {
			rawfeaturesbuffer += "diffword"					
		}
		
		return rawfeaturesbuffer.toArray
	}
	

	def extract_POS_Features(): Array[String] = {
		val rawfeaturesbuffer = new ArrayBuffer[String]()
		if (npos1 == npos2){
			rawfeaturesbuffer += "samepos" + npos1.toLowerCase()
		} else if (word1 == word2) {
			rawfeaturesbuffer += "diffpossameword"
		} else {	
			rawfeaturesbuffer += "singlepos" + npos1.toLowerCase()
			rawfeaturesbuffer += "singlepos" + npos2.toLowerCase()	

			rawfeaturesbuffer += "diffpos"
		} 

		return rawfeaturesbuffer.toArray
	}

	
	def extract_WordSig_Features(): Array[String] = {
		val rawfeaturesbuffer = new ArrayBuffer[String]()
		
		val trendid = this.rawspair.trendid
		
		val sig1 = WordSig.getWordSiginTrend(word1, trendid)
		val sig2 = WordSig.getWordSiginTrend(word2, trendid)
		
		if (sig1 > 10000.0 && sig2 > 5000.0) {
			rawfeaturesbuffer += "bothsupersig"
		} else if (sig1 > 1000.0 && sig2 > 1000.0) {
			rawfeaturesbuffer += "bothverysig"
		} else if (sig1 > 500.0 && sig2 > 500.0) {
			rawfeaturesbuffer += "bothsig"
		} else {
			rawfeaturesbuffer += "notbothsig"
		} 
		
		return rawfeaturesbuffer.toArray
	}

	override def toString() :String = {
	
		var output = word1 + " (" + stem1 + ") | " + word2 + " (" + stem2 + ")"
		output += " |" 
		for (rfeature <- this.rawfeatures) {
			output += " "+ rfeature
		}
		return output
	}
	
}

abstract class SuperRawSentencePair  {
	val rawwordpairs:Array[RawWordPair]
	val rawfeatureset:Set[String]
	
	val origsent:String
	val candsent:String
	
	val trendid:String
	val trendname:String

	val amtjudge:Option[Boolean]
	val expertjudge:Option[Boolean]
	
	val owords:Array[String]
	val cwords:Array[String]	
	val oposs:Array[String]
	val cposs:Array[String]
	val ostems:Array[String]
	val cstems:Array[String]

	val onwords:Array[String]
	val cnwords:Array[String]	
	val onstems:Array[String]
	val cnstems:Array[String]
		
	val valid:Boolean //both sentences match the topic names
}


/** Class SentPairsData: 
 *    A data structure describe a sentence pair with all original information in String 
 *    format that is used to read-in original data from (text format) annotation file and 
 *    then covert to features into class SentPairsData.
 */
class RawSentencePair (val trendid:String, val trendname:String, val origpossent:String, val candpossent:String, amtstr:String, expertstr:String) extends SuperRawSentencePair {

	// Read in amt/expert judgement scores, and convert to binary true/false judgements 
	// AMT - amazon mechanical turk labels, uses different cutoffs from EXPERT annotation
	val AMT_JUDGE_HIGH_THRESHOLD = 3
	val AMT_JUDGE_LOW_THRESHOLD = 1
	val EXPERT_JUDGE_HIGH_THRESHOLD = 4
	val EXPERT_JUDGE_LOW_THRESHOLD = 2
	
	var tmp_amtjudge:Option[Boolean] = None
	if (amtstr.charAt(0) == '(') { 
		val amttmp = amtstr.charAt(1).asDigit 
		if (amttmp >= AMT_JUDGE_HIGH_THRESHOLD) {
			tmp_amtjudge = Some(true) 
		} else if (amttmp <= AMT_JUDGE_LOW_THRESHOLD) {
			tmp_amtjudge = Some(false)
		}
	} 
	val amtjudge = tmp_amtjudge
	
	var tmp_expertjudge:Option[Boolean] = None
	if (expertstr != null) {
		val experttmp = expertstr.toInt
		if (experttmp >= EXPERT_JUDGE_HIGH_THRESHOLD) {
			tmp_expertjudge = Some(true) 
		} else if (experttmp <= EXPERT_JUDGE_LOW_THRESHOLD) {
			tmp_expertjudge = Some(false)
		}
	}	
	val expertjudge = tmp_expertjudge
	
	
	// create words/poss/stems arrays from input sentences
	val trendnamewords = this.trendname.replace("-","").toLowerCase().split(' ')
	val otmptags = origpossent.split(" ")
	val ctmptags = candpossent.split(" ")
	val ocasewords = otmptags.map(x => x.split('/')(0))
	val ccasewords = ctmptags.map(x => x.split('/')(0))
	val otmpwords = ocasewords.map(x => x.toLowerCase())
	val ctmpwords = ccasewords.map(x => x.toLowerCase())
	val otmpposs = otmptags.map(x => x.split('/')(2))
	val ctmpposs = ctmptags.map(x => x.split('/')(2))	

	
	val origsent = ocasewords.mkString(" ")
	val candsent = ccasewords.mkString(" ")
	
	var owords_buffer = new ArrayBuffer[String]()
	var cwords_buffer = new ArrayBuffer[String]()
	var oposs_buffer = new ArrayBuffer[String]()
	var cposs_buffer = new ArrayBuffer[String]()
	
	
	var oindex = 0
	var omatched = false
	while (oindex < otmptags.length) {
		var i = 0
		var matched = false
		if (otmpwords(oindex+i) == trendnamewords(i)) {
			matched = true
			i += 1
			while ( matched && oindex + i < otmptags.length && i < trendnamewords.length) {
				if (otmpwords(oindex+i) != trendnamewords(i)) {
					matched = false
				}
				i += 1
			}
		}
		
		if (matched == true) {
			omatched = true
			owords_buffer += "XXXX"
			oposs_buffer  += "XX"
			oindex += trendnamewords.length
		} else {
			owords_buffer += otmpwords(oindex)
			oposs_buffer  += otmpposs(oindex)
			oindex += 1
		}
	}

	var cindex = 0
	var cmatched = false
	while (cindex < ctmptags.length) {
		var i = 0
		var matched = false
		if (ctmpwords(cindex+i) == trendnamewords(i)) {
			matched = true
			i += 1
			while ( matched && cindex + i < ctmptags.length && i < trendnamewords.length) {
				if (ctmpwords(cindex+i) != trendnamewords(i)) {
					matched = false
				}
				i += 1
			}
		}
		
		if (matched == true) {
			cmatched = true
			cwords_buffer += "XXXX"
			cposs_buffer  += "XXX"
			cindex += trendnamewords.length
		} else {
			cwords_buffer += ctmpwords(cindex)
			cposs_buffer  += ctmpposs(cindex)
			cindex += 1
		}
	}
	
	val followsig = WordSig.getWordSiginTrend("follow", this.trendid)
	
	

	val owords = owords_buffer.toArray
	val cwords = cwords_buffer.toArray
	val oposs = oposs_buffer.toArray
	val cposs = cposs_buffer.toArray	

	var tmpvalid = false
	if (omatched == true && cmatched == true && origsent!=candsent && followsig <= 50000.0) {
		tmpvalid = true
	}
	
	val valid = tmpvalid
			
	
	val ostems = owords map {MorphaStemmer.stemToken(_)}
	val cstems = cwords map {MorphaStemmer.stemToken(_)}	

	val onwords = owords map {StringSim.normalizedByDictionary(_)}
	val cnwords = cwords map {StringSim.normalizedByDictionary(_)}	
	val onstems = onwords map {MorphaStemmer.stemToken(_)}
	val cnstems = cnwords map {MorphaStemmer.stemToken(_)}	

	
	// extract word pairs
	val rawwordpairs = this.extractWordPairsTrick()
	
	//End of the constructor of RawSentencePair
	
	var rawfeatureset_buffer = Set.empty[String]
	for (wpair <- this.rawwordpairs) {
		rawfeatureset_buffer = wpair.rawfeatures.toSet ++ rawfeatureset_buffer	
	}
	val rawfeatureset = rawfeatureset_buffer
	
	//Align the words on each side that share the same stem together
	//for other words, use all possible word pairs
	def extractWordPairsTrick () : Array[RawWordPair] = {
		var wordpairs_buffer:ArrayBuffer[RawWordPair] = new ArrayBuffer[RawWordPair]()
		
		val stemincommon = this.ostems.toSet & this.cstems.toSet
		
		for ( ioword <- 0 until owords.length) {
			val ostem = this.ostems(ioword)
			
			for ( icword <- 0 until cwords.length) {
				val cstem = this.cstems(icword)
			
				if (  (ostem == cstem && stemincommon(ostem) && stemincommon(cstem)) && ostem!="XXXX" || (!stemincommon(ostem) && !stemincommon(cstem))) {
					val wpair = new RawWordPair(ioword, icword, this)
					wordpairs_buffer += wpair		
							
				}			
			}		
		}

		return wordpairs_buffer.toArray
	}
	

	override def toString() :String = {
		var output = this.amtjudge + " | " + this.trendname + " | " + this.origsent + " | " + this.candsent + "\n"
		output += this.owords.mkString(" ") + " | " + this.ostems.mkString(" ") + " | " + this.oposs.mkString(" ") + "\n"
		output += this.cwords.mkString(" ") + " | " + this.cstems.mkString(" ") + " | " + this.cposs.mkString(" ")
		for ( rwpair <- this.rawwordpairs) {
			output += "\n" + rwpair.toString()
		}
		
		output += "\n"
		return output
	}

}


abstract class SuperVectorSentencePair  {
	
	val origsent:String
	val candsent:String
	
	val trendid:String
	val trendname:String

	val amtjudge:Option[Boolean]
	val expertjudge:Option[Boolean]

	val nRel = 2  // number of relations is 2, either paraphrase or not paraphrase.
	
	val IS_PARAPHRASE = 1
	val IS_NOT_PARAPHRASE = 0
	
	var w1ids:Array[Int] = null 
	var w2ids:Array[Int] = null
	var features:Array[SparseVectorCol[Double]] = null
	var rel:DenseVectorRow[Double] = DenseVector.zeros[Double](nRel).t
	var z:DenseVector[Int] = null // Though it is Int, but only will have two possible values 0 or 1
	var zScore:DenseVector[Double] = null

	var postZ:DenseMatrix[Double] = null	

}		

/** Class VectorSentencePair:	
 *   A data structure that describes a sentence pair using compact Vector format
 *   and only carries a few important information in String format 
 *   it can be generated from RawSentencePair
 */
class VectorSentencePair (val trendid:String, val trendname:String, val origsent:String, val candsent:String, val amtjudge:Option[Boolean], val expertjudge:Option[Boolean]) extends SuperVectorSentencePair {
	

	def this (rawsentpair:RawSentencePair) {
		this(rawsentpair.trendid, rawsentpair.trendname, rawsentpair.origsent, rawsentpair.candsent, rawsentpair.amtjudge, rawsentpair.expertjudge)
	}

	
	def this (rawsentpair:RawSentencePair, w1ids:Array[Int], w2ids:Array[Int], features:Array[SparseVectorCol[Double]], useExpert:Boolean) {
		this(rawsentpair.trendid, rawsentpair.trendname, rawsentpair.origsent, rawsentpair.candsent, rawsentpair.amtjudge, rawsentpair.expertjudge)
		
		this.w1ids = w1ids
		this.w2ids = w2ids
		this.features = features
		
		var judge = rawsentpair.amtjudge
		
		if (useExpert == true) {
			judge = rawsentpair.expertjudge
		}
		
		if (judge == Some(true)) {
			this.rel(IS_PARAPHRASE) = 1.0
		} else {
			this.rel(IS_NOT_PARAPHRASE) = 1.0
		}
	}
	
	
	def this (vsentpair:VectorSentencePair, w1ids:Array[Int], w2ids:Array[Int], features:Array[SparseVectorCol[Double]], rel:DenseVectorRow[Double], z:DenseVector[Int], zScore:DenseVector[Double]) {
		this(vsentpair.trendid, vsentpair.trendname, vsentpair.origsent, vsentpair.candsent, vsentpair.amtjudge, vsentpair.expertjudge)

		
		this.w1ids = w1ids
		this.w2ids = w2ids
		this.features = features
		this.rel = rel
		this.z = z
		this.zScore = zScore

	}


	override def toString() :String = {
		var output = "YesPara = " + this.rel(IS_PARAPHRASE) + " | " + "NonPara = " + this.rel(IS_NOT_PARAPHRASE) + " | " + this.trendname + " | " + this.origsent + " | " + this.candsent

		output += "\n"
		return output
	}	
	
}


/* Class SentPairsData: 
 *   a data structure of raw data read from files and converted into vector presentation for efficiency
 *
 * Main Function:
 *   readinFromAnnotationFile (inFile:String, useExpert:Boolean, trainData:SentPairsData) 
 *     - inFile: the data file name 
 *     - trainData: 
 *          The internal data structure uses vector representations.
 *          When reading in training data, set the input parameter "trainData" as "null", 
 *          and a new mapping of features to their vector index (featureVocab) will be built. 
 *          Next, when reading in the test data, point the input paramater "trainData" to
 *          the training data instance (of type SentPairsData) you just created, and 
 *          the feature vectors for test data will be created, using the same feature set
 *          of the training data.
 */ 
class SentPairsData(inFile:String, useExpert:Boolean, trainData:SentPairsData)  {
	
	var data:Array[VectorSentencePair] = null

	val IS_PARAPHRASE = 1
	val IS_NOT_PARAPHRASE = 0
		
	val nRel:Int = 2  // only 2 different labels for sentence pairs, either paraphrase or not 
	var nFeature:Int = 0 
	var nSentPairs:Int = 0
	
	var sentVocab:Vocab = null
	var wordVocab:Vocab = null
	var featureVocab:Vocab = null
	
	val relVocab = Array("NonPara", "YesPara")
	
	val NGRAM_PHRASE_PAIR = 1
	val N_FEATURE_CUTOFF = 3
	
	val usePOS = true
	
	this.readinFromAnnotationFile(inFile, useExpert, trainData)
	
	def readinFromAnnotationFile (inFile:String, useExpert:Boolean, trainData:SentPairsData) {
	
		
		this.sentVocab = new Vocab
		this.wordVocab = new Vocab
		
		if (trainData == null) {
			this.featureVocab = new Vocab
		} else {
			this.featureVocab = trainData.featureVocab
		}
	
		var rawsentpairs = new ArrayBuffer[RawSentencePair]()
		var rawfeaturecounter = Map.empty[String,Int]
	
		println("Read In Data From Annotation File: " + inFile)
		
		
		//First pass: Read In the original annotation file one line at each time (one sentence pair per line)
		var nLines = 0
		for(line <- Source.fromFile(inFile).getLines()) {

			nLines += 1
			if(nLines % 1000 == 0) {
				println("    read " + nLines + " lines")
			}

			val cols = line.toLowerCase().trim().split('\t')
			var rsentpair:RawSentencePair = null

		
		
			//Read In one sentence pair from original annotation file
			if (usePOS) {
		  		if (cols.length == 7 || cols.length == 8) {	

					//class RawSentencePair (val trendid:String, val trendname:String, val origpossent:String, val candpossent:String, amtstr:String, expertstr:String) 
					 
					if (cols.length == 7) {
						// 7-column format: only Amazon Mechanical Turk label
						rsentpair = new RawSentencePair(cols(0), cols(1), cols(5), cols(6), cols(4), null)
					} else if (cols.length == 8) {
						// 8-column format: both Amazon Mechanical Turk and Expert label
						
						// Note!! the "test" prefix before the trend it (cols(0)) is hard-coded for purpose of 
						// reading in the topic-word significance for the Topical features only.
						// The Topical features were used in our TACL paper, and computed by an external script (not in MultiP) 
						// and saved in the data files, which are read into the MultiP.
						// The "test" prefix is to distinguish the different trend id indices used for training and test data  
						
						// If you want to remove this topical feature (also called Sig feature in the code of MultiP), 
						// you would want to modify the code of class RawWordPair in this file.
						rsentpair = new RawSentencePair("test"+cols(0), cols(1), cols(6), cols(7), cols(4), cols(5))
					}				
				}
				
				
			} 
	
			//Extract phrase pairs and their features for this sentence pair
			if (rsentpair != null && (useExpert == true && rsentpair.expertjudge != None || useExpert == false && useExpert == false && rsentpair.valid == true) ) {
				
				rawsentpairs += rsentpair
				
				//Add the features (count 1 for each sent pair) that appear in this sentence pair to 'this.rawfeaturecounter'
				rawfeaturecounter = rawfeaturecounter ++ rsentpair.rawfeatureset.zip(Stream.continually(1)).toMap.map{ case (k,v) => k -> (v + rawfeaturecounter.getOrElse(k,0)) }			
				
				val s1 = this.sentVocab(rsentpair.origsent)
				val s2 = this.sentVocab(rsentpair.candsent)
				
				for (rwpair <- rsentpair.rawwordpairs) {
					val w1 = this.wordVocab(rwpair.word1)
					val w2 = this.wordVocab(rwpair.word2)					
				}
				
			}
		
		}
	
		//Go over rawfeaturecounter, and filter out features that appear in less than N_FEATURE_CUTOFF sentence pairs
		this.nFeature = 0
		for ((fstr, fcount) <- rawfeaturecounter) {
			if (fcount >= N_FEATURE_CUTOFF) {
				val f = this.featureVocab(fstr)
				this.nFeature += 1
			}
		}

		
		//Second pass:		
		this.featureVocab.lock
		
		val rspairs = rawsentpairs.toArray 
		this.data = new Array[VectorSentencePair](rspairs.length)
		this.nSentPairs = 0
		for (rspair <- rspairs) {
			val w1s = new Array[Int](rspair.rawwordpairs.length)
			val w2s = new Array[Int](rspair.rawwordpairs.length)
			val swfeatures = new Array[SparseVectorCol[Double]](rspair.rawwordpairs.length)			
			
			for(i <- 0 until rspair.rawwordpairs.length) {
				val wpair:RawWordPair = rspair.rawwordpairs(i)
				w1s(i) = this.wordVocab(rspair.rawwordpairs(i).word1)
				w2s(i) = this.wordVocab(rspair.rawwordpairs(i).word2)
				
				swfeatures(i) = SparseVector.zeros[Double](this.featureVocab.size + 1)
      			swfeatures(i)(this.featureVocab.size) = 1.0	//Bias feature
      			
      			for(j <- 0 until wpair.rawfeatures.length) {
					val f = this.featureVocab(wpair.rawfeatures(j))
					//println(f + " " + ppair.rawfeatures(j))
					if(f >= 0) {
	  					swfeatures(i)(f) = 1.0
	  				}
	  			}
			}
			
			this.data(this.nSentPairs) = new VectorSentencePair(rspair, w1s, w2s, swfeatures, useExpert)		
			this.nSentPairs += 1
			
			
		}	
	}
	
	override def toString() :String = {
		var output = ""
		for ( i <- 0 until data.length) {
			val datapoint = this.data(i)
			output += datapoint.rel(0) + " | " + datapoint.rel(1) + " | " + datapoint.trendname + " | " + datapoint.origsent + " | " + datapoint.candsent + "\n"
		}
		return output
	}
	
	def toString(index:Int) :String = {

	
		val datapoint = this.data(index)
		var output = ""
		
		if (datapoint.rel(IS_NOT_PARAPHRASE) == 1.0) {
			output += this.relVocab(IS_NOT_PARAPHRASE)
		} else if (datapoint.rel(IS_PARAPHRASE) == 1.0) {
			output += this.relVocab(IS_PARAPHRASE)
		}
		
		output += " | " + datapoint.trendname + " | " + datapoint.origsent + " | " + datapoint.candsent + "\n"
		for (i <- 0 until datapoint.features.length) {
			output += "WordPair #" + i + " : " + this.wordVocab(datapoint.w1ids(i)) + " | " + this.wordVocab(datapoint.w2ids(i)) + " | " 
			
			val strfeatures = Utils.bin2int(datapoint.features(i).toArray).map((f) => this.featureVocab(f))
			output += strfeatures.mkString(" ")
			output += "\n"
		}
		
		return output
	}
}




