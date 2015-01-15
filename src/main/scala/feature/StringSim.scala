package multip.feature

import com.rockymadden.stringmetric.similarity._
import scala.collection.immutable.HashSet
import scala.collection.immutable.HashMap
import scala.io.Source

object StringSim {

	

	val a_an_one = new HashSet() ++ List("a", "an", "one")
	val beWords = new HashSet() ++ List("be", "is", "was", "were", "are", "been", "s", "being")
	val doWords = new HashSet() ++ List("do", "does", "did")
	val haveWords = new HashSet() ++ List("have", "has", "had")
	val getWords = new HashSet() ++ List("get", "got", "getting", "gets", "getting")
	val goWords = new HashSet() ++ List("go", "goes", "going", "gone", "went", "goin")
	val followWords = new HashSet() ++ List("follow", "follows", "following", "followed")
	val pleaseWords = new HashSet() ++ List("please", "pls")
	
	var tmpDict = new HashMap[String, String]()
	
	val normDictFile = "./resources/bohan_norm_lexicon_emnlp2012.txt"
	for(line <- Source.fromFile(normDictFile).getLines()) {
		val cols = line.trim().split('\t')
		if (cols.length == 2) {
			tmpDict += (cols(0) -> cols(1))
		}
	}
	
	val normDict = tmpDict
	
	def normalizedByDictionary(w:String): String = {
		
		var t = w.replaceAll("a+", "a").replaceAll("b+", "b").replaceAll("c+", "c")
		t = t.replaceAll("d+", "d").replaceAll("e+", "e").replaceAll("f+", "f")
		t = t.replaceAll("g+", "g").replaceAll("h+", "h").replaceAll("i+", "i")
		t = t.replaceAll("j+", "j").replaceAll("k+", "k").replaceAll("l+", "l")
		t = t.replaceAll("m+", "m").replaceAll("n+", "n").replaceAll("o+", "o")
		t = t.replaceAll("p+", "p").replaceAll("q+", "q").replaceAll("r+", "r")
		t = t.replaceAll("s+", "s").replaceAll("t+", "t").replaceAll("u+", "u")
		t = t.replaceAll("v+", "v").replaceAll("w+", "w").replaceAll("x+", "x")
		t = t.replaceAll("y+", "y").replaceAll("z+", "z")
		
		if (normDict.contains(t)) {
			return normDict(t)
		}
		
		return t
	}
	

	def specialPOS(t:String, p:String): String = {
	    if (a_an_one.contains(t))
	        return "AN"
	    if (beWords.contains(t))
	        return "BE"
	    if (doWords.contains(t))
	        return "DO"
	    if (haveWords.contains(t))
	        return "HA"
	    if (getWords.contains(t))
	        return "GE"
	    if (goWords.contains(t))
	        return "GO"
	    //if (followWords.contains(t))
	    //    return "FL"
	    //if (pleaseWords.contains(t))
	    //    return "PL"
	    return p
	}	
	
	def specialMatch(t1:String, t2:String): String = {
	    if (a_an_one.contains(t1) && a_an_one.contains(t2))
	        return "AN"
	    if (beWords.contains(t1) && beWords.contains(t2))
	        return "BE"
	    if (doWords.contains(t1) && doWords.contains(t2))
	        return "DO"
	    if (haveWords.contains(t1) && haveWords.contains(t2))
	        return "HA"
	    return null
	}
	
	def samePrefixes (t1:String, t2:String): Boolean = {
		if (commonPrefix(t1, t2) >= 3) {
			return true
		} 
		
		return false
		
	}
	
	def sameSuffixes (t1:String, t2:String): Boolean = {
		if (commonSuffix(t1, t2) >= 3) {
			return true
		} 
		
		return false
		
	}
	
	def isContentWord(word:String, pos:String) : Boolean = {
		if (pos.length() < 2) {
			return false
		} else {
			val npos = specialPOS(word, pos.substring(0,2))
			if (npos == "VB" || npos == "NN" || npos == "JJ" || npos == "RB" || npos == "US"  || npos == "HT") {
				return true
			}
		
		}
		
		return false
	}
	
	def similarWords (t1:String, t2:String): Boolean = {
		val threshold = 0.9
		
		if (JaroWinklerSim(t1, t2) > threshold ) {
		//if (JaroWinklerSim(t1, t2) > threshold && t1.length >= 3 && t2.length >= 3 && isContentWord(t1,p1) && isContentWord(t2, p2)) {
			return true
		} else {
			return false
		}
	
	}

	def test() {
		var s1:String = "beast"
		var s2:String = "beastt"
		println(specialMatch(s1,s2))
		println(JaroWinklerSim(s1,s2), Binarization(JaroWinklerSim(s1,s2)))
		println(DiceSorensenSim(s1,s2), Binarization(DiceSorensenSim(s1,s2)))
		println(HammingSim(s1,s2), Binarization(HammingSim(s1,s2)))
		println(JaccardSim(s1,s2), Binarization(JaccardSim(s1,s2)))
		println(LevenshteinSim(s1,s2), Binarization(LevenshteinSim(s1,s2)))
		println(NGram1Sim(s1,s2), Binarization(NGram1Sim(s1,s2)))
		println(NGram2Sim(s1,s2), Binarization(NGram2Sim(s1,s2)))
		println(NGram3Sim(s1,s2), Binarization(NGram3Sim(s1,s2)))
		println(NGram4Sim(s1,s2), Binarization(NGram4Sim(s1,s2)))
		println(PrefixSim(s1,s2), Binarization(PrefixSim(s1,s2)))
		println(SuffixSim(s1,s2), Binarization(SuffixSim(s1,s2)))
	
	}
	

	def JaroWinklerSim(s1:String, s2:String) : Double = {
		return JaroWinklerMetric.compare(s1.toArray, s2.toArray).get
	}
	
	def DiceSorensenSim(s1:String, s2:String) : Double = {
		return DiceSorensenMetric.compare(s1.toArray, s2.toArray)(1).get
	}	

	def HammingSim(s1:String, s2:String) : Double = {
		return HammingMetric.compare(s1.toArray, s2.toArray).getOrElse(0).toDouble
	}
	
	def JaccardSim(s1:String, s2:String) : Double = {
		return JaccardMetric.compare(s1.toArray, s2.toArray)(1).get
	}
	
	def LevenshteinSim(s1:String, s2:String) : Double = {
		return LevenshteinMetric.compare(s1.toArray, s2.toArray).get
	}
	
	def NGram1Sim(s1:String, s2:String) : Double = {
		return NGramMetric.compare(s1.toArray, s2.toArray)(1).getOrElse(0)
	}
	
	def NGram2Sim(s1:String, s2:String) : Double = {
		return NGramMetric.compare(s1.toArray, s2.toArray)(2).getOrElse(0)
	}
	
	def NGram3Sim(s1:String, s2:String) : Double = {
		return NGramMetric.compare(s1.toArray, s2.toArray)(3).getOrElse(0)
	}
	
	def NGram4Sim(s1:String, s2:String) : Double = {
		return NGramMetric.compare(s1.toArray, s2.toArray)(4).getOrElse(0)
	}
	
	def PrefixSim(s1:String, s2:String) : Double = {
		return commonPrefix(s1, s2)
	}
	
	def SuffixSim(s1:String, s2:String) : Double = {
		return commonSuffix(s1, s2)
	}

	def commonPrefix(t1:String, t2:String): Double = {

	    val min = math.min(t1.length(), t2.length())
	    var i = 0
	    while (i < min) {
	        if (t1.charAt(i) != t2.charAt(i))
	            return i
	        i += 1
	    }
	    
	    return i
	    //return min*1.0/Math.max(t1.length(), t2.length())
	}

	def commonSuffix(t1:String, t2:String): Double = {

	    val min = math.min(t1.length(), t2.length())
	    var i = 0
	    while (i < min) {
	        if (t1.charAt(t1.length() - 1 - i) != t2.charAt(t2.length() - 1 - i))
	            return i
	        i += 1
	    }
	    
	    return i
	    //return min*1.0/Math.max(t1.length(), t2.length())
	}
	
	def Binarization(realnumber:Double) : String = {
		var bucket:String = ""
		
		if (realnumber == 0.0) {
			bucket = "0"
		} else if (realnumber <= 1/3) {
			bucket = "1"
		} else if (realnumber <= 2/3) {
			bucket = "2"
		} else if (realnumber < 1.0) {
			bucket = "3"
		} else if (realnumber == 1.0) {
			bucket = "4"
		} else if (realnumber <= 2.0) {
			bucket = "5"
		} else if (realnumber <= 3.0) {
			bucket = "6"
		} else if (realnumber > 3.0) {
			bucket = "7"
		}
		
		return bucket
	}




}

