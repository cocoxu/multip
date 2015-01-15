package multip

import scala.collection.mutable.ListBuffer

object PINC {

	def getRawScore (asent:String, bsent:String) : Double = {
	
		var lasent = asent.toLowerCase().replaceAll("[^\\p{L}\\p{Nd}]", " ")
		lasent = lasent.replaceAll(" +", " ")
		var lbsent = bsent.toLowerCase().replaceAll("[^\\p{L}\\p{Nd}]", " ")
		lbsent = lbsent.replaceAll(" +", " ")
		return getScore(lasent, lbsent)

	}

	def getScore(ssent:String, csent:String) : Double = {
				
		val s1grams = ssent.split(" ")
		val c1grams = csent.split(" ")
		var s2gramsbuf = new ListBuffer[String]()
		var c2gramsbuf = new ListBuffer[String]()
		var s3gramsbuf = new ListBuffer[String]()
		var c3gramsbuf = new ListBuffer[String]()
		var s4gramsbuf = new ListBuffer[String]()
		var c4gramsbuf = new ListBuffer[String]()
		
		for(i <- 0 until s1grams.length-1) { 
        	if (i < s1grams.length - 1) {
            	val s2gram = s1grams(i) + " " + s1grams(i+1)
            	s2gramsbuf += s2gram
            }
            
        	if (i < s1grams.length - 2) {
            	val s3gram = s1grams(i) + " " + s1grams(i+1) + " " + s1grams(i+2)
            	s3gramsbuf += s3gram
        	}
        	
        	if (i < s1grams.length - 3) {
            	val s4gram = s1grams(i) + " " + s1grams(i+1) + " " + s1grams(i+2) + " " + s1grams(i+3)
            	s4gramsbuf += s4gram
			}
		}		
		
		val s2grams = s2gramsbuf.toList
		val s3grams = s3gramsbuf.toList
		val s4grams = s4gramsbuf.toList

		for(i <- 0 until c1grams.length-1) { 
        	if (i < c1grams.length - 1) {
            	val c2gram = c1grams(i) + " " + c1grams(i+1)
            	c2gramsbuf += c2gram
            }
            
        	if (i < c1grams.length - 2) {
            	val c3gram = c1grams(i) + " " + c1grams(i+1) + " " + c1grams(i+2)
            	c3gramsbuf += c3gram
        	}
        	
        	if (i < c1grams.length - 3) {
            	val c4gram = c1grams(i) + " " + c1grams(i+1) + " " + c1grams(i+2) + " " + c1grams(i+3)
            	c4gramsbuf += c4gram
			}
		}		
		
		val c2grams = c2gramsbuf.toList
		val c3grams = c3gramsbuf.toList
		val c4grams = c4gramsbuf.toList

		//println(s1grams.mkString(","))
		//println(c1grams.mkString(","))
		//println(s1grams.intersect(c1grams).mkString(","))
		
		var score = s1grams.intersect(c1grams).length.toDouble / c1grams.length
		
		if (c2grams.length > 0) {
        	score += s2grams.intersect(c2grams).length.toDouble / c2grams.length
        }
        
    	if (c3grams.length > 0) {
        	score += s3grams.intersect(c3grams).length.toDouble / c3grams.length
        }
        
    	if (c4grams.length > 0) {
        	score += s4grams.intersect(c4grams).length.toDouble / c4grams.length
        }
        
    	return 1 - score/4
	}
	
	
}