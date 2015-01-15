package multip

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import collection.mutable.Map

object Helper {

	//this function readin the B/I/O tag for named entities (though NE features were not used in MultiP)
	def getNEfromSentence (sentwithtags:String) : Array[(String, Int, Int)] = {
		val tags = sentwithtags.split(" ")
	
		val words  = tags.map(x => x.split('/')(0))
		val netags = tags.map(x => x.split('/')(1))
		
		var ne_buffer = new ArrayBuffer[(String, Int, Int)]()
		
		var cur_ne_name = ""
		var cur_ne_type = ""
		var cur_ne_begin = -1
		var cur_ne_end = -1
		
		
		for (i <- 0 until netags.length) {
			val netag = netags(i)
			val netag0 = netag.charAt(0)

			//println(netag0)

			if (netag0 == 'O') {
				if (cur_ne_begin > 0) {
					cur_ne_end = i - 1
					ne_buffer += Tuple3(cur_ne_name, cur_ne_begin, cur_ne_end)
					cur_ne_name = ""
					cur_ne_type = ""
					cur_ne_begin = -1
					cur_ne_end = -1					
				}
			} else if ( netag0 == 'B' || netag0 == 'I' ) {
				val netagtype = netag.substring(2)
				if (cur_ne_begin > 0) {
					if (netagtype == cur_ne_type) {
						cur_ne_name += " " + words(i)
					} else if (cur_ne_begin > 0) {
						cur_ne_end = i - 1
						ne_buffer += Tuple3(cur_ne_name, cur_ne_begin, cur_ne_end)
						cur_ne_name = words(i)
						cur_ne_type = netagtype
						cur_ne_begin = i
						cur_ne_end = -1							
					} else {
						cur_ne_name = words(i)
						cur_ne_type = netagtype
						cur_ne_begin = i
						cur_ne_end = -1						
					}
				} else {
						cur_ne_name = words(i)
						cur_ne_type = netagtype
						cur_ne_begin = i
						cur_ne_end = -1										
				}			
			}		
		}	
	
		if (cur_ne_begin > 0) {
			ne_buffer += Tuple3(cur_ne_name, cur_ne_begin, cur_ne_end)					
		}	
		
		return ne_buffer.toArray
	
	}



}