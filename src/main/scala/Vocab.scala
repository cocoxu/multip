package multip;

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

// Vocabulary class maps strings to integers for efficiency
class Vocab(minCount:Int) {
  def this() = this(1)

  var string2Int   = new HashMap[String, Int]
  var string2Count = new HashMap[String, Int]
  var int2String   = new HashMap[Int, String]
  val unk = -1
  var nextInt = 0
  var locked = false
  
  def apply(s:String):Int = {
    var str = s

    //if(!string2Int.contains(str) && locked) {
    if((!string2Int.contains(str) || string2Count(str) < minCount) && locked) {
      return -1  //UNK is -1
    }
    else if(!string2Int.contains(str)) {
      string2Int   += str -> nextInt
      int2String   += nextInt -> str
      string2Count += str -> 0
      nextInt += 1
    }
    string2Count(str) += 1
    return string2Int(str)
  }

  def apply(i:Int):String = {
    if(!int2String.contains(i)) {
      "-UNK-"
    } else {
      int2String(i)
    }
  }

  def lock = { 
    locked = true
    this
  }

  def getMinCountVocab = { 
    //locked = true

    val newVocab = new Vocab(minCount)
    for((word, count) <- string2Count) {
      if(count >= minCount) {
	newVocab(word)
	newVocab.string2Count(word) = count
      }
    }

    newVocab.locked = true
    
    //this
    newVocab
  }

  def size() = nextInt
}
