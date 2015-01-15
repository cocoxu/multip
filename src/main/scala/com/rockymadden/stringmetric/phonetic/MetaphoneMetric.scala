package com.rockymadden.stringmetric.phonetic

import com.rockymadden.stringmetric.{StringFilter, StringMetric}
import com.rockymadden.stringmetric.Alphabet.Alpha

/** An implementation of the Metaphone metric. */
class MetaphoneMetric extends StringMetric[DummyImplicit, Boolean] { this: StringFilter =>
	final override def compare(charArray1: Array[Char], charArray2: Array[Char])
		(implicit di: DummyImplicit): Option[Boolean] = {

		val fca1 = filter(charArray1)
		lazy val fca2 = filter(charArray2)

		if (fca1.length == 0 || !(Alpha isSuperset fca1.head) || fca2.length == 0 || !(Alpha isSuperset fca2.head)) None
		else MetaphoneAlgorithm.compute(fca1).filter(_.length > 0).flatMap(mp1 =>
			MetaphoneAlgorithm.compute(fca2).filter(_.length > 0).map(mp1.sameElements(_))
		)
	}

	final override def compare(string1: String, string2: String)(implicit di: DummyImplicit): Option[Boolean] =
		compare(string1.toCharArray, string2.toCharArray)
}

object MetaphoneMetric {
	private lazy val self = apply()

	def apply(): MetaphoneMetric = new MetaphoneMetric with StringFilter

	def compare(charArray1: Array[Char], charArray2: Array[Char]) = self.compare(charArray1, charArray2)

	def compare(string1: String, string2: String) = self.compare(string1, string2)
}
