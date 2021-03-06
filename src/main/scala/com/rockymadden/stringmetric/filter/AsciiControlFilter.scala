package com.rockymadden.stringmetric.filter

import com.rockymadden.stringmetric.StringFilter

/** Ensures ASCII controls do not matter. */
trait AsciiControlFilter extends StringFilter {
	abstract override def filter(charArray: Array[Char]): Array[Char] =
		super.filter(charArray.filter(c => !(c <= 31 || c == 127)))

	abstract override def filter(string: String): String = filter(string.toCharArray).mkString
}
