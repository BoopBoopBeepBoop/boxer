package com.bbbb.boxer

/**
  */
object TableBuilder {

  def apply(t: Iterable[Product]): Iterable[Iterable[String]] = {
    if(t.isEmpty) return Iterable.empty

    val card = t.head.productArity
    val obs = t.map(_.productIterator)

    val longest = Array.fill(card) { 0 }

    val strings = obs.map { row =>
      row.zipWithIndex.map { case (col, index) =>
        val string = col.toString
        if (string.length > longest(index)) {
          longest(index) = string.length
        }
        string

    // force iterators to traverse bc of side effect in calculation
      }.toList
    }.toList

    val thing = strings.map {
      _.zipWithIndex.map {
        case (str, i) =>
          val filler = (longest(i) + 2) - str.length
          str + (" " * filler)
      }.toIterable
    }

    thing
  }
}
