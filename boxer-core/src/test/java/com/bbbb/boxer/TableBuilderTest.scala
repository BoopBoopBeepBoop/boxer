package com.bbbb.boxer

import org.scalatest.{ShouldMatchers, FlatSpec, FunSuite}

case class TestTable(a: String, b: String, c: Int)


/**
  */
class TableBuilderTest extends FlatSpec with ShouldMatchers {

  "TableBuilder" should "build table from case class" in {
    val str = TableBuilder.apply(Seq(
      TestTable("foo", "/thing/here", 42),
      TestTable("bar", "/data", 42),
      TestTable("bazoo", "/long/test/values/", 10056)
    ))

    str.map(_.mkString("")).foreach(println)



  }

}
