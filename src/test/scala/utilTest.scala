package com.kwaln.regression
/**
  * Created by Ken.Waln on 9/29/2016.
  */

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import breeze.linalg.{Vector => BreezeVector, _}
import interop._

@RunWith(classOf[JUnitRunner])
class UtilSuite extends FunSuite {
  test("Breeze from List implicit") {
    val l = List(1.0,2.0,3.0,4.0)
    val b: BreezeVector[Double] = l
    assert(b.length == l.length)
    for (i <- l.indices) assert(l(i) == b(i))
  }

  test("BreezeVector.toList") {
    val b = BreezeVector(1.0, 2.0,3.0)
    val l = b.toList
    assert(l.length == b.length)
    for (i <- l.indices) assert(l(i) == b(i))
  }
}
