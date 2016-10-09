package com.kwaln.regression

import breeze.linalg.{Vector => BreezeVector, SparseVector}

import scala.reflect.ClassTag

/**
  * Created by Ken.Waln on 9/29/2016.
  */

object interop {
  implicit def toBreezeVector[A:ClassTag](a:Seq[A]) : BreezeVector[A] = {
    BreezeVector.tabulate[A](a.indices)(i => a(i))
  }

  class BreezeToVector[A] (x: BreezeVector[A]) {
    def toList: List[A] = {
      (for (i <- 0 until x.length) yield x(i)).toList
    }
    def toScalaVector: Vector[A] = {
      (for (i <- 0 until x.length) yield x(i)).toVector
    }
  }
  implicit def convertBreezeToVector[A](x: BreezeVector[A]): BreezeToVector[A] = new BreezeToVector[A](x)

}




