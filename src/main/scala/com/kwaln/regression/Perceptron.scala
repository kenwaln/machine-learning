package com.kwaln.regression

import scala.annotation.tailrec
import java.util.Random
import breeze.linalg._

/**
  * Created by Ken.Waln on 9/25/2016.
  */

class Perceptron(val w: DenseVector[Double], loops:Int) extends Model {
  def getWeights: DenseVector[Double] = w
  def getLoopCount:Int = loops

  def errorRate(x:DenseMatrix[Double], y: Vector[Double]) : Double = {
    val yPred  = x * w
    val errs = (for (i <- 0 until y.length) yield y(i) != Math.signum(yPred(i))).toList
    errs.count(_ == true).toDouble / y.length.toDouble
  }
}

object Perceptron {
  //
  def solve(x: DenseMatrix[Double],
            y: DenseVector[Double],
            wInit: DenseVector[Double],
            plotFunc: ((Double, Double)) => Unit): Perceptron = {
    val randGen = new Random()
    @tailrec
    def solveRec(
                  x: DenseMatrix[Double],
                  y: DenseVector[Double],
                  w: DenseVector[Double],
                  loop: Int = 0
                ): Perceptron = {
      if (loop >= 1000) {
        throw new Exception("Loop count exceeded")
      }
      else {
        //plotFunc((-w(1) / w(2), -w(0) / w(2)))
        //println(x)
        //println(w)
        val yTestLin:DenseVector[Double] = x * w
        val yTest = yTestLin.map (a => Math.signum(a))
        //println(yTestLin)
        //println(yTest)
        val yErrs = {
          for {i <- 0 until y.length
               if yTest(i) != y(i)} yield i
        }
        //println(yErrs.length)
        if (yErrs.isEmpty) new Perceptron(w,loop)
        else {
          //println(yErrs)
          val idx = yErrs(randGen.nextInt(yErrs.length))
          //println(idx)
          //println(w)
          //println(x(idx,::))
          //println(y)
          //println(yTest)
          val wNew:DenseVector[Double] = w.toDenseVector + (x(idx,::).t.toDenseVector :* y(idx))
          //println(wNew)

          solveRec(x, y, wNew, loop + 1)
        }
        /*
        val yTest = for (
          xi <- x
        ) yield (xi, Math.signum(((w zip xi) map { case (wj, xij) => wj * xij }).sum).toInt)
        val yErrs = (yTest zip y) filter { case (a, b) => a._2 != b }
        if (yErrs.isEmpty) (loop,w)
        else yErrs(randGen.nextInt(yErrs.length)) match {  //
          case ((xBad, yTest), yBad) =>
            val wNew = (w zip xBad) map { case (wi, xi) => wi + xi * yBad }
            solveRec(x, y, wNew, loop + 1)
        }
        */
      }
    }
    val dim = x.cols
    solveRec(x, y, wInit, 0)
  }
}
