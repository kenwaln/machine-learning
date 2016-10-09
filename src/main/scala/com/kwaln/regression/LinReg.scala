package com.kwaln.regression

import breeze.linalg._

/**
  * Created by Ken.Waln on 10/8/2016.
  */
class LinReg(val w: DenseVector[Double]) extends Model {

  def getWeights = w

  def errorRate(x:DenseMatrix[Double], y: Vector[Double]) : Double = {
    val yPred  = x * w
    val errs = (for (i <- 0 until y.length) yield y(i) != Math.signum(yPred(i))).toList
    errs.count(_ == true).toDouble / y.length.toDouble
  }

  def predict1(x: Vector[Double]): Double = {
    Math.signum(w.t * x)
  }
}

object LinReg {
  def solve(x: DenseMatrix[Double],
            y: Vector[Double],
            plotFunc: ((Double, Double)) => Unit): LinReg = {
    val w = pinv(x) * y
    //println(w.getClass)
    new LinReg(w)
  }
}