package com.kwaln.regression

import breeze.linalg._

/**
  * Created by Ken.Waln on 10/8/2016.
  */
trait Model {
  def getWeights: DenseVector[Double]

  def errorRate(x:DenseMatrix[Double], y: Vector[Double]) : Double
}
