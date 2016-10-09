package com.kwaln.regression

import java.util._
import breeze.linalg._

/**
  * Created by Ken.Waln on 9/25/2016.
  */
object main extends App {

  object randGen {
    val rand = new Random()

    def randVal = rand.nextDouble * 2.0 - 1.0

    def randVect(dim: Integer) = DenseVector.tabulate[Double](dim)(_ => randVal)

    def randBool(p: Double): Boolean = rand.nextDouble < p

    //def randX(dim: Integer) = DenseVector.tabulate[Double](dim + 1)(i => if (i == 0) 1.0 else randVal)
  }

  def getRefLine: (Double, Double) = {
    val pt1 = randGen.randVect(2)
    val pt2 = randGen.randVect(2)
    val slope = (pt2(1) - pt1(1)) / (pt2(0) - pt1(0))
    val intercept = pt1(1) - pt1(0) * slope
    (slope, intercept)
  }

  def randCheck(results: scala.Vector[((Double, Double), LinReg)]): Double = {
    val testM = 1000
    (for {
      ((slope, intercept), model) <- results;
      j <- 0 until testM
    } yield {
      val x: DenseMatrix[Double] = DenseMatrix.tabulate[Double](N, dim + 1)((i, j) => if (j == 0) 1 else randGen.randVal)
      val y = for (row <- x(*, ::)) yield {
        Math.signum(row(1) * slope + intercept - row(2))
      }
      model.errorRate(x, y)
    }).sum / (testM * results.length)
  }

  def getPolyX(N:Int): DenseMatrix[Double] = {
    val xLin: DenseMatrix[Double] = DenseMatrix.tabulate[Double](N, 3)((i, j) => if (j == 0) 1 else randGen.randVal)
    DenseMatrix.tabulate[Double](N2, 6)((i, j) => j match {
      case 0 => 1.0
      case 1 => xLin(i,j)
      case 2 => xLin(i,j)
      case 3 => xLin(i, 1) * xLin(i, 2)
      case 4 => xLin(i, 1) * xLin(i, 1)
      case 5 => xLin(i, 2) * xLin(i, 2)
    })
  }

  def randCheckNL(results: scala.Vector[LinReg]): Double = {
    val testM = 1000
    val testN = 1000
    (for {
      model <- results;
      j <- 0 until testM
    } yield {
      val x: DenseMatrix[Double] = getPolyX(testN)
      val y = for (row <- x(*, ::)) yield {
        Math.signum(row(1) * row(1) + row(2) * row(2) - 0.6) * (if (randGen.randBool(0.1)) -1 else 1)
      }
      model.errorRate(x, y)
    }).sum / (testM * results.length)
  }

  val N = 10
  val dim = 2
  val M = 1
  var ySum: Double = 0.0
  val results = for (i <- 0 until M) yield {
    val (slope, intercept) = getRefLine
    val x: DenseMatrix[Double] = DenseMatrix.tabulate[Double](N, dim + 1)((i, j) => if (j == 0) 1 else randGen.randVal)
    val y = for (row <- x(*, ::)) yield {
      Math.signum(row(1) * slope + intercept - row(2))
    }
    ySum += y.sum
    val model = LinReg.solve(x, y, plotter.plotIt(x, y, (slope, intercept)))
    val w = model.getWeights
    val refLine = (-w(1) / w(2), -w(0) / w(2))

    if (i % 100 == 0) {
      //plotter.plotIt(x, y, (slope, intercept))(refLine)
    }

    val pFresh = Perceptron.solve(x, y, DenseVector.zeros[Double](3), plotter.plotIt(x, y, (slope, intercept)))
    val p = Perceptron.solve(x, y, w, plotter.plotIt(x, y, (slope, intercept)))
    val loops = p.getLoopCount
    ((slope, intercept), model, model.errorRate(x, y), pFresh, p)
  }
  println(s"Total of y: $ySum")

  val avgEIn = (results map {
    case ((s, i), model, e, pFresh, p) => e
  }).sum / M

  val avgFreshLoops = (results map {
    case ((s, i), model, e, pFresh, p) => pFresh.getLoopCount
  }).sum.toDouble / M
  val avgLoops = (results map {
    case ((s, i), model, e, pFresh, p) => p.getLoopCount
  }).sum.toDouble / M

  println(s"Average eIn = $avgEIn")
  val linResults = (results map {
    case ((s: Double, i: Double), l: LinReg, e: Double, pFresh: Perceptron, p: Perceptron) => ((s, i), l)
  }
    ).toVector
  val avgLinEOut = randCheck(linResults)
  println(s"Average Linear Rgression eOut = $avgLinEOut")
  val perResults = (results map {
    case ((s: Double, i: Double), l: LinReg, e: Double, pFresh: Perceptron, p: Perceptron) => ((s, i), l)
  }).toVector
  val avgPerEOut = randCheck(linResults)
  println(s"Average Perceptron eOut = $avgPerEOut")
  println(s"Average Loop Count = $avgLoops, without regression first = $avgFreshLoops")

  val N2 = 1000
  val dim2 = 2
  val M2 = 100
  val resultsNonLin = for (i <- 0 until M2) yield {
    val x: DenseMatrix[Double] = DenseMatrix.tabulate[Double](N2, dim2 + 1)((i, j) => if (j == 0) 1 else randGen.randVal)
    val y = for (row <- x(*, ::)) yield {
      Math.signum(row(1) * row(1) + row(2) * row(2) - 0.6) * (if (randGen.randBool(0.1)) -1 else 1)
    }
    val xnl = DenseMatrix.tabulate[Double](N2, 6)((i, j) => j match {
      case 0 => 1.0
      case 1 => x(i, 1)
      case 2 => x(i, 2)
      case 3 => x(i, 1) * x(i, 2)
      case 4 => x(i, 1) * x(i, 1)
      case 5 => x(i, 2) * x(i, 2)
    })

    val model = LinReg.solve(x, y, plotter.nullPlotter)
    val modelNL = LinReg.solve(xnl, y, plotter.nullPlotter)
    //val w = model.getWeights
    //val refLine = (-w(1) / w(2), -w(0) / w(2))
    //plotter.plotIt(x, y, (0.0, -1.0))(refLine)
    (model, model.errorRate(x, y), modelNL, modelNL.errorRate(xnl, y))
  }
  val avgNonLinEIn = (resultsNonLin map {
    case (model, e, modelNL, eNL) => {
      //println(model.getWeights)
      eNL
    }
  }).sum / M2
  val avgNonLinWeightsList = resultsNonLin map {
    case (model: Model, e: Double, modelNL: Model, eNL: Double) =>
      //println(modelNL.getWeights)
      modelNL.getWeights
  }
  val totNonLinWeights = avgNonLinWeightsList.foldLeft(DenseVector.zeros[Double](6)){case (s: DenseVector[Double], w) => s + w }
  val avgNonLinWeights = totNonLinWeights.map (_/M2.toDouble)
  println(s"Non-linear weights =$avgNonLinWeights")
  println(s"Nonlinear EIn = $avgNonLinEIn")

  val nlEOut = randCheckNL((resultsNonLin map {case (model, e, modelNL, eNL) => modelNL}).toVector)
  println(s"Non-linear eOut = $nlEOut")

}
