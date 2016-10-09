package com.kwaln.regression

import breeze.linalg._
import breeze.plot._
import java.awt.Color

/**
  * Created by Ken.Waln on 9/27/2016.
  */
object plotter {

  val f = Figure()


  def drawLine(line: (Double, Double), color:String): Series = {
    val x = Vector(-1.0, 1.0)
    val y = line match {
      case (slope, intercept) => for (xi <- x) yield xi * slope + intercept
    }
    plot(x,y, colorcode=color)
  }

  def nullPlotter(percLine: (Double, Double)): Unit = {}

  def plotIt(x: DenseMatrix[Double],
             y: Vector[Double],
             refLine: (Double,Double))(percLine: (Double, Double)): Unit = {

    f.clearPlot(0)
    val p = f.subplot(0)
    p.xlim(-1, 1)
    p.ylim(-1, 1)
    val x1:Vector[Double] = x(::,1)
    val x2:Vector[Double] = x(::,2)
    //println(x1.getClass)
    def colors(i:Int): Color = if(y(i) == -1.0) Color.RED else Color.BLACK
    p += scatter(
      x1,
      x2,
      { i => 0.02},
      colors)
    p += drawLine(refLine, "r")
    p += drawLine(percLine, "b")
    Thread.sleep(1000)
  }
}

