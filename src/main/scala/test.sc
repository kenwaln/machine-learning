import java.util.Random
import com.kwaln.regression.main

val a:Vector[Double] = Vector(1.0,2.0,3.0)
val b = Vector(2,3,4)
a.zip(b)
((a zip b) map {case (w,x) => w * x}).sum
//val x = Vector(Vector(-0.1,-0.1), Vector(1.0,1.0), Vector(0.2,0.2))
val y = Vector(-1,1,-1)
//val w = Perceptron.solve(x,y)
1.0 +: a
val w = Vector(1.0,2.0,3.0)
val x = Vector(1.5,2.5,3.5)
val pred = (x, Math.signum(((w zip x) map { case (wj, xj) => wj * xj }).sum).toInt)
object randGen {
  val rand = new Random()

  def randVal = rand.nextDouble * 2.0 - 1.0

  def randVect(dim: Integer): Vector[Double] = (for (i <- 0 until dim) yield randVal).toVector
}


1.0 +: randGen.randVect(2)
(0 until 10).length
