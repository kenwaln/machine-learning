import breeze.linalg.{Vector => BreezeVector, _}
import breeze.plot._
import java.awt.Color
import java.awt.Color._

val l = List(1.0,2.0,3.0)
l.getClass
l.toVector.getClass
val b = l map (_ > 2)
b filter (_==true)

val x = DenseMatrix((1.0,1.0,2.0),(1.0,3.0,4.0), (1.0,5.0,6.0))
for( row <- x(*,::)) yield row(1)
x == 0.0
val y = x -x
y :== 0.0



