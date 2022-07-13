import scala.collection.mutable
import scala.util.Random
import annotation.tailrec

package object kmedianas {
  import common._

    class Punto(val x: Double, val y: Double, val z: Double) {
      private def cuadrado(v: Double):Double = v*v

      def distanciaAlCuadrado(that: Punto): Double =
        cuadrado(that.x-x) + cuadrado(that.y-y) + cuadrado(that.z-z)

      private def round(v: Double): Double = (v * 100).toInt / 100.0

      override def toString = s"(${round ( x )} , ${round ( y )} , ${round ( z )})"
    }

  def hallarPuntoMasCercano(punto: Punto, medianas: IterableOnce[Punto]):Punto = {
    val it = medianas.iterator
    assert(it.nonEmpty)
    var puntoMasCercano = it.next()
    var minDistancia = punto.distanciaAlCuadrado(puntoMasCercano)
    while (it.hasNext) {
      val point = it.next()
      val distancia = punto.distanciaAlCuadrado(point)
        if (distancia < minDistancia) {
          minDistancia = distancia
          puntoMasCercano = point
        }
    }
    puntoMasCercano
  }

  def clasificarSeq(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
      medianas.groupBy((punto: Punto) => hallarPuntoMasCercano(punto, medianas))
  }

  def calculePromedioSeq(medianaVieja: Punto, puntos: Seq[Punto]):Punto = {
    if (puntos.isEmpty)
      medianaVieja
      else{
      var x = 0.0
      var y = 0.0
      var z = 0.0

      puntos.foreach{ p =>
        x += p.x
        y += p.y
        z += p.z
      }
      new Punto(x / puntos.length, y / puntos.length, z / puntos.length)
    }
  }

  def actualizarSeq(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]):Seq[Punto] = {
    val nuevasMedianas = (for {
      i <- medianasViejas
    }yield calculePromedioSeq(i, clasif(i))).toSeq

    nuevasMedianas
  }

  def hayConverSeq(eta: Double, medianasViejas: Seq[Punto], medianasNuevas: Seq[Punto]): Boolean = {

    val distancias = (for {
      i <- 0 until(medianasNuevas.length)
    } yield medianasNuevas(i).distanciaAlCuadrado(medianasViejas(i))).toList

      distancias.forall(x => x < eta)

  }

  final def kmedianasSeq(puntos: Seq[Punto], medianas: Seq[Punto], eta: Double):Seq[Punto] = {
    val puntosAgrupados = clasificarSeq(puntos, medianas)
    val medianasActualizadas = actualizarSeq(puntosAgrupados, medianas)

    if (hayConverSeq(eta, medianas, medianasActualizadas)){
      medianasActualizadas
    }else{
      kmedianasSeq(puntos, medianasActualizadas, eta)
    }

  }


  def generarPuntosSeq(k: Int, num: Int): Seq[Punto] = {
    val randX = new Random(1)
    val randY = new Random(3)
    val randZ = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) %k) * 1.0 / k + randX.nextDouble() * 0.5
        val y = ((i + 5) %k) * 1.0 / k + randY.nextDouble() * 0.5
        val z = ((i + 7) %k) * 1.0 / k + randZ.nextDouble() * 0.5
        new Punto(x, y, z)
      })

  }

  def inicializarMedianasSeq(k: Int, puntos: Seq[Punto]): Seq[Punto] = {
    val random = new Random(7)
    (0 until k).map(_ => puntos(random.nextInt(puntos.length)))
  }

}
