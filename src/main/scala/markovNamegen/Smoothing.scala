package markovNamegen

import zio.{ Ref, ZIO }

object Smoothing {
  type ChainMap   = Ref[Map[String, Ref[Vector[Double]]]]
  type SmoothingF = ZIO[ChainMap, Nothing, Any]

  val noSmoothing: SmoothingF =
    ZIO.unit

  def priorSmoothing(prior: Double): SmoothingF =
    for {
      ch <- ZIO.accessM[ChainMap](_.get)
      _  <- ZIO.foreach_(ch)(_._2.update(_.map(_ + prior)))
    } yield ()

  val goodTuringSmoothing: SmoothingF =
    for {
      ch <- ZIO.accessM[ChainMap](_.get)
      _ <- ZIO.foreach_(ch) {
            _._2.update { oldV =>
              val n1 = oldV.count(_ == 1)
              val n0 = n1 / oldV.sum
              oldV.map(x => if (x == 0) n0 else x)
            }
          }
    } yield ()
}
