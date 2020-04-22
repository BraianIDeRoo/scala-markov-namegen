package markovNamegen

import braianideroo.random.value.{ Probability, RandomVIO }
import zio.{ Ref, ZIO }

object Smoothing {
  type Probabilities = Ref[Map[String, Ref[Vector[Probability[String]]]]]
  type SmoothingF    = ZIO[Probabilities, Nothing, Any]

  val noSmoothing: SmoothingF = ZIO.unit

  def priorSmoothing(prior: Double): SmoothingF =
    for {
      probabilities <- ZIO.accessM[Probabilities](_.get)
      _             <- ZIO.foreach_(probabilities)(_._2.update(_.map(x => Probability(x.value, x.probability + prior))))
    } yield ()

  val goodTuringSmoothing: SmoothingF =
    for {
      ch <- ZIO.accessM[Probabilities](_.get)
      _ <- ZIO.foreach_(ch) {
            _._2.update { oldV =>
              val n1 = oldV.count(_.probability == 1)
              val n0 = n1 / oldV.map(x => x.probability).sum
              oldV.map(x =>
                if (x.probability == 0) Probability(x.value, n0)
                else x
              )
            }
          }
    } yield ()
}
