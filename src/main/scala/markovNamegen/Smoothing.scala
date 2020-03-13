package markovNamegen

import markovNamegen.util.UniqueVector
import zio.{ Ref, ZIO }

trait Smoothing {
  def smooth(
    observations: Ref[Map[String, Ref[UniqueVector[String]]]],
    chains: Ref[Map[String, Ref[Vector[Double]]]]
  ): ZIO[Any, Nothing, Unit]
}

case class PriorSmoothing(prior: Double) extends Smoothing {
  override def smooth(
    observations: Ref[Map[String, Ref[UniqueVector[String]]]],
    chains: Ref[Map[String, Ref[Vector[Double]]]]
  ): ZIO[Any, Nothing, Unit] =
    for {
      ch <- chains.get
      _  <- ZIO.foreach_(ch)(oldChain => oldChain._2.update(oldV => oldV.map(_ + prior)))
    } yield ()
}

case object NoSmoothing extends Smoothing {
  override def smooth(
    observations: Ref[Map[String, Ref[UniqueVector[String]]]],
    chains: Ref[Map[String, Ref[Vector[Double]]]]
  ): ZIO[Any, Nothing, Unit] =
    ZIO.unit
}

case object GoodTuringSmoothing extends Smoothing {
  override def smooth(
    observations: Ref[Map[String, Ref[UniqueVector[String]]]],
    chains: Ref[Map[String, Ref[Vector[Double]]]]
  ): ZIO[Any, Nothing, Unit] =
    for {
      ch <- chains.get
      _ <- ZIO.foreach_(ch) { oldChain =>
            oldChain._2.update { oldV =>
              val n1 = oldV.count(_ == 1)
              val n0 = n1 / oldV.sum
              oldV.map(x => if (x == 0) n0 else x)
            }
          }
    } yield ()
}
