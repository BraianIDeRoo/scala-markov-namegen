package markovNamegen

import markovNamegen.Smoothing.SmoothingF
import markovNamegen.util.UniqueVector
import zio.ZIO._
import zio.random.Random
import zio.{ random, Ref, ZIO }

private[markovNamegen] class Model private (
  val alphabet: IndexedSeq[String],
  val order: Int,
  _observations: Ref[Map[String, Ref[UniqueVector[String]]]],
  _chains: Ref[Map[String, Ref[Vector[Double]]]],
  val smoothing: SmoothingF
) {

  private def accessMapRef[A, B <: Iterable[A]](map: Ref[Map[String, Ref[B]]]): ZIO[Any, Nothing, Map[String, B]] =
    for {
      a   <- map.get
      res <- foreach(a)(o => o._2.get >>= (x => succeed(o._1, x)))
    } yield res.toMap

  def observations: ZIO[Any, Nothing, Map[String, UniqueVector[String]]] =
    accessMapRef[String, UniqueVector[String]](_observations)

  def chains: ZIO[Any, Nothing, Map[String, Vector[Double]]] =
    accessMapRef[Double, Vector[Double]](_chains)

  def generate(context: String): ZIO[Random, Nothing, Option[String]] =
    for {
      maybeChain <- _chains.get >>= (x => succeed(x.get(context)))
      res <- maybeChain match {
              case Some(ref) =>
                for {
                  chain  <- ref.get
                  index  <- selectIndex(chain)
                  result = index.map(x => alphabet(x))
                } yield result
              case None => ZIO.none
            }
    } yield res

  def retrain(data: IndexedSeq[String]): ZIO[Any, Nothing, Unit] =
    train(data) *> buildChains

  private def train(data: IndexedSeq[String]) =
    foreach_(data) { element =>
      val d = ("#" * order) + element + "#"
      foreach_(0 until (d.length - order))(i => addObservation(d, i))
    }

  private def addObservation(d: String, i: Int) = {
    val key = d.substring(i, i + order)
    for {
      maybeValue <- _observations.get >>= (x => succeed(x.get(key)))
      value <- maybeValue match {
                case Some(vector) => succeed(vector)
                case None =>
                  Ref.make[UniqueVector[String]](UniqueVector()).tap(x => _observations.update(_ + (key -> x)))
              }
      _ <- value.update(_ :+ d.charAt(i + order).toString)
    } yield ()
  }

  private def buildChains =
    for {
      _    <- _chains.set(Map())
      keys <- _observations.get >>= (obs => succeed(obs.keys))
      _    <- foreach_(keys)(context => foreach_(alphabet)(prediction => buildContext(prediction, context)))
      _    <- smoothing.provide(_chains)
    } yield ()

  private def buildContext(prediction: String, context: String) =
    for {
      maybeChain <- _chains.get >>= (x => succeed(x.get(context)))
      value <- maybeChain match {
                case Some(ref) => succeed(ref)
                case None =>
                  Ref
                    .make[Vector[Double]](Vector())
                    .tap(x => _chains.update(_ + (context -> x)))
              }
      arr <- _observations.get >>= (x => succeed(x.get(context)))
      matches <- arr match {
                  case Some(value) =>
                    value.get >>= (x => succeed(countMatches(x, prediction)))
                  case None => succeed(0)
                }
      _ <- value.update(_ :+ matches)
    } yield ()

  private def countMatches[A](arr: Iterable[A], v: A) =
    arr.foldLeft(0)((a, b) =>
      b match {
        case s if s == v => a + 1
        case _           => a
      }
    )

  private def selectIndex(chain: Iterable[Double]) = {
    val (totals, accumulator) =
      chain.foldLeft((Vector[Double](), 0d))((old, current) =>
        (
          old._1 :+
            current,
          old._2 + current
        )
      )

    @scala.annotation.tailrec
    def inner(rand: Double, totals: Iterable[(Double, Int)]): Option[Int] =
      if (totals.isEmpty) None
      else {
        if (rand < totals.head._1) Some(totals.head._2)
        else inner(rand - totals.head._1, totals.tail)
      }

    for {
      rand   <- random.nextDouble >>= (x => succeed(x * accumulator))
      result = inner(rand, totals.zipWithIndex)
    } yield result
  }
}

object Model {
  def make(alphabet: IndexedSeq[String], smoothingF: SmoothingF, order: Int): ZIO[Any, Nothing, Model] =
    for {
      observations <- Ref.make[Map[String, Ref[UniqueVector[String]]]](Map())
      chains       <- Ref.make[Map[String, Ref[Vector[Double]]]](Map())
    } yield new Model(alphabet, order, observations, chains, smoothingF)
}
