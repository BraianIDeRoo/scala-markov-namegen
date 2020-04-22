package markovNamegen

import braianideroo.random.SeedRandom
import braianideroo.random.value.{ Probability, RandomVIO }
import markovNamegen.Smoothing.SmoothingF
import zio.ZIO._
import zio.{ Ref, ZIO }

private[markovNamegen] class Model private (
  val order: Int,
  probabilities: Ref[Map[String, RandomVIO[Nothing, Option[String]]]],
  val smoothing: SmoothingF,
  letters: IndexedSeq[String]
) {
  type ProbVector = Vector[Probability[String]]

  def generate(context: String): ZIO[SeedRandom, Nothing, Option[String]] =
    for {
      maybeChain <- probabilities.get >>= (x => succeed(x.get(context)))
      res <- maybeChain match {
              case Some(ref) => ref
              case None      => ZIO.none
            }
    } yield res

  def retrain(data: IndexedSeq[String]): ZIO[Any, Nothing, Unit] =
    train(data)

  private def train(data: IndexedSeq[String]) =
    for {
      observations <- Ref.make[Map[String, Ref[ProbVector]]](Map())
      _ <- foreach_(data) { element =>
            val parts = getParts(element, order)
            foreach_(parts)(part =>
              addLetters(part._1, observations) *>
                addPart(part, observations)
            )
          }
      // smooth the probabilities
      _   <- smoothing.provide(observations)
      obs <- observations.get
      x   = obs.map(x => (x._1, x._2.get >>= (y => braianideroo.random.value.RandomValue.fromProbabilityIterable(y))))
      _   <- probabilities.set(x)
    } yield ()

  private def addLetters(element: String, observations: Ref[Map[String, Ref[ProbVector]]]) = {
    val l = letters.map(x => Probability(x, 0))
    for {
      refVec <- Ref.make[Vector[Probability[String]]](Vector(l: _*))
      _      <- observations.update(_ + (element -> refVec))
    } yield ()
  }

  private def addPart(part: (String, String), obs: Ref[Map[String, Ref[ProbVector]]]) =
    for {
      maybeObservationRef <- obs.get.map(_.get(part._1))
      _ <- maybeObservationRef match {
            case Some(observationRef) =>
              for {
                observation      <- observationRef.get
                probabilityIndex = observation.indexWhere(_.value == part._2)
                _ <- probabilityIndex match {
                      case n: Int if n > -1 =>
                        val prob    = observation(n)
                        val newProb = Probability(prob.value, prob.probability + 1)
                        observationRef.update(_.updated(n, newProb))
                      case _ =>
                        observationRef.update(_ :+ Probability(part._2, 1))
                    }
              } yield ()
            case None =>
              for {
                observationRef <- Ref
                                   .make[ProbVector](Vector(Probability(part._2, 1)))
                _ <- obs.update(_ + (part._1 -> observationRef))
              } yield ()
          }
    } yield ()

  def getParts(string: String, order: Int): Vector[(String, String)] = {
    @scala.annotation.tailrec
    def inner(string: String, order: Int, res: Vector[(String, String)]): Vector[(String, String)] =
      if (string.length <= order) res
      else {
        val aux = string.substring(0, order)
        inner(string.tail, order, res :+ (aux, string(order).toString))
      }
    inner(("#" * order) + string + "#", order, Vector())
  }

}

object Model {
  def make(smoothingF: SmoothingF, order: Int, letters: IndexedSeq[String]): ZIO[Any, Nothing, Model] =
    for {
      probabilities <- Ref.make[Map[String, RandomVIO[Nothing, Option[String]]]](Map())
    } yield new Model(order, probabilities, smoothingF, letters)
}
