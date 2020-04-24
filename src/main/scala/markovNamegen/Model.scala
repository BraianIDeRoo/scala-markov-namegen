package markovNamegen

import braianideroo.random.SeedRandom
import braianideroo.random.value._
import zio.ZIO._
import zio.{ Ref, ZIO }

private[markovNamegen] class Model private (
  val order: Int,
  probabilities: Ref[Map[String, RandomVIO[Nothing, Option[String]]]],
  val smoothing: SmoothF[String],
  letters: IndexedSeq[String]
) {

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
      observations <- Ref.make[Map[String, Ref[Probabilities[String]]]](Map())
      _ <- foreach_(data) { element =>
            val parts = getParts(element, order)
            foreach_(parts)(part =>
              for {
                obs <- observations.get
                ref <- obs.get(part._1) match {
                        case Some(value) => succeed(value)
                        case None        => addLetters(part._1, observations)
                      }
                _ <- addPart(part, ref)
                _ <- ref.get >>= (x => succeed(x.count(x => x._2 > 0)))
              } yield ()
            )
          }
      // smooth the probabilities
      obs <- observations.get
      _ <- foreach_(obs) { x =>
            for {
              aux      <- x._2.get
              smoothed <- aux.smooth(smoothing)
              _        <- x._2.set(smoothed)
            } yield ()
          }
      x = obs.map(x => (x._1, x._2.get >>= (y => RandomValue.fromMap(y))))
      _ <- probabilities.set(x)
    } yield ()

  private def addLetters(element: String, observations: Ref[Map[String, Ref[Probabilities[String]]]]) = {
    val l = letters.map(x => (x, 0.0))
    for {
      refVec <- Ref.make[Probabilities[String]](Map(l: _*))
      _      <- observations.update(_ + (element -> refVec))
    } yield refVec
  }

  private def addPart(part: (String, String), obs: Ref[Probabilities[String]]): ZIO[Any, Nothing, Unit] =
    for {
      probabilities <- obs.get
      probability   = probabilities(part._2)
      _             <- obs.update(_ + (part._2 -> (probability + 1)))
    } yield ()

  /*
  private def addPart(part: (String, String), obs: Ref[Map[String, Ref[Probabilities[String]]]]) =
    for {
      maybeProbabilitiesRef <- obs.get.map(_.get(part._1))
      _ <- maybeProbabilitiesRef match {
            case Some(probabilitiesRef) =>
              for {
                probabilities    <- probabilitiesRef.get
                maybeProbability = probabilities.get(part._2)
                _ <- probabilitiesRef.set(probabilities + (part._2 -> (maybeProbability match {
                      case Some(probability) => probability + 1
                      case None              => 1
                    })))
                aux <- probabilitiesRef.get
              } yield ()
            case None =>
              for {
                observationRef <- Ref
                                   .make[Probabilities[String]](Map(part._2 -> 1))
                _ <- obs.update(_ + (part._1 -> observationRef))
              } yield ()
          }
    } yield ()

   */

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
  def make(smoothingF: SmoothF[String], order: Int, letters: IndexedSeq[String]): ZIO[Any, Nothing, Model] =
    for {
      probabilities <- Ref.make[Map[String, RandomVIO[Nothing, Option[String]]]](Map())
    } yield new Model(order, probabilities, smoothingF, letters)
}
