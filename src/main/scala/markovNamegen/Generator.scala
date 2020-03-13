package markovNamegen

import markovNamegen.Smoothing.SmoothingF
import markovNamegen.util.UniqueVector
import zio.ZIO
import zio.ZIO._
import zio.random.Random

private[markovNamegen] class Generator private (
  data: IndexedSeq[String],
  val order: Int,
  val smoothingF: SmoothingF,
  _models: List[Model],
  val alphabet: IndexedSeq[String]
) {

  def models: ZIO[Any, Nothing, List[
    (Int, SmoothingF, IndexedSeq[String], Map[String, UniqueVector[String]], Map[String, Vector[Double]])
  ]] =
    foreach(_models) { m =>
      for {
        observations <- m.observations
        chains       <- m.chains
      } yield (m.order, m.smoothing, m.alphabet, observations, chains)
    }

  def trainAll: ZIO[Any, Nothing, Unit] =
    foreachPar_(_models)(_.retrain(data))

  def generate: ZIO[Random, Nothing, Option[String]] = {
    val word = "#" * order

    def aux(s: String): ZIO[Random, Nothing, Option[String]] =
      for {
        l <- getLetter(s)
        res <- l match {
                case Some(value) => inner(s, value)
                case None        => succeed(Some(s))
              }
      } yield res

    def inner(word: String, letter: String): ZIO[Random, Nothing, Option[String]] =
      if (letter == "#")
        if (word.isEmpty) none
        else succeed(Some(word))
      else {
        val w = word + letter
        aux(w).map {
          case Some(value) =>
            val k = value.replace("#", "")
            if (k.isEmpty) None
            else Some(k)
          case None => None
        }

      }

    aux(word)
  }

  private def getLetter(context: String) = {
    val ctx = context.substring(context.length - order, context.length)

    def inner(models: List[Model], ctx: String): ZIO[Random, Nothing, Option[String]] =
      models match {
        case Nil          => none
        case model :: Nil => model.generate(ctx)
        case model :: others =>
          model.generate(ctx) >>= {
            case Some(value) => succeed(Some(value))
            case None        => inner(others, ctx.drop(1))
          }
      }
    inner(_models, ctx)
  }
}

object Generator {
  def make(data: IndexedSeq[String], smoothingF: SmoothingF, order: Int): ZIO[Any, Nothing, Generator] = {
    val letters =
      data.flatMap(x => x.toList.map(_.toString)).sorted.distinct :+ "#"

    for {
      models <- foreach(0 until order)(x =>
                 Model.make(
                   letters.toVector,
                   smoothingF,
                   order - x
                 )
               )
    } yield new Generator(data, order, smoothingF, models, letters)

  }.tap(_.trainAll)
}
