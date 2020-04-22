package markovNamegen

import scala.util.matching.Regex
import zio.UIO

sealed trait GenerationOption

// if the option doesnt return true the generation should keep going
trait GroupGenerationOption extends GenerationOption {
  def check(elements: Seq[String]): Boolean
}

trait GroupGenerationOptionM extends GenerationOption {
  def check(elements: Seq[String]): UIO[Boolean]
}

// if the option doesnt return true the generated string should be discarded
trait SingleGenerationOption extends GenerationOption {
  def check(element: String): Boolean
}

trait SingleGenerationOptionM extends GenerationOption {
  def check(element: String): UIO[Boolean]
}

case class Length(value: Int) extends SingleGenerationOption {
  override def check(element: String): Boolean = element.length == value
}

case class MinLength(value: Int) extends SingleGenerationOption {
  override def check(element: String): Boolean = element.length >= value
}

case class MaxLength(value: Int) extends SingleGenerationOption {
  override def check(element: String): Boolean = element.length <= value
}

case class StartsWith(value: String) extends SingleGenerationOption {
  override def check(element: String): Boolean = element.startsWith(value)
}

case class EndsWith(value: String) extends SingleGenerationOption {
  override def check(element: String): Boolean = element.endsWith(value)
}

case class Includes(value: String) extends SingleGenerationOption {
  override def check(element: String): Boolean = element.contains(value)
}

case class Excludes(value: String) extends SingleGenerationOption {
  override def check(element: String): Boolean = !element.contains(value)
}

case class ExcludesAny(values: Iterable[String]) extends
  SingleGenerationOption {
  override def check(element: String): Boolean = values.forall(s => element
    .contains(s))
}

case class DamerauLevenshteinDistance(value: String, maxDistance: Int,
                                      enableTranspositions: Boolean)
    extends SingleGenerationOptionM {
  import zio.Ref
  import zio.ZIO
  import zio.ZIO._

  private def damerauLevenshteinMatrix(
    source: String,
    target: String,
    enableTranspositions: Boolean
  ): ZIO[Any, Nothing, Vector[Int]] = {
    val w = source.length + 1
    val h = source.length + 1
    for {
      costs <- Ref.make[Vector[Int]](Vector.fill(w * h)(0))
      _     <- foreach_(0 until w)(x => costs.update(_.updated(x, x)))
      _     <- foreach_(1 until h)(x => costs.update(_.updated(x * w, x)))
      cost  <- Ref.make[Int](0)
      _ <- foreach_(1 until w) { x =>
            foreach_(1 until h) { y =>
              if (source.charAt(x - 1) == target.charAt(y - 1)) cost.set(0)
              else cost.set(1)

              cost.get >>= (c =>
                costs.update(old =>
                  old.updated(
                    x + y * w,
                    List(
                      old((x - 1) + (y * w)) + 1,
                      List(
                        old(x + ((y - 1) * w)) + 1,
                        old((x - 1) + ((y - 1) * w)) + c
                      ).min
                    ).min
                  )
                ) <*> when(
                  enableTranspositions && x > 1 && y > 1 && source.charAt(x)
                    == target.charAt(y - 1) && source
                    .charAt(x - 1) == target.charAt(y)
                )(costs.update(old => old.updated(x + y * w, List(old(x + y *
                  w), old(x - 2 + ((y - 2) * w)) + c).min))).unit
              )
            }
          }
      res <- costs.get
    } yield res
  }

  override def check(element: String): UIO[Boolean] =
    if (element.length == 0) succeed(value.length <= maxDistance)
    else if (value.length == 0) succeed(element.length <= maxDistance)
    else
      for {
        distance <- damerauLevenshteinMatrix(element, value,
          enableTranspositions) >>= (v => succeed(v(v.length - 1)))
      } yield distance <= maxDistance
}

case class RegexMatch(value: Regex) extends SingleGenerationOption {
  override def check(element: String): Boolean = value.matches(element)
}
