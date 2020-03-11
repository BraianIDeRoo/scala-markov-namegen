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

case class Number(value: Int) extends GroupGenerationOption {
  override def check(elements: Seq[String]): Boolean = elements.length == value
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

case class SimilarTo(value: String) extends SingleGenerationOption {
  override def check(element: String): Boolean = ???
}

case class RegexMatch(value: Regex) extends SingleGenerationOption {
  override def check(element: String): Boolean = value.matches(element)
}
