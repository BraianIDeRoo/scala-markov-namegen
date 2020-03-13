package markovNamegen

import zio.test._
import Assertion._
import markovNamegen.Smoothing.SmoothingF
import zio.ZIO._
import zio.random.Random
import zio.test.environment.TestRandom
import zio.test.TestAspect.forked
import zio.test.{ DefaultRunnableSpec, ZSpec }

object GeneratorSpec extends DefaultRunnableSpec {
  val testData: Vector[String]       = Vector("foo", "foobar", "ook")
  val testAlphabet                   = IndexedSeq("a", "b", "f", "k", "o", "r", "#")
  val testPriorSmoothing: SmoothingF = Smoothing.priorSmoothing(0.01)

  val generatorSuite: Spec[Random with TestRandom, TestFailure[Nothing], TestSuccess] =
    suite("Generator")(
      testM("should pass its values to the models correctly") {
        for {
          g        <- Generator.make(testData, testPriorSmoothing, 2)
          alphabet = g.alphabet
          ms       <- g.models
          mA       = ms.map(_._3)
          mO       = ms.map(_._1)
          _        <- succeed(())
        } yield assert(alphabet)(equalTo(testAlphabet)) &&
          assert(mA.forall(_ == testAlphabet))(isTrue) &&
          assert(mO.head == 2)(isTrue) &&
          assert(mO(1) == 1)(isTrue)
      },
      testM("should always generate a word with prior") {
        for {
          _        <- TestRandom.feedDoubles(0.4, 0.1, 1.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
          g        <- Generator.make(testData, testPriorSmoothing, 2)
          maybeRes <- g.generate
          res = maybeRes match {
            case Some(value) => value
            case None        => ""
          }
        } yield assert(res.isEmpty)(isFalse) &&
          assert(res)(equalTo("fobar"))
      }
    )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    generatorSuite @@ forked
}
