package markovNamegen

import zio.test._
import Assertion._
import zio.ZIO._
import zio.random.Random
import zio.test.environment.TestRandom
import zio.test.TestAspect.forked
import zio.test.{ DefaultRunnableSpec, ZSpec }

object GeneratorSpec extends DefaultRunnableSpec {
  val testData: Vector[String] = Vector("foo", "foobar", "ook")
  val testAlphabet             = IndexedSeq("a", "b", "f", "k", "o", "r", "#")

  val generatorSuite: Spec[Random with TestRandom, TestFailure[Nothing], TestSuccess] =
    suite("Generator")(
      testM("should pass its values to the models correctly") {
        for {
          g        <- Generator.make(testData, 0.01, 2)
          alphabet = g.alphabet
          order    = g.order
          prior    = g.prior
          ms       <- g.models
          mA       = ms.map(_._3)
          mO       = ms.map(_._1)
          mP       = ms.map(_._2)
          _        <- succeed(())
        } yield assert(alphabet)(equalTo(testAlphabet)) &&
          assert(mA.forall(_ == testAlphabet))(isTrue) &&
          assert(mO.head == 2)(isTrue) &&
          assert(mO(1) == 1)(isTrue) &&
          assert(mP)(equalTo(List(0.01, 0.01)))
      },
      testM("should always generate a word with prior") {
        for {
          _   <- TestRandom.feedDoubles(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
          g   <- Generator.make(testData, 0.01, 2)
          res <- g.generate
        } yield assert(res.isEmpty)(isFalse) &&
          assert(res)(equalTo("f"))
      }
    )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    generatorSuite
}
