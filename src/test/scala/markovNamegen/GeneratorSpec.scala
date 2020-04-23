package markovNamegen

import zio.test._
import Assertion._
import braianideroo.random.SeedRandom
import markovNamegen.Smoothing.SmoothingF
import zio.{ Has, Layer, ZLayer }
import zio.ZIO._
import zio.test.TestAspect.forked
import zio.test.{ DefaultRunnableSpec, ZSpec }

object GeneratorSpec extends DefaultRunnableSpec {
  val testData: Vector[String]       = Vector("foo", "foobar", "ook")
  val testAlphabet                   = IndexedSeq("a", "b", "f", "k", "o", "r", "#")
  val testPriorSmoothing: SmoothingF = Smoothing.priorSmoothing(0.01)

  val seed: Layer[Nothing, Has[Long]] = ZLayer.succeed(501L)
  val randomLayer: ZLayer[Any, Nothing, Has[SeedRandom.Service]] =
    seed >>> braianideroo.random.SeedRandom.live

  val generatorSuite: Spec[SeedRandom, TestFailure[Nothing], TestSuccess] =
    suite("Generator")(
      testM("should always generate a word with prior") {
        for {
          g        <- Generator.make(testData, testPriorSmoothing, 2)
          maybeRes <- g.generate
          res = maybeRes match {
            case Some(value) => value
            case None        => ""
          }
        } yield assert(res.isEmpty)(isFalse) &&
          assert(res)(equalTo("ook"))
      }
    )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    generatorSuite.provideLayer(randomLayer) @@ forked
}
