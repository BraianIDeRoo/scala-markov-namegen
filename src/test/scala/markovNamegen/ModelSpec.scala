package markovNamegen

import zio.test._
import Assertion._
import zio.ZLayer
import zio.random.Random
import zio.test.environment.TestRandom
import zio.test.TestAspect.forked

object ModelSpec extends DefaultRunnableSpec {
  val testAlphabet: Vector[String] = Vector(
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z"
  )
  val testData: Vector[String] = Vector("foo", "foobar", "ook")

  val modelSuite: Spec[Random with TestRandom, TestFailure[Nothing], TestSuccess] = suite("Model")(
    testM("should generate a correct output with correct parameters") {
      for {
        _   <- TestRandom.feedDoubles(0.1)
        m   <- Model.make(testAlphabet, 0, 2)
        _   <- m.retrain(testData)
        res <- m.generate("fo")
      } yield assert(res.isEmpty)(isFalse) &&
        assert(res.get)(equalTo("o"))
    },
    testM("should fail to generate an output from an incorrect input") {
      for {
        _   <- TestRandom.feedDoubles(0.1)
        m   <- Model.make(testAlphabet, 0, 2)
        _   <- m.retrain(testData)
        res <- m.generate("foo")
      } yield assert(res.isEmpty)(isTrue)
    },
    testM(
      "should fail to generate an output if the random value exceeds the " +
        "total sum"
    ) {
      for {
        _   <- TestRandom.feedDoubles(4)
        m   <- Model.make(testAlphabet, 0, 2)
        _   <- m.retrain(testData)
        res <- m.generate("fo")
      } yield assert(res.isEmpty)(isTrue)
    },
    testM(
      "should generate the correct output with correct parameters and " +
        "prior"
    ) {
      for {
        _   <- TestRandom.feedDoubles(0.5)
        m   <- Model.make(testAlphabet, 0.1, 2)
        _   <- m.retrain(testData)
        res <- m.generate("fo")
      } yield assert(res.isEmpty)(isFalse) &&
        assert(res.get)(equalTo("o"))
    },
    testM("should always fail if provided an empty dataset") {
      for {
        _   <- TestRandom.feedDoubles(0.5)
        m   <- Model.make(testAlphabet, 0.1, 2)
        _   <- m.retrain(Vector())
        res <- m.generate("fo")
      } yield assert(res.isEmpty)(isTrue)
    },
    testM("should always fail if provided an empty alphabet") {
      for {
        _   <- TestRandom.feedDoubles(0.5)
        m   <- Model.make(Vector(), 0.1, 2)
        _   <- m.retrain(testData)
        res <- m.generate("fo")
      } yield assert(res.isEmpty)(isTrue)
    },
    testM("should always fail if provided with a wrong context") {
      for {
        _   <- TestRandom.feedDoubles(0.5)
        m   <- Model.make(testAlphabet, 0, 2)
        _   <- m.retrain(testData)
        res <- m.generate("xe")
      } yield assert(res.isEmpty)(isTrue)
    }
  )

  override def spec: Spec[Random with TestRandom, TestFailure[Nothing], TestSuccess] =
    modelSuite @@ forked
}
