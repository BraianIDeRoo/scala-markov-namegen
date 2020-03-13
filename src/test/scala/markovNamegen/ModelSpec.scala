package markovNamegen

import zio.test._
import Assertion._
import markovNamegen.util.UniqueVector
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
    "z",
    "#"
  )
  val testData: Vector[String] = Vector("foo", "foobar", "ook")
  val observationMap2: Map[String, UniqueVector[String]] = Map(
    "#o" -> UniqueVector("o"),
    "#f" -> UniqueVector("o"),
    "ob" -> UniqueVector("a"),
    "fo" -> UniqueVector("o"),
    "ok" -> UniqueVector("#"),
    "oo" -> UniqueVector("#", "b", "k"),
    "##" -> UniqueVector("f", "o"),
    "ba" -> UniqueVector("r"),
    "ar" -> UniqueVector("#")
  )
  val chainMap2: Map[String, Vector[Double]] = Map(
    "#o" -> Vector(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    "#f" -> Vector(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    "ob" -> Vector(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    "fo" -> Vector(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    "ok" -> Vector(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0),
    "oo" -> Vector(0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0),
    "##" -> Vector(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    "ba" -> Vector(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    "ar" -> Vector(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0)
  )
  val testPriorSmoothing: Smoothing = PriorSmoothing(0.1)
  val testNoSmoothing: Smoothing    = NoSmoothing

  val modelSuite2: Spec[Random with TestRandom, TestFailure[Nothing], TestSuccess] =
    suite("Model order 2")(
      testM("should generate a correct output with correct parameters") {
        for {
          _            <- TestRandom.feedDoubles(0.1)
          m            <- Model.make(testAlphabet, testNoSmoothing, 2)
          _            <- m.retrain(testData)
          observations <- m.observations
          chains       <- m.chains
          res          <- m.generate("fo")
        } yield assert(res.isEmpty)(isFalse) &&
          assert(res.get)(equalTo("o")) &&
          assert(observations)(equalTo(observationMap2)) &&
          assert(chains)(equalTo(chainMap2))
      },
      testM("should fail to generate an output from an incorrect input") {
        for {
          _   <- TestRandom.feedDoubles(0.1)
          m   <- Model.make(testAlphabet, testNoSmoothing, 2)
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
          m   <- Model.make(testAlphabet, testNoSmoothing, 2)
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
          m   <- Model.make(testAlphabet, testPriorSmoothing, 2)
          _   <- m.retrain(testData)
          res <- m.generate("fo")
        } yield assert(res.isEmpty)(isFalse) &&
          assert(res.get)(equalTo("o"))
      },
      testM("should always fail if provided an empty dataset") {
        for {
          _   <- TestRandom.feedDoubles(0.5)
          m   <- Model.make(testAlphabet, testPriorSmoothing, 2)
          _   <- m.retrain(Vector())
          res <- m.generate("fo")
        } yield assert(res.isEmpty)(isTrue)
      },
      testM("should always fail if provided an empty alphabet") {
        for {
          _   <- TestRandom.feedDoubles(0.5)
          m   <- Model.make(Vector(), testPriorSmoothing, 2)
          _   <- m.retrain(testData)
          res <- m.generate("fo")
        } yield assert(res.isEmpty)(isTrue)
      },
      testM("should always fail if provided with a wrong context") {
        for {
          _   <- TestRandom.feedDoubles(0.5)
          m   <- Model.make(testAlphabet, testNoSmoothing, 2)
          _   <- m.retrain(testData)
          res <- m.generate("xe")
        } yield assert(res.isEmpty)(isTrue)
      }
    )
  val modelSuite1: Spec[Random with TestRandom, TestFailure[Nothing], TestSuccess] = suite("Model order 1")(
    testM("should generate a correct output with correct parameters") {
      for {
        _   <- TestRandom.feedDoubles(0.1)
        m   <- Model.make(testAlphabet, testNoSmoothing, 1)
        _   <- m.retrain(testData)
        res <- m.generate("f")
      } yield assert(res.isEmpty)(isFalse) &&
        assert(res.get)(equalTo("o"))
    },
    testM("should fail to generate an output from an incorrect input") {
      for {
        _   <- TestRandom.feedDoubles(0.1)
        m   <- Model.make(testAlphabet, testNoSmoothing, 1)
        _   <- m.retrain(testData)
        res <- m.generate("fo")
      } yield assert(res.isEmpty)(isTrue)
    },
    testM(
      "should fail to generate an output if the random value exceeds the " +
        "total sum"
    ) {
      for {
        _   <- TestRandom.feedDoubles(4)
        m   <- Model.make(testAlphabet, testNoSmoothing, 1)
        _   <- m.retrain(testData)
        res <- m.generate("f")
      } yield assert(res.isEmpty)(isTrue)
    },
    testM(
      "should generate the correct output with correct parameters and " +
        "prior"
    ) {
      for {
        _   <- TestRandom.feedDoubles(0.5)
        m   <- Model.make(testAlphabet, testPriorSmoothing, 1)
        _   <- m.retrain(testData)
        res <- m.generate("f")
      } yield assert(res.isEmpty)(isFalse) &&
        assert(res.get)(equalTo("o"))
    },
    testM("should always fail if provided an empty dataset") {
      for {
        _   <- TestRandom.feedDoubles(0.5)
        m   <- Model.make(testAlphabet, testPriorSmoothing, 1)
        _   <- m.retrain(Vector())
        res <- m.generate("f")
      } yield assert(res.isEmpty)(isTrue)
    },
    testM("should always fail if provided an empty alphabet") {
      for {
        _   <- TestRandom.feedDoubles(0.5)
        m   <- Model.make(Vector(), testPriorSmoothing, 1)
        _   <- m.retrain(testData)
        res <- m.generate("f")
      } yield assert(res.isEmpty)(isTrue)
    },
    testM("should always fail if provided with a wrong context") {
      for {
        _   <- TestRandom.feedDoubles(0.5)
        m   <- Model.make(testAlphabet, testNoSmoothing, 1)
        _   <- m.retrain(testData)
        res <- m.generate("x")
      } yield assert(res.isEmpty)(isTrue)
    }
  )
  override def spec: Spec[Random with TestRandom, TestFailure[Nothing], TestSuccess] =
    suite("all Models")(
      modelSuite2 @@ forked,
      modelSuite1 @@ forked
    ) @@ forked
}
