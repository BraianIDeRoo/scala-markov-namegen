/*
 * Copyright 2020 Braian De Roo
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package markovNamegen

import braianideroo.random.SeedRandom
import braianideroo.random.value.{ SmoothF, Smoothing }
import zio.test.Assertion._
import zio.test.TestAspect.forked
import zio.test._
import zio.{ Has, Layer, ZLayer }

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
  val testData: Vector[String]            = Vector("foo", "foobar", "ook")
  val testPriorSmoothing: SmoothF[String] = Smoothing.priorSmoothing(0.1)
  val testNoSmoothing: SmoothF[String]    = Smoothing.noSmoothing

  val seed: Layer[Nothing, Has[Long]] = ZLayer.succeed(501L)
  val randomLayer: ZLayer[Any, Nothing, Has[SeedRandom.Service]] =
    seed >>> braianideroo.random.SeedRandom.live

  val modelSuite2: Spec[SeedRandom, TestFailure[Nothing], TestSuccess] =
    suite("Model order 2")(
      testM("should generate a correct output with correct parameters") {
        for {
          m   <- Model.make(testNoSmoothing, 2, testAlphabet)
          _   <- m.retrain(testData)
          res <- m.generate("fo")
        } yield assert(res.isEmpty)(isFalse) &&
          assert(res.get)(equalTo("o"))
      },
      testM("should fail to generate an output from an incorrect input") {
        for {
          m   <- Model.make(testNoSmoothing, 2, testAlphabet)
          _   <- m.retrain(testData)
          res <- m.generate("foo")
        } yield assert(res.isEmpty)(isTrue)
      },
      testM(
        "should generate the correct output with correct parameters and " +
          "prior"
      ) {
        for {
          m   <- Model.make(testPriorSmoothing, 2, testAlphabet)
          _   <- m.retrain(testData)
          res <- m.generate("fo")
        } yield assert(res.isEmpty)(isFalse) &&
          assert(res.get)(equalTo("o"))
      },
      testM("should always fail if provided an empty dataset") {
        for {
          m   <- Model.make(testPriorSmoothing, 2, testAlphabet)
          _   <- m.retrain(Vector())
          res <- m.generate("fo")
        } yield assert(res.isEmpty)(isTrue)
      },
      testM("should always fail if provided with a wrong context") {
        for {
          m   <- Model.make(testNoSmoothing, 2, testAlphabet)
          _   <- m.retrain(testData)
          res <- m.generate("xe")
        } yield assert(res.isEmpty)(isTrue)
      }
    )

  val modelSuite1: Spec[SeedRandom, TestFailure[Nothing], TestSuccess] =
    suite("Model order 1")(
      testM("should generate a correct output with correct parameters") {
        for {
          m   <- Model.make(testNoSmoothing, 1, testAlphabet)
          _   <- m.retrain(testData)
          res <- m.generate("f")
        } yield assert(res.isEmpty)(isFalse) &&
          assert(res.get)(equalTo("o"))
      },
      testM("should fail to generate an output from an incorrect input") {
        for {
          m   <- Model.make(testNoSmoothing, 1, testAlphabet)
          _   <- m.retrain(testData)
          res <- m.generate("fo")
        } yield assert(res.isEmpty)(isTrue)
      },
      testM(
        "should generate the correct output with correct parameters and " +
          "prior"
      ) {
        for {
          m   <- Model.make(testPriorSmoothing, 1, testAlphabet)
          _   <- m.retrain(testData)
          res <- m.generate("f")
        } yield assert(res.isEmpty)(isFalse) &&
          assert(res.get)(equalTo("o"))
      },
      testM("should always fail if provided an empty dataset") {
        for {
          m   <- Model.make(testPriorSmoothing, 1, testAlphabet)
          _   <- m.retrain(Vector())
          res <- m.generate("f")
        } yield assert(res.isEmpty)(isTrue)
      },
      testM("should always fail if provided with a wrong context") {
        for {
          m   <- Model.make(testNoSmoothing, 1, testAlphabet)
          _   <- m.retrain(testData)
          res <- m.generate("x")
        } yield assert(res.isEmpty)(isTrue)
      }
    )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    suite("all Models")(
      modelSuite2 @@ forked,
      modelSuite1 @@ forked
    ).provideLayer(randomLayer) @@ forked
}
