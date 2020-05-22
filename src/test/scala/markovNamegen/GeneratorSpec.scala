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

import zio.test._
import Assertion._
import braianideroo.random.SeedRandom
import braianideroo.random.value.{ SmoothF, Smoothing }
import zio.{ Has, Layer, ZLayer }
import zio.test.TestAspect.forked
import zio.test.{ DefaultRunnableSpec, ZSpec }

object GeneratorSpec extends DefaultRunnableSpec {
  val testData: StringData                     = StringData(Vector("foo", "foobar", "ook"))
  val testAlphabet                             = IndexedSeq("a", "b", "f", "k", "o", "r", "#")
  val testPriorSmoothing: SmoothF[Any, String] = Smoothing.priorSmoothing(0.01)

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
          assert(res)(equalTo("oobar"))
      }
    )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    generatorSuite.provideLayer(randomLayer) @@ forked
}
