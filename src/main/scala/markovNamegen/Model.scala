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
import braianideroo.random.value._
import zio.ZIO._
import zio.{ Ref, ZIO }

private[markovNamegen] class Model private (
  val order: Int,
  probabilities: Ref[Map[String, RandomVIO[Nothing, Option[String]]]],
  val smoothing: SmoothF[Any, String],
  letters: IndexedSeq[(String, Double)]
) {

  def generate(context: String): ZIO[SeedRandom, Nothing, Option[String]] =
    for {
      maybeChain <- probabilities.get >>= (x => succeed(x.get(context)))
      res <- maybeChain match {
              case Some(ref) => ref
              case None      => ZIO.none
            }
    } yield res

  def retrain(data: IndexedSeq[String]): ZIO[Any, Nothing, Unit] =
    train(data)

  private def train(data: IndexedSeq[String]) =
    for {
      observations <- Ref.make[Map[String, Ref[Probabilities[Any, String]]]](Map())
      _ <- foreach_(data) { element =>
            val parts = getParts(element, order)
            foreach_(parts)(part =>
              for {
                obs <- observations.get
                ref <- obs.get(part._1) match {
                        case Some(value) => succeed(value)
                        case None        => addLetters(part._1, observations)
                      }
                _ <- addPart(part, ref)
              } yield ()
            )
          }
      // smooth the probabilities
      obs <- observations.get
      _ <- foreach_(obs) { x =>
            for {
              aux <- x._2.get
              _   <- aux.smooth(smoothing)
            } yield ()
          }
      x = obs.map(x => (x._1, x._2.get >>= (y => RandomValue.fromMap(y))))
      _ <- probabilities.set(x)
    } yield ()

  private def addLetters(element: String, observations: Ref[Map[String, Ref[Probabilities[Any, String]]]]) =
    for {
      lettersProb <- ZIO.foreach(letters)(x => Probability.make[Any](x._2).map(y => (x._1, y)))
      refVec      <- Ref.make[Probabilities[Any, String]](Map.from(lettersProb))
      _           <- observations.update(_ + (element -> refVec))
    } yield refVec

  private def addPart(part: (String, String), obs: Ref[Probabilities[Any, String]]): ZIO[Any, Nothing, Unit] =
    for {
      probabilities <- obs.get
      probability   = probabilities(part._2)
      _ <- probability.addModifier("model", new Modifier[Any] {
            override def value: ZIO[Any, Nothing, Option[Double]] = ZIO.some(1)
          })
    } yield ()

  def getParts(string: String, order: Int): Vector[(String, String)] = {
    @scala.annotation.tailrec
    def inner(string: String, order: Int, res: Vector[(String, String)]): Vector[(String, String)] =
      if (string.length <= order) res
      else {
        val aux = string.substring(0, order)
        inner(string.tail, order, res :+ (aux, string(order).toString))
      }
    inner(("#" * order) + string + "#", order, Vector())
  }

}

object Model {
  def make(
    smoothingF: SmoothF[Any, String],
    order: Int,
    letters: IndexedSeq[String],
    probabilities: Ref[Map[String, RandomVIO[Nothing, Option[String]]]]
  ): Model =
    new Model(order, probabilities, smoothingF, letters.map(letter => (letter, 0.0)))

  def make(
    smoothingF: SmoothF[Any, String],
    order: Int,
    letters: IndexedSeq[String],
    probabilities: Map[String, RandomVIO[Nothing, Option[String]]]
  ): ZIO[Any, Nothing, Model] =
    Ref
      .make[Map[String, RandomVIO[Nothing, Option[String]]]](probabilities)
      .map(ref => make(smoothingF, order, letters, ref))

  def make(smoothingF: SmoothF[Any, String], order: Int, letters: IndexedSeq[String]): ZIO[Any, Nothing, Model] =
    make(smoothingF, order, letters, Map())
}
