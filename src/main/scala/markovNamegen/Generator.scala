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
import braianideroo.random.value.SmoothF
import zio.ZIO
import zio.ZIO._

private[markovNamegen] class Generator private (
  data: IndexedSeq[String],
  val order: Int,
  val smoothingF: SmoothF[Any, String],
  _models: List[Model]
) {

  def trainAll: ZIO[Any, Nothing, Unit] =
    foreachPar_(_models)(_.retrain(data))

  def generate: ZIO[SeedRandom, Nothing, Option[String]] = {
    val word = "#" * order

    def aux(s: String): ZIO[SeedRandom, Nothing, Option[String]] =
      for {
        l <- getLetter(s)
        res <- l match {
                case Some(value) => inner(s, value)
                case None        => succeed(Some(s))
              }
      } yield res

    def inner(word: String, letter: String): ZIO[SeedRandom, Nothing, Option[String]] =
      if (letter == "#")
        if (word.isEmpty) none
        else succeed(Some(word))
      else {
        val w = word + letter
        aux(w).map {
          case Some(value) =>
            val k = value.replace("#", "")
            if (k.isEmpty) None
            else Some(k)
          case None => None
        }

      }

    aux(word)
  }

  private def getLetter(context: String) = {
    val ctx = context.substring(context.length - order, context.length)

    def inner(models: List[Model], ctx: String): ZIO[SeedRandom, Nothing, Option[String]] =
      models match {
        case Nil          => none
        case model :: Nil => model.generate(ctx)
        case model :: others =>
          model.generate(ctx) >>= {
            case Some(value) => succeed(Some(value))
            case None        => inner(others, ctx.drop(1))
          }
      }
    inner(_models, ctx)
  }
}

object Generator {
  def make(data: IndexedSeq[String], smoothingF: SmoothF[Any, String], order: Int): ZIO[Any, Nothing, Generator] = {
    val letters =
      data.flatMap(x => x.toList.map(_.toString)).sorted.distinct :+ "#"
    for {
      models <- foreach(0 until order)(x =>
                 Model.make(
                   smoothingF,
                   order - x,
                   letters
                 )
               )
    } yield new Generator(data, order, smoothingF, models)

  }.tap(_.trainAll)
}
