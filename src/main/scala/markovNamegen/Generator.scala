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

import scala.collection.immutable

private[markovNamegen] class Generator private (
  data: Data,
  val order: Int,
  val smoothingF: SmoothF[Any, String],
  _models: List[Model]
) {

  def trainAll: ZIO[Any, Nothing, Unit] =
    data match {
      case StringData(data) => foreachPar_(_models)(_.retrain(data))
      case QuantifiedData(data) =>
        foreachPar_(_models)(
          _.retrain(
            data.flatMap(x => List.fill(x._2)(x._1)).toIndexedSeq
          )
        )
      case _ => unit
    }

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

  private def getLetters(words: IndexedSeq[String]) = {
    val aux3 = words.map(_.map(_.toString))
    val aux4 = aux3.flatten
    val aux5 = aux4.distinct
    val aux6 = aux5.sorted
    aux6 :+ "#"
  }

  private def createModels(
    smoothingF: SmoothF[Any, String],
    order: Int,
    letters: IndexedSeq[String]
  ): ZIO[Any, Nothing, List[Model]] =
    foreach(0 until order)(x =>
      Model.make(
        smoothingF,
        order - x,
        letters
      )
    )

  def make(data: Data, smoothingF: SmoothF[Any, String], order: Int): ZIO[Any, Nothing, Generator] =
    data match {
      case StringData(d) =>
        val letters = getLetters(d)
        createModels(smoothingF, order, letters)
          .map(models => new Generator(data, order, smoothingF, models))
      case QuantifiedData(d) =>
        val letters = getLetters(d.flatMap(x => List.fill(x._2)(x._1)).toIndexedSeq)
        createModels(smoothingF, order, letters)
          .map(models => new Generator(data, order, smoothingF, models))
      case TrainedData(models, letters) =>
        foreach(models)(x => Model.make(smoothingF, x._1, letters, x._2))
          .map(models => new Generator(data, order, smoothingF, models))
      case TrainedRefData(models, letters) =>
        val m = models.map(x => Model.make(smoothingF, x._1, letters, x._2))
        ZIO.effectTotal(new Generator(data, order, smoothingF, m.toList))
    }
}
