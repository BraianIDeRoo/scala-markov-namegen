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

import braianideroo.random.value.{ RandomVIO, SmoothF }
import zio.Ref

trait Config {
  def smoothing: SmoothF[Any, String]
  def order: Int
  def data: Data
}

sealed trait Data
case class StringData(data: IndexedSeq[String])   extends Data
case class QuantifiedData(data: Map[String, Int]) extends Data
case class TrainedData(models: Map[Int, Map[String, RandomVIO[Nothing, Option[String]]]], letters: IndexedSeq[String])
    extends Data
case class TrainedRefData(
  models: Map[Int, Ref[Map[String, RandomVIO[Nothing, Option[String]]]]],
  letters: IndexedSeq[String]
) extends Data
