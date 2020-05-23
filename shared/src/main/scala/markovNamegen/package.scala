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

import braianideroo.random.value.{ RandomVIO, SmoothF }
import braianideroo.random.{ SeedRandom, SeedRandomError }
import zio.ZIO._
import zio.{ Has, ZIO, ZLayer }

package object markovNamegen {
  type StringGenerator = Has[StringGenerator.Service]

  object StringGenerator {
    trait Service {
      def generate(
        number: Int,
        groupOptions: List[GroupGenerationOption],
        perStringOptions: List[SingleGenerationOption]
      ): ZIO[Any, SeedRandomError, List[String]]

      def train: ZIO[Any, Nothing, Option[List[Map[String, RandomVIO[Nothing, Option[String]]]]]]
    }

    case class StringGeneratorLiveConfig(smoothing: SmoothF[Any, String], order: Int, data: Data) extends Config

    private val liveF: ZIO[Has[StringGeneratorLiveConfig] with SeedRandom, Nothing, Service] = {
      for {
        config     <- ZIO.access[Has[StringGeneratorLiveConfig]](x => x.get)
        smoothingF = config.smoothing
        order      = config.order
        data       = config.data
        random     <- ZIO.access[SeedRandom](x => x)
        g          <- Generator.make(data, smoothingF, order)
      } yield new Service {

        private def repeatTask[R, E](
          task: ZIO[R, E, Option[String]],
          perStringOptions: List[SingleGenerationOption]
        ): ZIO[R, E, String] =
          task >>= {
            case Some(value) if perStringOptions.forall(_.check(value)) =>
              ZIO.succeed(value)
            case _ => repeatTask(task, perStringOptions)
          }

        override def generate(
          number: Int,
          groupOptions: List[GroupGenerationOption],
          perStringOptions: List[SingleGenerationOption]
        ): ZIO[Any, SeedRandomError, List[String]] =
          (for {
            seeds   <- foreach(0 until number)(_ => braianideroo.random.nextLong)
            randoms = seeds.map(s => ZLayer.succeed(s) >>> braianideroo.random.SeedRandom.live.fresh)
            aux     <- foreachPar(randoms)(r => repeatTask(g.generate, perStringOptions).provideLayer(r))

          } yield aux).provide(random)

        override def train: ZIO[Any, Nothing, Option[List[Map[String, RandomVIO[Nothing, Option[String]]]]]] =
          g.trainAll
      }
    }

    val Live: ZLayer[Has[StringGeneratorLiveConfig] with SeedRandom, Nothing, StringGenerator] =
      ZLayer.fromEffect(liveF)

    def generate(
      number: Int,
      groupOptions: List[GroupGenerationOption],
      perStringOptions: List[SingleGenerationOption]
    ): ZIO[StringGenerator, SeedRandomError, List[String]] =
      for {
        generator <- ZIO.access[StringGenerator](x => x.get)
        res       <- generator.generate(number, groupOptions, perStringOptions)
      } yield res

  }

}
