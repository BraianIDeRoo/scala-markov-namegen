import braianideroo.random.SeedRandom
import braianideroo.random.value.SmoothF
import zio.ZIO._
import zio.{ Has, Ref, ZIO, ZLayer }

package object markovNamegen {
  type StringGenerator = Has[StringGenerator.Service]

  object StringGenerator {
    trait Service {
      def generate(
        number: Int,
        groupOptions: List[GroupGenerationOption],
        perStringOptions: List[SingleGenerationOption]
      ): ZIO[Any, Nothing, List[String]]
    }

    case class StringGeneratorLiveConfig(smoothingF: SmoothF[String], order: Int, data: Vector[String])

    private val liveF: ZIO[Has[StringGeneratorLiveConfig] with SeedRandom, Nothing, Service] = {
      for {
        config     <- ZIO.access[Has[StringGeneratorLiveConfig]](x => x.get)
        smoothingF = config.smoothingF
        order      = config.order
        data       = config.data
        random     <- ZIO.access[SeedRandom](x => x)
        g          <- Generator.make(data, smoothingF, order)
      } yield new Service {

        private def tryTask[R, E](
          task: ZIO[R, E, Option[String]],
          perStringOptions: List[SingleGenerationOption],
          finishedJobs: Ref[List[String]]
        ): ZIO[R, E, Unit] =
          task >>= {
            case Some(value) if perStringOptions.forall(_.check(value)) =>
              finishedJobs.update(_ :+ value).unit
            case _ => tryTask(task, perStringOptions, finishedJobs)
          }

        override def generate(
          number: Int,
          groupOptions: List[GroupGenerationOption],
          perStringOptions: List[SingleGenerationOption]
        ): ZIO[Any, Nothing, List[String]] =
          (for {
            finishedJobs <- Ref.make[List[String]](List())
            _            <- foreach_(0 until number)(_ => tryTask(g.generate, perStringOptions, finishedJobs))
            res          <- finishedJobs.get
          } yield res).provide(random)
      }
    }

    val Live: ZLayer[Has[StringGeneratorLiveConfig] with SeedRandom, Nothing, StringGenerator] =
      ZLayer.fromEffect(liveF)

    def generate(
      number: Int,
      groupOptions: List[GroupGenerationOption],
      perStringOptions: List[SingleGenerationOption]
    ): ZIO[StringGenerator, Nothing, List[String]] =
      for {
        generator <- ZIO.access[StringGenerator](x => x.get)
        res       <- generator.generate(number, groupOptions, perStringOptions)
      } yield res

  }

}
