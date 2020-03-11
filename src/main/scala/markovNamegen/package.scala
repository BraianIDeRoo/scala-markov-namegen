import zio.ZIO._
import zio.duration.Duration
import zio.random.Random
import zio.{ Has, IO, Ref, ZIO, ZLayer }

package object markovNamegen {
  type StringGenerator = Has[StringGenerator.Service]

  object StringGenerator {
    trait Service {
      def generate(
        groupOptions: List[GroupGenerationOption],
        perStringOptions: List[SingleGenerationOption]
      ): ZIO[Random, Nothing, List[String]]
    }

    private val liveF: ZIO[Has[Double] with Has[Int] with Has[Vector[String]], Nothing, Service] = {
      for {
        prior <- ZIO.access[Has[Double]](x => x.get)
        order <- ZIO.access[Has[Int]](x => x.get)
        data  <- ZIO.access[Has[Vector[String]]](x => x.get)
        g     <- Generator.make(data, prior, order)
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
          groupOptions: List[GroupGenerationOption],
          perStringOptions: List[SingleGenerationOption]
        ): ZIO[Random, Nothing, List[String]] =
          for {
            finishedJobs <- Ref.make[List[String]](List())
            taskNumber   = groupOptions.collectFirst { case x: Number => x.value }.getOrElse(1)
            _            <- foreachPar_(0 until taskNumber)(_ => tryTask(g.generate, perStringOptions, finishedJobs))
            res          <- finishedJobs.get
          } yield res
      }
    }

    val Live: ZLayer[Has[Double] with Has[Int] with Has[Vector[String]], Nothing, StringGenerator] =
      ZLayer.fromEffect(liveF)

    def generate(
      groupOptions: List[GroupGenerationOption],
      perStringOptions: List[SingleGenerationOption]
    ): ZIO[Random with markovNamegen.StringGenerator, Nothing, List[String]] =
      for {
        generator <- ZIO.access[StringGenerator](x => x.get)
        res       <- generator.generate(groupOptions, perStringOptions)
      } yield res

  }

}
