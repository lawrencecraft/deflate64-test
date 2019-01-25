package deflate64test

import java.io.InputStream

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits._
import org.apache.commons.compress.archivers.zip.{ZipArchiveEntry, ZipFile}

import scala.annotation.tailrec

object Deflate64DecompressionTest extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = args match {
    case archiveName :: fileName :: Nil =>
      import Kompressor._
      import ConsoleIO._

      openZipFile(archiveName).use {
        file =>
          for {
            innerEntries <- file.extractEntries
            entryMap = innerEntries.map(zae => zae.getName -> zae).toMap
            _ <- putStrLn("Files in archive:")
            _ <- entryMap.keys.toList.traverse(putStrLn)
            exitCode <- entryMap.get(fileName) match {
              case Some(entry) =>
                val stream = openStream(file, entry)

                for {
                  count <- stream.use(enumerateStream)
                  _ <- putStrLn(s"The byte size of the entry is $count")
                } yield ExitCode.Success

              case None =>
                ConsoleIO.putError(s"Cannot find the file $fileName in the archive") *> IO(ExitCode.Error)
            }
          } yield exitCode
      }

    case _ =>
      ConsoleIO.putError("Please supply an argument of the file you want to introspect") *> IO(ExitCode.Error)
  }
}

object ConsoleIO {
  def putStrLn(message: String): IO[Unit] = IO(println(message))

  def putError(message: String): IO[Unit] = IO(System.err.println(message))
}

object Kompressor {
  def openZipFile(f: String): Resource[IO, ZipFile] =
    Resource.make(IO(new ZipFile(f)))(zf => IO(zf.close()))

  def openStream(file: ZipFile, entry: ZipArchiveEntry): Resource[IO, InputStream] = {
    val acquireStream = IO(file.getInputStream(entry))
    Resource.make(acquireStream)(stream => IO(stream.close()))
  }

  def enumerateStream(stream: InputStream): IO[Long] = {
    val buf = new Array[Byte](8096)

    @tailrec
    def loop(soFar: Long): Long = {
      val read = stream.read(buf)
      if (read > 0) {
        loop(soFar + read)
      } else {
        soFar
      }
    }

    IO(loop(0))
  }

  implicit class ZipFileSafeOps(zipFile: ZipFile) {
    val extractEntries: IO[List[ZipArchiveEntry]] = IO(zipFile.getEntries).map(enumeratorToList)

    private def enumeratorToList[E](enm: java.util.Enumeration[E]): List[E] = {
      @tailrec
      def loop(soFar: List[E]): List[E] = {
        if (enm.hasMoreElements) {
          loop(enm.nextElement :: soFar)
        } else {
          soFar
        }
      }

      loop(Nil).reverse
    }
  }

}
