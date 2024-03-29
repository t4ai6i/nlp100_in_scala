package com.example

import org.apache.commons.io.FileUtils
import java.io._
import java.nio.charset.{Charset, StandardCharsets}
import java.util.zip.ZipInputStream

import scala.collection.JavaConverters._
import scala.util.Try
import scala.util.control.Exception._

/**
  * Created by t4ai6i on 2017/01/22.
  */
object Utils {
  def file2iterator[T](file: File, charset: Charset = StandardCharsets.UTF_8)(body: Iterator[String] => T): Try[T] = Try {
    type Closable = {def close(): Unit}

    def using[A <: Closable, B](resource: A)(f: A => B) = try {
      f(resource)
    } finally {
      resource.close
    }

    using(FileUtils.lineIterator(file, charset.toString))(lineIterator => body(lineIterator.asScala))
  }

  def write(read: File, bw: BufferedWriter)(body: (String, BufferedWriter) => Unit) = {
    file2iterator(read) { ite =>
      allCatch withApply { t =>
        throw t
      } andFinally {
        bw.close()
      } apply {
        ite.takeWhile(_ => ite.hasNext).foreach { _ =>
          body(ite.next(), bw)
        }
        bw.flush()
      }
    }
  }

  def bufferedReader2Writer(br: BufferedReader, bw: BufferedWriter)(body: (String, BufferedWriter) => Unit) = {
    allCatch withApply { t =>
      throw t
    } andFinally {
      bw.close()
      br.close()
    } apply {
      val lines = Iterator.continually(br.readLine()).takeWhile(_ != null)
      for {
        line <- lines
      } {
        body(line, bw)
      }
      bw.flush()
    }
  }

  def unzip(zipFile: File) = {
    val zis = new ZipInputStream(new FileInputStream(zipFile))
    allCatch withApply { t =>
      throw t
    } andFinally {
      zis.closeEntry()
      zis.close()
    } apply {
      val buffer = new Array[Byte](1024)
      val zes = Iterator.continually(zis.getNextEntry).takeWhile(_ != null)
      for {
        ze <- zes
      } yield {
        val file = ze.getName
        ze.isDirectory
        ze.getComment
        ze.getCompressedSize
        ze.toString
        val fos = new FileOutputStream(file);
        Iterator.continually(zis.read(buffer)).takeWhile(_ > 0).foreach { len =>
          fos.write(buffer, 0, len)
        }
      }
    }
  }
}
