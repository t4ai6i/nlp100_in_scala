/**
  * Created by tomoya.igarashi.0510@gmail.com on 2016/10/27.
  */

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.{FileUtils, LineIterator}
import org.specs2.mutable._
import java.nio.charset.{Charset, StandardCharsets}

import scala.collection.JavaConverters._
import scala.util.control.Exception._

class NLPSection2Spec extends Specification with LazyLogging {

  def open[X](file: File, charset: Charset = StandardCharsets.UTF_8)(invalid: Throwable => X, body: Iterator[String] => X) = {
    val lineIterator = FileUtils.lineIterator(file, charset.toString)
    allCatch withApply { t =>
      invalid(t)
    } andFinally {
      LineIterator.closeQuietly(lineIterator)
    } apply {
      body(lineIterator.asScala.map(_.toString))
    }
  }

  private val filePath = "src/test/resources/hightemp.txt"
  private val file = new File(filePath)

  "NLP 100 section2" >> {
    "10. 行数のカウント" >> {
      val answer = open(file)(
        th => 0,
        ite => ite.length
      )
      answer must_== 24
    }
  }
}
