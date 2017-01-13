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

  def open[X](file: File, charset: Charset = StandardCharsets.UTF_8)(body: Iterator[String] => X) = {
    val lineIterator = FileUtils.lineIterator(file, charset.toString)
    allCatch withApply { t =>
      throw t
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
      val answer = allCatch withApply { th =>
        logger.error("error", th)
        0
      } apply {
        open(file) { ite =>
          ite.length
        }
      }
      answer must_== 24
    }
    "11. タブをスペースに置換" >> {
      val xs = allCatch withApply { th =>
        logger.error("error", th)
        Vector.empty[String]
      } apply {
        open(file) { ite =>
            ite.map(_.replaceAll("\t", " ")).toVector
        }
      }
      val answer = xs.headOption
      answer must_== Some("高知県 江川崎 41 2013-08-12")
    }
    "12. 1列目をcol1.txtに，2列目をcol2.txtに保存" >> {
      val split = allCatch withApply { th =>
        logger.error("error", th)
        (Vector.empty[String], Vector.empty[String])
      } apply {
        open(file) { ite =>
          val split = ite.map(_.split("\t").take(2))
          val (xs, ys) = split.duplicate
          val col1 = xs.map(_ (0)).toVector
          val col2 = ys.map(_ (1)).toVector
          (col1, col2)
        }
      }
      val answer = (split._1 zip split._2).headOption
      answer must_== Some(("高知県", "江川崎"))
    }
  }
}
