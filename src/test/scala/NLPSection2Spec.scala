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

  private def open[X](file: File, charset: Charset = StandardCharsets.UTF_8)(body: Iterator[String] => X) = {
    val lineIterator = FileUtils.lineIterator(file, charset.toString)
    allCatch withApply { t =>
      throw t
    } andFinally {
      LineIterator.closeQuietly(lineIterator)
    } apply {
      body(lineIterator.asScala.map(_.toString))
    }
  }

  private def splitColumn(file: File, separator: String) = open(file) { ite =>
    val split = ite.map(_.split(separator).take(2))
    val (xs, ys) = split.duplicate
    val col1 = xs.map(_ (0)).toVector
    val col2 = ys.map(_ (1)).toVector
    (col1, col2)
  }

  private def divmod(x: Int, y: Int) = (x / y, x % y)

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
        splitColumn(file, "\t")
      }
      val answer = (split._1 zip split._2).headOption
      answer must_== Some(("高知県", "江川崎"))
    }
    "13. col1.txtとcol2.txtをマージ" >> {
      val zipped = allCatch withApply { th =>
        logger.error("error", th)
        Vector.empty[String]
      } apply {
        val (col1, col2) = splitColumn(file, "\t")
        (col1 zip col2).map { zip => s"${zip._1}\t${zip._2}" }
      }
      val answer = zipped.headOption
      answer must_== Some("高知県\t江川崎")
    }
    "14. 先頭からN行を出力" >> {
      val n = 2
      val answer = allCatch withApply { th =>
        logger.error("error", th)
        Vector.empty[String]
      } apply {
        open(file) { ite =>
          ite.take(n).toVector
        }
      }
      answer must_== Vector("高知県	江川崎	41	2013-08-12", "埼玉県	熊谷	40.9	2007-08-16")
    }
    "15. 末尾のN行を出力" >> {
      val n = 2
      val answer = allCatch withApply { th =>
        logger.error("error", th)
        Vector.empty[String]
      } apply {
        open(file) { ite =>
          val(ite1, ite2) = ite.duplicate
          val length = ite1.length
          val tail = length - n
          ite2.drop(tail).toVector
        }
      }
      answer must_== Vector("山形県	鶴岡	39.9	1978-08-03", "愛知県	名古屋	39.9	1942-08-02")
    }
    "16. ファイルをN分割する" >> {
      val n = 5
      val split = allCatch withApply { th =>
        logger.error("error", th)
        Vector.empty[Vector[String]]
      } apply {
        open(file) { ite =>
          val(ite1, ite2) = ite.duplicate
          val length = ite1.length
          val (quotient, rest) = divmod(length, n)
          val N = (rest == 0) match {
            case true => quotient
            case false => quotient + 1
          }
          (0 until n).map { _ => ite2.take(N).toVector }.toVector
        }
      }
      val answer = split.map(_.head)
      answer must_== Vector("高知県	江川崎	41	2013-08-12", "和歌山県	かつらぎ	40.6	1994-08-08", "群馬県	上里見	40.3	1998-07-04", "山形県	酒田	40.1	1978-08-03", "大阪府	豊中	39.9	1994-08-08")
    }
  }
}
