/**
  * Created by t4ai6i on 2016/10/27.
  */

import com.typesafe.scalalogging.LazyLogging

import com.example.Utils._

import java.io.File

import org.specs2.mutable._

class NLPSection2Spec extends Specification with LazyLogging {

  private def splitColumn(file: File, separator: String) = file2iterator(file) { ite =>
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
      val tryCount = file2iterator(file) { ite =>
        ite.length
      }
      val answer = tryCount.get
      answer must_== 24
    }
    "11. タブをスペースに置換" >> {
      val tryLines = file2iterator(file) { ite =>
        ite.map(_.replaceAll("\t", " ")).toVector
      }
      val answer = tryLines.get.headOption
      answer must_== Some("高知県 江川崎 41 2013-08-12")
    }
    "12. 1列目をcol1.txtに，2列目をcol2.txtに保存" >> {
      val trySplit = splitColumn(file, "\t")
      val split = trySplit.get
      val answer = (split._1 zip split._2).headOption
      answer must_== Some(("高知県", "江川崎"))
    }
    "13. col1.txtとcol2.txtをマージ" >> {
      val trySplit = splitColumn(file, "\t")
      val (col1, col2) = trySplit.get
      val zipped = (col1 zip col2).map { zip => s"${zip._1}\t${zip._2}" }
      val answer = zipped.headOption
      answer must_== Some("高知県\t江川崎")
    }
    "14. 先頭からN行を出力" >> {
      val n = 2
      val tryHeadN = file2iterator(file) { ite =>
        ite.take(n).toVector
      }
      val answer = tryHeadN.get
      answer must_== Vector("高知県	江川崎	41	2013-08-12", "埼玉県	熊谷	40.9	2007-08-16")
    }
    "15. 末尾のN行を出力" >> {
      val n = 2
      val tryVector = file2iterator(file) { ite =>
        val (ite1, ite2) = ite.duplicate
        val length = ite1.length
        val tail = length - n
        ite2.drop(tail).toVector
      }
      val answer = tryVector.get
      answer must_== Vector("山形県	鶴岡	39.9	1978-08-03", "愛知県	名古屋	39.9	1942-08-02")
    }
    "16. ファイルをN分割する" >> {
      val n = 5
      val trySplit = file2iterator(file) { ite =>
        val (ite1, ite2) = ite.duplicate
        val length = ite1.length
        val (quotient, rest) = divmod(length, n)
        val N = (rest == 0) match {
          case true => quotient
          case false => quotient + 1
        }
        (0 until n).map { _ => ite2.take(N).toVector }.toVector
      }
      val answer = trySplit.get.map(_.head)
      answer must_== Vector("高知県	江川崎	41	2013-08-12", "和歌山県	かつらぎ	40.6	1994-08-08", "群馬県	上里見	40.3	1998-07-04", "山形県	酒田	40.1	1978-08-03", "大阪府	豊中	39.9	1994-08-08")
    }
    "17. １列目の文字列の異なり" >> {
      val trySet = file2iterator(file) { ite =>
        ite.map(_.split("\t").head).toSet
      }
      val answer = trySet.get
      answer must_== Set("高知県", "愛媛県", "愛知県", "埼玉県", "群馬県", "千葉県", "山梨県", "大阪府", "山形県", "和歌山県", "岐阜県", "静岡県")
    }
    "18. 各行を3コラム目の数値の降順にソート" >> {
      val trySorted = file2iterator(file) { ite =>
        ite.map { str =>
          str.split("\t") match {
            case Array(a, b, c, d) => (c.toDouble, s"${a}\t${b}\t${c}\t${d}")
          }
        }.toVector.sortWith((x, y) => x._1 > y._1).map(_._2)
      }
      val answer = trySorted.get.map(_.split("\t").apply(2).toDouble)
      answer must_== Vector(41.0, 40.9, 40.9, 40.8, 40.7, 40.6, 40.6, 40.5, 40.4, 40.3, 40.3, 40.3, 40.2, 40.2, 40.2, 40.1, 40.0, 40.0, 39.9, 39.9, 39.9, 39.9, 39.9, 39.9)
    }
    "19. 各行の1コラム目の文字列の出現頻度を求め，出現頻度の高い順に並べる" >> {
      val trySorted = file2iterator(file) { ite =>
        val grouped = ite.map(_.split("\t")(0)).toVector.groupBy(identity)
        grouped.map(t => (t._1, t._2.length)).toIndexedSeq.sortWith((t1, t2) => t1._2 > t2._2)
      }
      val answer = trySorted.get
      answer must_== Vector(("埼玉県", 3), ("群馬県", 3), ("山梨県", 3), ("山形県", 3), ("愛知県", 2), ("千葉県", 2), ("岐阜県", 2), ("静岡県", 2), ("高知県", 1), ("愛媛県", 1), ("大阪府", 1), ("和歌山県", 1))
    }
  }
}
