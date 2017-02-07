/**
  * Created by tomoya.igarashi.0510@gmail.com on 2017/01/22.
  */

import com.typesafe.scalalogging.LazyLogging
import com.example.Utils._
import java.io._
import java.util.zip.GZIPInputStream

import org.specs2.mutable._

import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.util.Properties._

class NLPSection3Spec extends Specification with LazyLogging {

  private val gzFilePath = "src/test/resources/jawiki-country.json.gz"
  private val gzFile = new File(gzFilePath)
  private val engFilePath = "src/test/resources/jawiki-england.json"
  private val engFile = new File(engFilePath)

  case class Article(title: String, text: String)

  implicit val locationFormat: Format[Article] = (
    (JsPath \ "title").format[String] and
      (JsPath \ "text").format[String]
    ) (Article.apply, unlift(Article.unapply))

  def iterator2Articles(ite: Iterator[String]): Iterator[Article] = {
    val results = for {
      str <- ite
    } yield {
      Json.parse(str).validate[Article]
    }
    for {
      result <- results if result.isSuccess
    } yield {
      result.get
    }
  }

  "NLP 100 section3" >> {
    "20. JSONデータの読み込み" >> pending {
      val br = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(gzFile))))
      val sw = new StringWriter()
      val bw = new BufferedWriter(sw)
      //    val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
      bufferedReader2Writer(br, bw) { (line, bw) =>
        val jsResult = Json.parse(line).validate[Article]
        for {
          article <- jsResult
          title = article.title if title == "イギリス"
        } {
          bw.write(title)
          bw.newLine()
        }
      }
      val lines = sw.toString
      val answer = lines.split(lineSeparator).head
      answer must_== "イギリス"
    }

    "21. カテゴリ名を含む行を抽出" >> {
      val answer = file2iterator(engFile) { ite =>
        val articles = iterator2Articles(ite)
        val categories = for {
          article <- articles
          lines = article.text.split(lineSeparator)
          line <- lines if line.startsWith("[[Category:")
        } yield {
          line
        }
        categories.toVector
      }
      answer must_== Vector("[[Category:イギリス|*]]", "[[Category:英連邦王国|*]]", "[[Category:G8加盟国]]", "[[Category:欧州連合加盟国]]", "[[Category:海洋国家]]", "[[Category:君主国]]", "[[Category:島国|くれいとふりてん]]", "[[Category:1801年に設立された州・地域]]")
    }
  }
}
