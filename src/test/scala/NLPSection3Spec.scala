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

  def validatedArticles(ite: Iterator[String]): (Iterator[Article], Iterator[JsError]) = {
    val xs = for {
      str <- ite
    } yield {
      Json.parse(str).validate[Article]
    }
    val (ite1, ite2) = xs.duplicate
    val articles = for {
      r <- ite1 if r.isSuccess
      opt = r.asOpt
      article <- opt
    } yield {
      article
    }
    val errors = jsErrorIterator(ite2)
    (articles, errors)
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
        val (articles, _) = validatedArticles(ite)
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

    "22. カテゴリ名の抽出" >> {
      val answer = file2iterator(engFile) { ite =>
        val r = """\[\[Category:(.*)\]\]""".r
        val (articles, _) = validatedArticles(ite)
        val names = for {
          article <- articles
          lines = article.text.split(lineSeparator)
          line <- lines
          matched = r.findFirstMatchIn(line)
          matched <- matched
          name = matched.group(1)
        } yield {
          name
        }
        names.toVector
      }
      answer must_== Vector("イギリス|*", "英連邦王国|*", "G8加盟国", "欧州連合加盟国", "海洋国家", "君主国", "島国|くれいとふりてん", "1801年に設立された州・地域")
    }

    "24. ファイル参照の抽出" >> {
      val answer = file2iterator(engFile) { ite =>
        val r = """(File|ファイル):(.*?)\|""".r
        val (articles, _) = validatedArticles(ite)
        val names = for {
          article <- articles
          lines = article.text.split(lineSeparator)
          line <- lines
          matched = r.findFirstMatchIn(line)
          matched <- matched
          name = matched.group(2)
        } yield {
          name
        }
        names.toVector
      }
      answer must_== Vector(
        "Royal Coat of Arms of the United Kingdom.svg",
        "Battle of Waterloo 1815.PNG",
        "The British Empire.png",
        "Uk topo en.jpg",
        "BenNevis2005.jpg",
        "Elizabeth II greets NASA GSFC employees, May 8, 2007 edit.jpg",
        "Palace of Westminster, London - Feb 2007.jpg",
        "David Cameron and Barack Obama at the G20 Summit in Toronto.jpg",
        "Soldiers Trooping the Colour, 16th June 2007.jpg",
        "Scotland Parliament Holyrood.jpg",
        "London.bankofengland.arp.jpg",
        "City of London skyline from London City Hall - Oct 2008.jpg",
        "Oil platform in the North SeaPros.jpg",
        "Eurostar at St Pancras Jan 2008.jpg",
        "Heathrow T5.jpg",
        "Anglospeak.svg",
        "CHANDOS3.jpg",
        "The Fabs.JPG",
        "PalaceOfWestminsterAtNight.jpg",
        "Westminster Abbey - West Door.jpg",
        "Edinburgh Cockburn St dsc06789.jpg",
        "Canterbury Cathedral - Portal Nave Cross-spire.jpeg",
        "Kew Gardens Palm House, London - July 2009.jpg",
        "2005-06-27 - United Kingdom - England - London - Greenwich.jpg",
        "Stonehenge2007 07 30.jpg",
        "Yard2.jpg",
        "Durham Kathedrale Nahaufnahme.jpg",
        "Roman Baths in Bath Spa, England - July 2006.jpg",
        "Fountains Abbey view02 2005-08-27.jpg",
        "Blenheim Palace IMG 3673.JPG",
        "Liverpool Pier Head by night.jpg",
        "Hadrian's Wall view near Greenhead.jpg",
        "London Tower (1).JPG",
        "Wembley Stadium, illuminated.jpg")
    }
  }
}
