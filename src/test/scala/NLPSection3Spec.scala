/**
  * Created by tomoya.igarashi.0510@gmail.com on 2017/01/22.
  */

import com.typesafe.scalalogging.LazyLogging
import com.example.Utils._
import java.io._
import java.util.zip.GZIPInputStream

import org.specs2.mutable._

import util.Properties

class NLPSection3Spec extends Specification with LazyLogging {

  private val gzFilePath = "src/test/resources/jawiki-country.json.gz"
  private val gzFile = new File(gzFilePath)
  private val engFilePath = "src/test/resources/jawiki-england.json"
  private val engFile = new File(engFilePath)

  "NLP 100 section3" >> {
    "generate jawiki-england.json" >> {
      val br = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(gzFile))))
      val sw = new StringWriter()
      val bw = new BufferedWriter(sw)
      //    val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
      bufferedReader2Writer(br, bw) { (line, bw) =>
        bw.write(line)
        bw.newLine()
      }
      val lines = sw.toString
      val answer = lines.split(Properties.lineSeparator).head
      answer must_== false
    }

    "20. JSONデータの読み込み" >> pending {
      val answer = file2iterator(engFile) { ite =>
        ite.length
      }
      answer must_== 24
    }
  }
}
