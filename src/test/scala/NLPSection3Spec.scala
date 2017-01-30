/**
  * Created by tomoya.igarashi.0510@gmail.com on 2017/01/22.
  */

import com.typesafe.scalalogging.LazyLogging

import com.example.Utils._

import java.io.File

import org.specs2.mutable._

class NLPSection3Spec extends Specification with LazyLogging {

  private val gzFilePath = "src/test/resources/jawiki-country.json.gz"
  private val gzFile = new File(gzFilePath)
  private val engFilePath = "src/test/resources/jawiki-england.json"
  private val engFile = new File(engFilePath)

  "NLP 100 section3" >> {
    "generate jawiki-england.json" >> {
      val answer = false
      answer must_== false
    }

    "20. JSONデータの読み込み" >> pending {
      val answer = open(engFile) { ite =>
        ite.length
      }
      answer must_== 24
    }
  }
}
