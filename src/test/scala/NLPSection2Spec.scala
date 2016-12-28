/**
  * Created by tomoya.igarashi.0510@gmail.com on 2016/10/27.
  */

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils
import org.specs2.mutable._
import scala.collection.JavaConversions._

class NLPSection2Spec extends Specification with LazyLogging {

  private val filePath = "src/test/resources/hightemp.txt"
  private val file = new File(filePath)
  private val lines = FileUtils.readLines(file)

  "NLP 100 section2" >> {
    "10. 行数のカウント" >> {
      val answer = lines.length
      answer must_== 24
    }
  }
}
