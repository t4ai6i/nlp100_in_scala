/**
  * Created by tomoya.igarashi.0510@gmail.com on 2016/10/27.
  */
import org.specs2.mutable._

class NLPSection1Spec extends Specification {
  "NLP 100 section1" >> {
    "文字列\"stressed\"の文字を逆に（末尾から先頭に向かって）並べた文字列を得よ．" >> {
      val answer = "stressed".reverse
       answer must_== "desserts"
    }
  }
}
