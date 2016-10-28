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

    "「パタトクカシーー」という文字列の1,3,5,7文字目を取り出して連結した文字列を得よ．" >> {
      val answer = "パタトクカシーー".zipWithIndex.collect {
        case (c, i) if i % 2 == 0 => c
      }.mkString
      answer must_== "パトカー"
    }

    "「パトカー」＋「タクシー」の文字を先頭から交互に連結して文字列「パタトクカシーー」を得よ．" >> {
      val answer = ("パトカー" zip "タクシー").map {
        case (a, b) => s"$a$b"
      }.mkString
      answer must_== "パタトクカシーー"
    }

    "\"Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics.\"という文を単語に分解し，各単語の（アルファベットの）文字数を先頭から出現順に並べたリストを作成せよ．" >> {
      val answer = "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
        .split(" ").map(_.count(_.isLetter)).mkString
      answer must_== "314159265358979"
    }
  }
}
