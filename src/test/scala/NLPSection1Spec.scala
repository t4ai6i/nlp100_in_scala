/**
  * Created by tomoya.igarashi.0510@gmail.com on 2016/10/27.
  */

import org.specs2.mutable._

import scala.collection.immutable.TreeMap

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

    """"Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
      |という文を単語に分解し，各単語の（アルファベットの）文字数を先頭から出現順に並べたリストを作成せよ．""".stripMargin >> {
      val answer = "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
        .split(" ").map(_.count(_.isLetter)).mkString
      answer must_== "314159265358979"
    }

    """"Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
      |という文を単語に分解し，1, 5, 6, 7, 8, 9, 15, 16, 19番目の単語は先頭の1文字，それ以外の単語は先頭に2文字を取り出し，
      |取り出した文字列から単語の位置（先頭から何番目の単語か）への連想配列（辞書型）を作成せよ．""".stripMargin >> {
      val headOnes = Seq(1, 5, 6, 7, 8, 9, 15, 16, 19).map(_ - 1)
      val answer = "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
        .split(" ").zipWithIndex.collect {
        case (w, i) if headOnes.exists(_ == i) => w.substring(0, 1) -> i
        case (w, i) => w.substring(0, 2) -> i
      }.toMap
      answer must_== Map("Cl" -> 16, "N" -> 6, "Be" -> 3, "F" -> 8, "Mi" -> 11, "Na" -> 10, "Ne" -> 9, "Ar" -> 17, "B" -> 4, "Li" -> 2, "P" -> 14, "He" -> 1, "Si" -> 13, "C" -> 5, "H" -> 0, "Ca" -> 19, "K" -> 18, "Al" -> 12, "O" -> 7, "S" -> 15)
    }

    """与えられたシーケンス（文字列やリストなど）からn-gramを作る関数を作成せよ．
      |この関数を用い，"I am an NLPer"という文から単語bi-gram，文字bi-gramを得よ．""".stripMargin >> {
      def ngram[A](n: Int)(xs: Iterable[A]) = xs.sliding(n)
      "文字bi-gram" >> {
        val answer = ngram(2)("I am an NLPer").map(f => f.toString).toVector
        answer must_== Vector("I ", " a", "am", "m ", " a", "an", "n ", " N", "NL", "LP", "Pe", "er")
      }
      "単語bi-gram" >> {
        val answer = ngram(2)("I am an NLPer".split(" ")).map(f => f.toVector).toVector
        answer must_== Vector(Vector("I", "am"), Vector("am", "an"), Vector("an", "NLPer"))
      }
    }
  }
}
