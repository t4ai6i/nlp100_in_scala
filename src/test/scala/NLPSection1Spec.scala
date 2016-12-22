/**
  * Created by tomoya.igarashi.0510@gmail.com on 2016/10/27.
  */

import org.specs2.mutable._

import scala.collection.immutable.TreeMap

class NLPSection1Spec extends Specification {

  def ngram[A](n: Int)(xs: Iterable[A]) = xs.sliding(n)
  def atXoclockYisZ(x: Int)(y: String)(z: Double) = s"${x}時の${y}は${z}"
  def cipher(str: String) = str.toCharArray.map {
    case c if c.isLower => (219 - c).toChar
    case c => c
  }.mkString

  "NLP 100 section1" >> {
    "00.文字列\"stressed\"の文字を逆に（末尾から先頭に向かって）並べた文字列を得よ．" >> {
      val answer = "stressed".reverse
      answer must_== "desserts"
    }

    "01.「パタトクカシーー」という文字列の1,3,5,7文字目を取り出して連結した文字列を得よ．" >> {
      val answer = "パタトクカシーー".zipWithIndex.collect {
        case (c, i) if i % 2 == 0 => c
      }.mkString
      answer must_== "パトカー"
    }

    "02.「パトカー」＋「タクシー」の文字を先頭から交互に連結して文字列「パタトクカシーー」を得よ．" >> {
      val answer = ("パトカー" zip "タクシー").map {
        case (a, b) => s"$a$b"
      }.mkString
      answer must_== "パタトクカシーー"
    }

    """03."Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
      |という文を単語に分解し，各単語の（アルファベットの）文字数を先頭から出現順に並べたリストを作成せよ．""".stripMargin >> {
      val answer = "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
        .split(" ").map(_.count(_.isLetter)).mkString
      answer must_== "314159265358979"
    }

    """04."Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
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

    """05.与えられたシーケンス（文字列やリストなど）からn-gramを作る関数を作成せよ．
      |この関数を用い，"I am an NLPer"という文から単語bi-gram，文字bi-gramを得よ．""".stripMargin >> {
      "文字bi-gram" >> {
        val answer = ngram(2)("I am an NLPer").map(f => f.toString).toVector
        answer must_== Vector("I ", " a", "am", "m ", " a", "an", "n ", " N", "NL", "LP", "Pe", "er")
      }
      "単語bi-gram" >> {
        val answer = ngram(2)("I am an NLPer".split(" ")).map(f => f.toVector).toVector
        answer must_== Vector(Vector("I", "am"), Vector("am", "an"), Vector("an", "NLPer"))
      }
    }

    """06."paraparaparadise"と"paragraph"に含まれる文字bi-gramの集合を，それぞれ, XとYとして求め，XとYの和集合，積集合，差集合を求めよ．
      |さらに，'se'というbi-gramがXおよびYに含まれるかどうかを調べよ．""".stripMargin >> {
      val x = ngram[Char](2)("paraparaparadise").toSet
      val y = ngram[Char](2)("paragraph").toSet
      "和集合" >> {
        val answer = (x union y).map(bi => bi.toString)
        answer must_== Set("ph", "gr", "se", "di", "ad", "ra", "ap", "ar", "is", "pa", "ag")
      }
      "積集合" >> {
        val answer = (x intersect y).map(bi => bi.toString)
        answer must_== Set("ra", "ap", "ar", "pa")
      }
      "差集合" >> {
        val answer = (x diff y).map(bi => bi.toString)
        answer must_== Set("se", "di", "ad", "is")
      }
      "contains se in X" >> {
        val answer = x.contains("se")
        answer must_== true
      }
      "contains se not in Y" >> {
        val answer = y.contains("se")
        answer must_== false
      }
    }

    """07.引数x, y, zを受け取り「x時のyはz」という文字列を返す関数を実装せよ．
      |さらに，x=12, y="気温", z=22.4として，実行結果を確認せよ．""".stripMargin >> {
      val answer = atXoclockYisZ(12)("気温")(22.4)
      answer must_== "12時の気温は22.4"
    }

    """08.暗号文
      |与えられた文字列の各文字を，以下の仕様で変換する関数cipherを実装せよ．
      |英小文字ならば(219 - 文字コード)の文字に置換
      |その他の文字はそのまま出力
      |この関数を用い，英語のメッセージを暗号化・復号化せよ．
    """.stripMargin >> {
      val question = "This is a test."
      val encode = cipher(question)
      val answer = cipher(encode)
      answer must_== question
    }
  }
}
