/**
  * Created by t4ai6i on 2016/10/27.
  */

import com.typesafe.scalalogging.LazyLogging
import org.specs2.mutable._

import scala.util.Random

class NLPSection1Spec extends Specification with LazyLogging {

  def ngram[A](n: Int, xs: Iterable[A]) = xs.sliding(n)

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
      val expected = "パトカー"
      "#zipWithIndex" >> {
        val answer = "パタトクカシーー".zipWithIndex.collect {
          case (c, i) if i % 2 == 0 => c
        }.mkString
        answer must_== expected
      }

      "#foldLeft" >> {
        val answer = "パタトクカシーー".foldLeft((true, "")) {
          case ((true, str), c) => (false, str + c)
          case ((false, str), _) => (true, str)
        }._2
        answer must_== expected
      }

      "Tail Recursive" >> {
        def funcA(str: String): String = {
          def recursive(str: String, flag: Boolean, acc: String): String = (str.isEmpty, flag) match {
            case (true, _) => acc
            case (_, true) => recursive(str.tail, false, acc + str.head)
            case (_, false) => recursive(str.tail, true, acc)
          }

          recursive(str, true, "")
        }

        val answer = funcA("パタトクカシーー")
        answer must_== expected
      }
    }

    "02.「パトカー」＋「タクシー」の文字を先頭から交互に連結して文字列「パタトクカシーー」を得よ．" >> {
      val expected = "パタトクカシーー"
      "#zip" >> {
        val answer = ("パトカー" zip "タクシー").map {
          case (a, b) => s"$a$b"
        }.mkString
        answer must_== expected
      }

      "#foldLeft" >> {
        val answer = "パトカー".foldLeft(("", "タクシー")) { case ((acc, ys), c) =>
          (acc + c + ys.head, ys.tail)
        }._1.mkString
        answer must_== expected
      }

      "Tail Recursive" >> {
        def funcA(xs: Seq[Char], ys: Seq[Char]): String = {
          def recursive(us: Seq[Char], vs: Seq[Char], acc: String): String = {
            (us.isEmpty, vs.isEmpty) match {
              case (true, true) => acc
              case (true, false) => acc + vs.mkString
              case (false, true) => acc + us.mkString
              case _ => recursive(us.tail, vs.tail, acc + us.head + vs.head)
            }
          }

          recursive(xs, ys, "")
        }

        val answer = funcA("パトカー", "タクシー")
        answer must_== expected
      }
    }

    """03."Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
      |という文を単語に分解し，各単語の（アルファベットの）文字数を先頭から出現順に並べたリストを作成せよ．""".stripMargin >> {
      val expected = Seq(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9)
      val string = "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."

      "#map" >> {
        val answer = string.split("""\W+""").map(_.length).toSeq
        answer must_== expected
      }

      "for expression" >> {
        val answer = (for {
          xs <- string.split("""\W+""")
        } yield {
          xs.foldLeft(0) { case (acc, _) => acc + 1 }
        }).toSeq
        answer must_== expected
      }
    }

    """04."Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
      |という文を単語に分解し，1, 5, 6, 7, 8, 9, 15, 16, 19番目の単語は先頭の1文字，それ以外の単語は先頭に2文字を取り出し，
      |取り出した文字列から単語の位置（先頭から何番目の単語か）への連想配列（辞書型）を作成せよ．""".stripMargin >> {
      val expected = Map(16 -> "Cl", 6 -> "N", 3 -> "Be", 8 -> "F", 11 -> "Mi", 10 -> "Na", 9 -> "Ne", 17 -> "Ar", 4 -> "B", 2 -> "Li", 14 -> "P", 1 -> "He", 13 -> "Si", 5 -> "C", 0 -> "H", 19 -> "Ca", 18 -> "K", 12 -> "Al", 7 -> "O", 15 -> "S")
      val string = "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
      val headOne = Seq(1, 5, 6, 7, 8, 9, 15, 16, 19).map(_ - 1)

      def containInHeadOne(number: Int): Int = number match {
        case n if headOne.contains(n) => 1
        case _ => 2
      }

      "#zipWithIndex" >> {
        val answer = string.split("""\W+""").zipWithIndex.map {
          case (w, i) => i -> w.take(containInHeadOne(i))
        }.toMap
        answer must_== expected
      }
      "#foldLeft" >> {
        val (_, answer) = string.split("""\W+""").foldLeft((0, Map.empty[Int, String])) {
          case ((current, map), word) =>
            (current + 1, map.updated(current, word.take(containInHeadOne(current))))
        }
        answer must_== expected
      }
      "Regex" >> {
        var n: Int = 0
        val poss = "^(1|5|6|7|8|9|15|16|19)$".r
        val answer = string.split("""\W+""").map { word =>
          n += 1
          n.toString match {
            case poss(_) => (n - 1, word.take(1))
            case _ => (n - 1, word.take(2))
          }
        }.toMap
        answer must_== expected
      }
    }

    """05.与えられたシーケンス（文字列やリストなど）からn-gramを作る関数を作成せよ．
      |この関数を用い，"I am an NLPer"という文から単語bi-gram，文字bi-gramを得よ．""".stripMargin >> {

      "@kikou5656.tail recursive" >> {
        def ngram(s: Seq[String], n: Int): Seq[String] = {
          def fn(ss: Seq[Seq[String]], acc: Vector[String]): Vector[String] = {
            if (ss.exists(_.isEmpty)) acc
            else {
              val t: String = ss.map(_.head).mkString
              fn(ss.map(_.drop(1)), acc :+ t)
            }
          }

          // 1要素ずつずらしたものを作成
          val a = for {
            i <- 1 to n
          } yield s.drop(i - 1)

          fn(a, Vector())
        }

        val a = ngram("hoge".toCharArray.map(_.toString).toSeq, 2)
        a must_== Seq("ho", "og", "ge")

        val b = ngram("hoge piyo huga".split(" "), 2)
        b must_== Seq("hogepiyo", "piyohuga")
      }

      "@macorains.tail recursive" >> {
        val s = "I am an NLPer"

        def getCharNgram1(n: Int, src: String, acc: List[String]): List[String] = {
          src match {
            case s: String if s.length > n - 1 => {
              getCharNgram1(n, src.drop(1), acc :+ src.take(n))
            }
            case _ => acc
          }
        }

        val answer = getCharNgram1(2, s, List.empty[String])
        val expected = List("I ", " a", "am", "m ", " a", "an", "n ", " N", "NL", "LP", "Pe", "er")
        answer must_== expected
      }

      "文字bi-gram" >> {
        val expected = Seq("I ", " a", "am", "m ", " a", "an", "n ", " N", "NL", "LP", "Pe", "er")
        val answer = ngram(2, "I am an NLPer").map(_.toString).toSeq
        Seq(answer) must contain(===(expected))
      }
      "単語bi-gram" >> {
        val expected = Seq(Seq("I", "am"), Seq("am", "an"), Seq("an", "NLPer"))
        val answer = ngram(2, "I am an NLPer".split(" ")).map(_.toSeq).toSeq
        Seq(answer) must contain(===(expected))
      }
    }

    """06."paraparaparadise"と"paragraph"に含まれる文字bi-gramの集合を，それぞれ, XとYとして求め，XとYの和集合，積集合，差集合を求めよ．
      |さらに，'se'というbi-gramがXおよびYに含まれるかどうかを調べよ．""".stripMargin >> {
      val x = ngram(2, "paraparaparadise").map(_.toString).toSet
      val y = ngram(2, "paragraph").map(_.toString).toSet
      "和集合" >> {
        val expected = Set("ph", "gr", "se", "di", "ad", "ra", "ap", "ar", "is", "pa", "ag")
        val answer = (x union y).map(bi => bi.toString)
        answer must_== expected
      }
      "積集合" >> {
        val expected = Set("ra", "ap", "ar", "pa")
        val answer = (x intersect y).map(bi => bi.toString)
        answer must_== expected
      }
      "差集合" >> {
        val expected = Set("se", "di", "ad", "is")
        val answer = (x diff y).map(bi => bi.toString)
        answer must_== expected
      }
      "排他的論理和" >> {
        val expected = Set("se", "di", "ad", "is", "ph", "gr", "ag")
        val answer = ((x union y) diff (x intersect y)).map(bi => bi.toString)
        answer must_== expected
      }
      "contains se in X" >> {
        x must contain("se")
      }
      "contains se not in Y" >> {
        y must not contain("se")
      }
    }

    """07.引数x, y, zを受け取り「x時のyはz」という文字列を返す関数を実装せよ．
      |さらに，x=12, y="気温", z=22.4として，実行結果を確認せよ．""".stripMargin >> {
      def atXoclockYisZ(x: Int)(y: String)(z: Double) = s"${x}時の${y}は${z}"

      "#1" >> {
        val expected = "12時の気温は22.4"
        val answer = atXoclockYisZ(12)("気温")(22.4)
        answer must_== expected
      }

      "#2" >> {
        val expected = "12時の気温は22.4"
        val answer = atXoclockYisZ(_: Int)("気温")(22.4)
        answer(12) must_== expected
      }
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

    """09.スペースで区切られた単語列に対して，各単語の先頭と末尾の文字は残し，それ以外の文字の順序をランダムに並び替えるプログラムを作成せよ．
      |ただし，長さが４以下の単語は並び替えないこととする．適当な英語の文（例えば
      |"I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind ."）
      |を与え，その実行結果を確認せよ．
    """.stripMargin >> {

      val string = "I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind ."

      def typoglycemia(word: String): String = {
        val head = word.head
        val middle = Random.shuffle(word.substring(1, word.length - 1).toSeq).mkString
        val last = word.last
        head +: middle :+ last
      }

      "#map" >> {
        val answer = string.split("""\s+""").map {
          case w if w.length > 4 => typoglycemia(w)
          case w => w
        }.mkString(" ")

        logger.debug(answer)
        true must_== true
      }

      "for" >> {
        def condition(word: String, num: Int): String = word match {
          case w if w.length > num => typoglycemia(w)
          case w => w
        }
        val answer = (for {
          word <- string.split("""\s+""")
        } yield {
          condition(word, 4)
        }).mkString(" ")

        logger.debug(answer)
        true must_== true
      }
    }
  }
}
