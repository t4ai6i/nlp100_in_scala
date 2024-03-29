/**
  * Created by t4ai6i on 2017/01/29.
  */
package com.example

import com.typesafe.scalalogging.LazyLogging
import java.io._

import org.specs2.mutable._
import util.Properties
import Utils._

class UtilsSpec extends Specification with LazyLogging {

  private val filePath = "src/test/resources/hightemp.txt"
  private val file = new File(filePath)

  "com.example.Utils" >> {
    "#open & #write" >> {
      val stringWriter = new StringWriter()
      val bufferedWriter = new BufferedWriter(stringWriter)
      write(file, bufferedWriter) { (line, bw) =>
        bw.write(line)
        bw.newLine()
      }
      val lines = stringWriter.toString
      val answer = lines.split(Properties.lineSeparator).head
      answer must_== "埼玉県\t熊谷\t40.9\t2007-08-16"
    }
  }
}
