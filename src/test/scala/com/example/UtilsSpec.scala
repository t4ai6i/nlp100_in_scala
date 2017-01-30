/**
  * Created by tomoya.igarashi on 2017/01/29.
  */
package com.example

import com.typesafe.scalalogging.LazyLogging

import java.io.File

import org.specs2.mutable._

import Utils._

class UtilsSpec extends Specification with LazyLogging {

  private val filePath = "src/test/resources/hightemp.txt"
  private val file = new File(filePath)
  private val outFilePath = "src/test/resources/out.txt"
  private val outFile = new File(outFilePath)

  "com.example.Utils" >> {
    "#write" >> {
      open(file) { ite =>
        write(outFile, ite) { (line, bw) =>
          bw.write(line)
          bw.newLine()
        }
      }
      val answer = false
      answer must_== false
    }
  }
}
