package com.example

import org.apache.commons.io.{FileUtils, LineIterator}

import java.io.File
import java.nio.charset.{Charset, StandardCharsets}

import scala.collection.JavaConverters._
import scala.util.control.Exception._

/**
  * Created by tomoya.igarashi.0510@gmail.com on 2017/01/22.
  */
object Utils {

  def open[X](file: File, charset: Charset = StandardCharsets.UTF_8)(body: Iterator[String] => X) = {
    val lineIterator = FileUtils.lineIterator(file, charset.toString)
    allCatch withApply { t =>
      throw t
    } andFinally {
      LineIterator.closeQuietly(lineIterator)
    } apply {
      body(lineIterator.asScala.map(_.toString))
    }
  }

}
