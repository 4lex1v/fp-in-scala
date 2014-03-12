package mongo

import scalaz.stream._
import scalaz.stream.Process.Await

object Streams {

  val lines = io.linesR("someFile.txt")

  val filtered = lines.filter(s => !s.trim.isEmpty && !s.startsWith("//"))

  val test: Sink[]

}
