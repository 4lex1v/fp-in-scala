package mongo

import scalaz.stream.mongodb.{ collectionSyntax => cs }
import com.mongodb.{BasicDBObject, DBCollection, MongoClient}
import scalaz.stream.Process
import scalaz.concurrent.Task
import scalaz.stream.mongodb.query.Query


object MongoTest extends App {

  import cs._

  val coll = new MongoClient().getDB("test").getCollection("names")

  val names: Process[Task, DBCollection] = use(coll)
  val q: Query = query("name" ≠ "Maria")

  def time[R](body: => R): R = {
    val start = System.nanoTime()
    val r = body
    val end = System.nanoTime() - start
    println(s"Time: $end")
    r
  }

  println(time((coll through query("name" === "Maria")).runLog.run))

  println(time(coll.findOne(new BasicDBObject("name", "Maria"))))

  println(time((coll through query("name" === "Maria")).runLog.run))

  println(time(coll.findOne(new BasicDBObject("name", "Maria"))))

  println(time((coll through query("name" === "Maria")).runLog.run))

  println(time(coll.findOne(new BasicDBObject("name", "Maria"))))

  println(time((coll through query("name" === "Maria")).runLog.run))

  println(time(coll.findOne(new BasicDBObject("name", "Maria"))))

  println(time((coll through query("name" === "Maria")).runLog.run))

  println(time(coll.findOne(new BasicDBObject("name", "Maria"))))

  println(time((coll through query("name" === "Maria")).runLog.run))

  println(time(coll.findOne(new BasicDBObject("name", "Maria"))))

  println(time((coll through query("name" === "Maria")).runLog.run))

  println(time(coll.findOne(new BasicDBObject("name", "Maria"))))

  println(time((coll through query("name" === "Maria")).runLog.run))

  println(time(coll.findOne(new BasicDBObject("name", "Maria"))))

  println(time((coll through query("name" === "Maria")).runLog.run))

  println(time(coll.findOne(new BasicDBObject("name", "Maria"))))

  println(time((coll through query("name" === "Maria")).runLog.run))

  println(time(coll.findOne(new BasicDBObject("name", "Maria"))))

}
