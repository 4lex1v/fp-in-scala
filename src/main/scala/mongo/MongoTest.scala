package mongo

import scalaz.stream.mongodb.{ collectionSyntax => cs }
import com.mongodb.{DBObject, BasicDBObject, DBCollection, MongoClient}
import scalaz.stream.{process1, Process}
import scalaz.concurrent.Task
import scalaz.stream.mongodb.query.Query
import scalaz.stream.mongodb.update.InsertWriteResult
import scalaz.stream.Process.Process1


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

  import scalaz.syntax.bind._

  val dbo = new BasicDBObject("name", "Alex")

  def action(dbo: DBObject) = for {
    InsertWriteResult(_, _, _, doc) <- coll >>> insert(dbo)
  } yield doc

  def entity[A: JsonFormat] = process1.lift[DBObject, A] { dbo =>

  }

  action(dbo) pipe entity[String]

}




  private val process: Process[Task, DBObject] = coll through query("name" === "Maria")
  coll >>> query("name" === "Maria")

  println(time(process.runLog.run))

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
