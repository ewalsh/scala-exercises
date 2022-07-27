package ai.economicdatasciences.examples

import java.time.Instant
import java.time.Duration
import java.time.temporal.ChronoUnit

import scala.collection.immutable.Queue

sealed trait Food {
  val food_id = 1
  val make_time = 60
  val serve_time = 30
}
case object Sandwich extends Food {
  override val food_id = 1
  override val make_time = 60
  override val serve_time = 30
}
// case object JacketPotato extends Food
//
// sealed abstract class PrepTime(val start: Instant, val timeAdd: Int, val unit: ChronoUnit) {
//   val finishedPrep = start.plus(timeAdd, unit)
// }
// case object MakeSandwith extends PrepTime(val start: Instant, val timeAdd: Int, val unit: ChronoUnit)

case class Order(
  id : Int,
  items: Queue[Food],
  created_at: Instant,
  estimated: Instant,
  accepted: Boolean
)

case class PrepAction(
  order_id: Int,
  prep_id: Int,
  action: String,
  created_at: Instant
)

class OrderQueue {
  private var orders = Queue[Order]()
  private var prepActions = Queue[PrepAction]()
  private var orderId: Int = 0
  private var sandwichInventory: Int = 45

  def genPrepTotalEstimate(start: Instant, items: Queue[Food]): Instant = {
    start.plus(items.length * 90, ChronoUnit.SECONDS)
  }

  def checkEstimate(start: Instant, estimated: Instant): Boolean = {
    val diff = Duration.between(start, estimated)
    diff.toSeconds < Duration.ofMinutes(5).toSeconds
  }

  def genPrepActions(start: Instant, items: Queue[Food], order_id: Int): Queue[PrepAction] = {
    var tmpActions = Queue[PrepAction]()
    var counter = 0
    var timeCounter = start
    for(i <- Range(0, items.length)){
      tmpActions = tmpActions ++ Queue(
        PrepAction(order_id, counter, s"make ${items(i).toString} ${i + 1}", timeCounter.plus(items(i).make_time, ChronoUnit.SECONDS)),
        PrepAction(order_id, counter + 1, s"serve ${items(i).toString} ${i + 1}", timeCounter.plus(items(i).make_time + items(i).serve_time, ChronoUnit.SECONDS))
      )
      counter = counter + 2
      timeCounter = timeCounter.plus(items(i).make_time + items(i).serve_time, ChronoUnit.SECONDS)
    }
    tmpActions.drop(1)
  }

  def recieveOrder(items: Queue[Food]): (Queue[Order], Queue[PrepAction]) = {
    // initialize order
    val orderTime = Instant.now()
    val totalTime = genPrepTotalEstimate(orderTime, items)
    val check = checkEstimate(orderTime, totalTime)
    var prepId: Int = 1
    orderId = orderId + 1
    orders = orders :+ Order(orderId, items, orderTime, totalTime, check)
    check match {
      case false => {
        // val foodGroups = items.groupBy(_.food_id)
        prepActions = prepActions :+ PrepAction(
          orderId,
          prepId,
          s"${items.length} sandwich orders placed, this breaches time estimate",
          orderTime
        )
      }
      case true => {
        sandwichInventory = sandwichInventory - items.length
        prepActions = prepActions :+ PrepAction(
          orderId,
          prepId,
          s"${items.length} sandwich orders placed, started making sandwich 1",
          orderTime
        )
        val restOfPrepQueue = genPrepActions(orderTime, items, orderId)
        prepActions = prepActions ++ restOfPrepQueue
      }
    }

    (orders, prepActions)
  }

}

object OrderQueueApp {
  def main(args: Array[String]): Unit = {
    val inputOrders = Queue(Sandwich, Sandwich, Sandwich)
    val oq = new OrderQueue
    val (ords, preps) = oq.recieveOrder(inputOrders)
    preps.map(x => {
      println(s"${x.created_at} ${x.action}")
    })
  }

}
