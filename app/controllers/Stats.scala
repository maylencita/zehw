package controllers

import models._
import org.joda.time._
import org.joda.time.format.DateTimeFormat

object Stats{

  val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

  def dateRange(from: DateTime, to: DateTime, step: ReadablePeriod): Iterator[DateTime] =
    Iterator.iterate(from)(_.plus(step)).takeWhile(! _.isAfter(to))

  def getCommitersRanking(indata: List[Commit]):Seq[(GitUser, Int)] = {
    val groupeddata = indata.groupBy(c => c.committer).map{case (k,lst) => k -> lst.size}
    groupeddata.toSeq.sortWith{
      case (c1, c2) => c1._2 > c2._2
    }.toSeq
  }

  def groupCommitsByDate(indata: Seq[Commit]):Seq[(DateTime,Int)] = {
    indata.groupBy(c => formatter.parseDateTime(c.date.split('T')(0)))
      .map{case (k,lst) => k -> lst.size}
      .toSeq.sortWith{
        case (c1, c2) => c2._1 isAfter c1._1
      }
  }

  /**
    Gets a sequence of projection points based on existing data,
    a projection point in the future (in days)
    and an algorithm.
  **/
  def getCommitsProjection(indata: Seq[Commit], factor: Double,
    algo:(Seq[(Int,Double)], Double) => Seq[(Int,Double)] = linearProjection): Seq[(DateTime, Double)] = {

    val groupeddata = groupCommitsByDate(indata)

    val fromDate = groupeddata.head._1
    val toDate = groupeddata.last._1


    val (dates, data) = (for{t <- dateRange(fromDate, toDate, Days.ONE)
      count = groupeddata.find(_._1 == t).map(_._2.toDouble).getOrElse(0.0)
    } yield t -> count).toSeq.unzip

    val newdata = algo(data.zipWithIndex.map(_.swap), factor)
    val days = (data.size * factor).toInt

    val newdates = dateRange(toDate.plus(Days.ONE), toDate.plus(Days.days(days)), Days.ONE)

    (for{i <- newdata.indices
      t = toDate.plus(Days.days(i+1))
    } yield t -> newdata(i)._2 )
  }

  def linearProjection(indata:Seq[(Int,Double)], factor: Double) = {

    val (xdata, ydata) = indata.unzip
    val xavg = if(indata.size > 0) xdata.sum / xdata.size else 0
    val yavg = if(indata.size > 0) ydata.sum / ydata.size else 0

    val cov = xdata.zip(ydata).foldLeft(0.0){
      (sum, xy) => sum + (xy._1 - xavg) * (xy._2 - yavg)
    }

    val varc = xdata.foldLeft(0.0){
      (sum, x) => sum + scala.math.pow(x - xavg, 2)
    }

    val b = if(varc != 0) cov / varc else 0
    val a = yavg - xavg * b
    val first = if(xdata.size == 0) 0 else xdata.last
    val last = first + (indata.size * factor).toInt

    for( i <- first to last) yield (i -> (a + b * i))
  }

}
