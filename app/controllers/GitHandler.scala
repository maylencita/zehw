package controllers

import models._

import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import play.api.data.validation.ValidationError
import play.api.libs.json.JsPath
import play.api.libs.iteratee._
import play.api.Configuration
import scala.concurrent.Future
import scala.util.{Try, Success, Failure}
import scala.concurrent.ExecutionContext.Implicits.global

object GitHandler{

  val giturlstring = "https://api.github.com" //TODO Put this in some config file
  val gitreposseach = WS.url(giturlstring + "/search/repositories")
  val authHeaders = current.configuration.getString("github.secret").map(
    t => Seq("Authorization" -> s"token $t")).getOrElse(Seq[(String,String)]())
  val headers = Seq("Accept" ->  "application/json") ++ authHeaders

  /**
    Calls the Github API operation for searching repositories
  */
  def searchRepositories(searchString:String):Future[Try[List[GitRepo]]] = {
    val holder = gitreposseach.withHeaders(headers:_*)
      .withRequestTimeout(10000)
      .withQueryString("q" -> searchString )

    holder.get().map{
      response =>
        (response.json \ "items").validate[List[GitRepo]].fold(
          errs => Failure(
            new Exception(
              errs.foldLeft("Invalid response:"){
                case (err, jserrs) => err +"\n"+ jserrs._1 + " -> " + jserrs._2.map(_.message + "\n")
              })
          ),
          list => Success(list)
        )
    }
  }

  /**
    Gets the list of contributors of a repository
  **/
  def getRepoCommitters(repoOwner:String, repoName:String):Future[Try[List[GitUser]]] = {
    val gitrepocommiters = s"/repos/${repoOwner}/${repoName}/collaborators"
    val holder = WS.url(giturlstring + gitrepocommiters)
      .withHeaders(headers:_*)
      .withRequestTimeout(10000)

    holder.get().map{
      response =>
        response.json.validate[List[GitUser]].fold(
          errs => genInvalidResponseFailure(errs),
          list => Success(list)
        )
    }
  }

  /**
  * Gets the last max commits from a repository
  **/
  def getRepoCommits(repoOwner:String,repoName:String,max:Int):Future[Try[List[Commit]]] = {
      val gitrepocommits = s"/repos/${repoOwner}/${repoName}/commits"

      // This method gets a list of commits from the given url.
      // It returns a Future contianing the result of the API call
      // and the eventual link for the next page of results.
      def getChunk(link:String):Future[(Option[String], Try[List[Commit]])] = {
        WS.url(link).withHeaders(headers:_*).get().map{
          response =>
            val next = response.header("Link").map(_.split(Array('<','>'))(1))
            println("next: " + next)

            response.json.validate[List[Cmt]].fold(
              errs => None -> genInvalidResponseFailure(errs),
              list => next -> Success(list.map(_.getCommit))
            )
        }
      }

      //Like Iteratee.consume, but ends when the concatenated list's size is "max" or bigger
      val consume = Iteratee.fold2[Try[List[Commit]], Try[List[Commit]]](Success(List[Commit]())){
        case (Success(list1), Success(list2)) =>
          var list = list1 ++ list2
          Future(Success(list) -> (list.size >= max))
        case (f@Failure(_), _) => scala.concurrent.Future(f -> true)
        case (_, f@Failure(_)) => scala.concurrent.Future(f -> true)
      }

      //Calls getChunk and sends a stop when link is None
      val generate:Enumerator[Try[List[Commit]]] =
        Enumerator.unfoldM[Option[String],Try[List[Commit]]](Some(giturlstring + gitrepocommits)){
          case Some(link) => getChunk(link).map(Some(_))
          case _ => Future(None)
      }

      (generate |>>> consume).map(_.map(_.take(max)))
  }



  /**
    Utility method for generating Failure state on Json errors
  **/
  private def genInvalidResponseFailure(errs:Seq[(JsPath, Seq[ValidationError])]) = Failure(
    new Exception(
      errs.foldLeft("Invalid response:"){
        case (err, jserrs) => err +"\n"+ jserrs._1 + " -> " + jserrs._2.map(_.args + "\n")
      })
    )
}
