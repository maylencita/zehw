package controllers

import forms.AppForms._
import models._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.util.{Success,Failure}

import scala.concurrent.ExecutionContext.Implicits.global

object Application extends Controller {

  def index = Action {
    Ok(views.html.index(searchForm))
  }

  def apiSearch(query:String) = Action.async{
    //import models.
    GitHandler.searchRepositories(query).map(
      _ match{
        case Success(ok) => Ok(Json.toJson(ok))
        case Failure(err) =>
           Logger.error(err.toString)
           InternalServerError("Oops. Something went wrong. Plase try again later")
      }
    )
  }

  def apiCommitters(repoOwner:String,repoName:String,maxCommits:Integer) = Action.async{

      implicit val twrites: Writes[(GitUser, Int)] = (
          (JsPath \ "user").write[GitUser] and
          (JsPath \ "commits").write[Int]
      )(p => p._1 -> p._2)

      GitHandler.getRepoCommits(repoOwner, repoName, maxCommits).map(
        _ match {
          case Success(commits) =>
            val rankin = Stats.getCommitersRanking(commits)
            Ok(Json.toJson(rankin.toList))
          case _ =>
            InternalServerError("Failed to load results. Please try again later.")
        }
      )
  }

  def apiForecast(repoOwner:String,repoName:String,maxCommits:Integer,method:String) = Action.async{
    import Stats._

    val forecastMethod = forcastMethods.getOrElse(method, linearProjection _)

    GitHandler.getRepoCommits(repoOwner,repoName,maxCommits).map{
      case Success(commits) => Ok(Json.toJson(commits))
      case Failure(err) =>
        Logger.error(err.toString)
        InternalServerError("Failed to load results. Please try again later.")
    }
  }

  def searchRepository = Action.async{
    implicit request =>
      searchForm.bindFromRequest.fold(
	       error => scala.concurrent.Future(BadRequest(views.html.index(searchForm))),
	       searchText =>
           GitHandler.searchRepositories(searchText).map(
             _ match{
               case Success(ok) => Ok(views.html.searchResult(ok))
               case Failure(err) =>
                  Logger.error(err.toString)
                  InternalServerError(views.html.index(searchForm))
             }
           )
      )
  }

  def repoDetails(repoOwner:String,repoName:String,maxCommits:Integer,method:String) = Action.async{
    import Stats._

    val forecastMethod = forcastMethods.getOrElse(method, linearProjection _)

    (GitHandler.getRepoCommitters(repoOwner, repoName) zip GitHandler.getRepoCommits(repoOwner,repoName,maxCommits)).map(
      _ match {
        case (Success(committers),Success(commits)) =>
          Ok(views.html.repodetails(repoOwner, repoName, maxCommits,
            getCommitersRanking(commits),
            groupCommitsByDate(commits),
            getCommitsProjection(commits, 0.1, forecastMethod)))
        case (Failure(err1),Failure(err2)) =>
          Logger.error("Failed to load page:" + err1 + " and " + err2)
          InternalServerError(views.html.index(searchForm))
        case (Failure(err),Success(commits)) =>
          Logger.error("Failed to load committers: " + err)
          InternalServerError(views.html.index(searchForm))
        case (Success(committers),Failure(err)) =>
          Logger.error("Failed to load commits: " + err)
          InternalServerError(views.html.index(searchForm))
      }
    )
  }
}
