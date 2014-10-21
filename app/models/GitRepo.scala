package object models{

  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  case class GitUser(
    login: String,
    id: Double,
    avatar_url: String,
    gravatar_id: Option[String],
    url: Option[String] //,
    //html_url: String,
    //followers_url: String,
    //following_url: String,
    //gists_url: String,
    //starred_url: String,
    //subscriptions_url: String,
    //organizations_url: String,
    //repos_url: String,
    //events_url: String,
    //received_events_url: String,
    //`type`: String,
    //site_admin: Boolean
  )

  case class GitRepo(
    id: Double,
    name: String,
    full_name: String,
    owner: GitUser,
    `private`: Boolean,
    html_url: String,
    description: String,
    url: String,
    git_url: String,
    size: Double,
    language: Option[String],
    has_issues: Boolean,
    has_downloads: Boolean,
    has_wiki: Boolean,
    has_pages: Boolean,
    forks_count: Double,
    forks: Double,
    score: Double
  )

  case class Commit(
    url: String,
    sha: String,
    committer: GitUser,
    date: String,
    message: String
  )

  case class Cmt(
    url: String,
    sha: String,
    committer: Option[GitUser],
    date: String,
    message: String
  ){
    val defaultCommiter = GitUser("N/A",0.0,"",None,None)

    def getCommit = Commit(url,sha,committer.getOrElse(defaultCommiter),date,message)
  }

  implicit val userFormat:Format[GitUser] = (
    (__ \ "login").format[String] ~
    (__ \ "id").format[Double] ~
    (__ \ "avatar_url").format[String] ~
    (__ \ "gravatar_id").format[Option[String]] ~
    (__ \ "url").format[Option[String]] //~
    //(__ \ "html_url").format[String] ~
    //(__ \ "followers_url").format[String] ~
    //(__ \ "following_url").format[String] ~
    //(__ \ "gists_url").format[String] ~
    //(__ \ "starred_url").format[String] ~
    //(__ \ "subscriptions_url").format[String] ~
    //(__ \ "organizations_url").format[String] ~
    //(__ \ "repos_url").format[String] ~
    //(__ \ "events_url").format[String] ~
    //(__ \ "received_events_url").format[String] ~
    //(__ \ "type").format[String] ~
    //(__ \ "site_admin").format[Boolean]
  )(GitUser.apply, unlift(GitUser.unapply))

  implicit val repoFormat:Format[GitRepo] = (
      (__ \ "id").format[Double] ~
      (__ \ "name").format[String] ~
      (__ \ "full_name").format[String] ~
      (__ \ "owner").format[GitUser] ~
      (__ \ "private").format[Boolean] ~
      (__ \ "html_url").format[String] ~
      (__ \ "description").format[String] ~
      (__ \ "url").format[String] ~
      (__ \ "git_url").format[String] ~
      (__ \ "size").format[Double] ~
      (__ \ "language").format[Option[String]] ~
      (__ \ "has_issues").format[Boolean] ~
      (__ \ "has_downloads").format[Boolean] ~
      (__ \ "has_wiki").format[Boolean] ~
      (__ \ "has_pages").format[Boolean] ~
      (__ \ "forks_count").format[Double] ~
      (__ \ "forks").format[Double] ~
      (__ \ "score").format[Double]
  )(GitRepo.apply, unlift(GitRepo.unapply))

  implicit val commitFormat:Format[Cmt] = (
    (__ \ "url").format[String] ~
    (__ \ "sha").format[String] ~
    (__ \ "committer").format[Option[GitUser]] ~
    (__ \ "commit" \ "committer" \ "date").format[String] ~
    (__ \ "commit" \ "message").format[String]
  )(Cmt.apply, unlift(Cmt.unapply))
 }
