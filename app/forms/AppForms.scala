package forms

import play.api.data.Form
import play.api.data.Forms._
import java.util.{UUID,Date}
import play.api.i18n.Messages

object AppForms{

    val searchForm = Form("searchText" -> text)

    val forecastForm = Form("forcast-method" -> text)
}
