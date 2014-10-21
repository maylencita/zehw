import views.html.helper.FieldConstructor
import views.html._

package forms.ghelpers{
  object navHelpers {
    implicit val myfields = FieldConstructor(navTemplate.f)
  }
}
