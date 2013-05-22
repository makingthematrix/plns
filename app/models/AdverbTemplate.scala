package models

import play.api.libs.json._

case class AdverbTemplate(lang: String, mode: String, ind: String, cmp: String);

object AdverbTemplate {
  implicit val writes = new Writes[AdverbTemplate] {
    def writes(template: AdverbTemplate): JsValue = 
      Json.obj( "lang" -> template.lang, "mode" -> template.mode, "ind" -> template.ind, "cmp" -> template.cmp);
  }
}