package models

import play.api.libs.json._

case class AdjectiveTemplate(lang: String, mode: String, cmp: String,
                             m: DeclensionTemplate, f: DeclensionTemplate, n: DeclensionTemplate, 
                             p: DeclensionTemplate, P: DeclensionTemplate);

object AdjectiveTemplate {
  implicit val writes = new Writes[AdjectiveTemplate] {
    def writes(template: AdjectiveTemplate): JsValue = {
      Json.obj( "lang" -> template.lang, "mode" -> template.mode,
                "cmp" -> template.cmp,
                "m" -> template.m, "f" -> template.f, "n" -> template.n,
                "p" -> template.p, "P" -> template.P
      );
    }
  }
  

}