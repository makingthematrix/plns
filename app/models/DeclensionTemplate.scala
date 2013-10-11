package models

import play.api.libs.json.Writes
import play.api.libs.json.JsValue
import play.api.libs.json.Json

case class DeclensionTemplate(lang: String,mode: String,example: String,suffices:Map[logic.Decl.Value,String]){
  def get(id:String):String = suffices.getOrElse(id,"")
}

object DeclensionTemplate {
  implicit val writes = new Writes[DeclensionTemplate] {
    def writes(dt: DeclensionTemplate): JsValue = 
      Json.obj("lang" -> dt.lang, "mode" -> dt.mode, "example" -> dt.example,
    		  	"NOMS" -> dt.get("NOMS"),"GENS" -> dt.get("GENS"),"DATS" -> dt.get("DATS"),"ACCS" -> dt.get("ACCS"),
    		  	"VOCS" -> dt.get("VOCS"),"LOCS" -> dt.get("LOCS"),"INSS" -> dt.get("INSS"),
    		  	"NOMP" -> dt.get("NOMP"),"GENP" -> dt.get("GENP"),"DATP" -> dt.get("DATP"),"ACCP" -> dt.get("ACCP"),
    		  	"VOCP" -> dt.get("VOCP"),"LOCP" -> dt.get("LOCP"),"INSP" -> dt.get("INSP"))
  }
}