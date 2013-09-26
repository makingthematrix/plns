package models

import play.api.libs.json.Writes
import play.api.libs.json.JsValue
import play.api.libs.json.Json

case class ConjugationTemplate(lang: String,mode: String,example: String,suffices:Map[logic.Conj.Value,String]){
  def get(id:String) = suffices.getOrElse(id,"")
}

object ConjugationTemplate {
  implicit val writes = new Writes[ConjugationTemplate] {
    def writes(ct: ConjugationTemplate): JsValue = 
      Json.obj("lang" -> ct.lang, "mode" -> ct.mode, "example" -> ct.example,
    		    "INF" -> ct.get("INF"), "PRES1S" -> ct.get("PRES1S"), "PRES2S" -> ct.get("PRES2S"), "PRES3S" -> ct.get("PRES3S"), 
    		    "PRES1P" -> ct.get("PRES1P"), "PRES2P" -> ct.get("PRES2P"), "PRES3P" -> ct.get("PRES3P"),
    		    "PAST1SM" -> ct.get("PAST1SM"), "PAST1SF" -> ct.get("PAST1SF"), "PAST2SM" -> ct.get("PAST2SM"), "PAST2SF" -> ct.get("PAST2SF"), 
    		    "PAST3SM" -> ct.get("PAST3SM"), "PAST3SF" -> ct.get("PAST3SF"), "PAST3SN" -> ct.get("PAST3SN"),
    		    "PAST1PM" -> ct.get("PAST1PM"), "PAST1PF" -> ct.get("PAST1PF"), "PAST2PM" -> ct.get("PAST2PM"), "PAST2PF" -> ct.get("PAST2PF"), 
    		    "PAST3PM" -> ct.get("PAST3PM"), "PAST3PF" -> ct.get("PAST3PF"),
    		    "IMP2S" -> ct.get("IMP2S"), "IMP1P" -> ct.get("IMP1P"), "IMP2P" -> ct.get("IMP2P"), 
    		    "ACTIVE" -> ct.get("ACTIVE"), "PASSIVE" -> ct.get("PASSIVE"), "NOUN" -> ct.get("NOUN"),
    		    "PERFECT" -> ct.get("PERFECT"))
  }
}
 