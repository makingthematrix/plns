package models

import logic.Adjective
import logic.PLAdjective
import logic.NSAdjective
import logic.AdjectiveCase

case class AdjectivePair(plInd:String,plAdvInd:String,plCmp:String,plAdvCmp:String,
                         plMode:String,plAdvMode:String,plExceptions:Option[String],
                         nsInd:String,nsAdvInd:String,nsCmp:String,nsAdvCmp:String,
                         nsExceptions:Option[String],cmpIgnored:String) 
  extends SpeechPartPair[Adjective]{
  def this(plInd:String,plCmp:String,plMode:String,plExceptions:Option[String],nsInd:String,nsCmp:String,nsExceptions:Option[String]) =
    this(plInd,plInd,plCmp,plCmp,plMode,plMode,plExceptions,nsInd,nsInd,nsCmp,nsCmp,nsExceptions,"false")
  def this(plInd:String,plMode:String,plExceptions:Option[String],nsInd:String,nsExceptions:Option[String]) =
    this(plInd,plInd,plInd,plInd,plMode,plMode,plExceptions,nsInd,nsInd,nsInd,nsInd,nsExceptions,"true")
  
  def pl: Adjective = {	
    val word = PLAdjective.word(plInd, plCmp, plAdvInd, plAdvCmp, plMode, plAdvMode, cmpIgnored.equals("true"))
    plExceptions match {
      case Some(str) => SpeechPartPair.parseExceptions(str).foreach(ae => word.except(AdjectiveCase.parse(ae._1), ae._2))
      case None => 
    } 
    word
  }

  def ns: Adjective = {
    val word = NSAdjective.word(nsInd,nsCmp,nsAdvInd,nsAdvCmp,cmpIgnored.equals("true"))
    nsExceptions match {
      case Some(str) => SpeechPartPair.parseExceptions(str).foreach( ae => word.except(ae._1, ae._2) )
      case None => 
    }
    word
  }
  
  //implicit val writes = new Writes[AdjectiveException] {
  //  def writes(ae: AdjectiveException): JsValue = 
  //    Json.obj( "gender" -> ae.gender, "degree" -> ae.degree, "declCase" -> ae.declCase.toString(), "word" -> ae.word );
  //}
  
}