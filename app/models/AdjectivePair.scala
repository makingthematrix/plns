package models

import logic.Adjective
import logic.PLAdjective
import logic.NSAdjective
import logic.AdjectiveException
import logic.Decl
import logic.PLMode._
import play.api.libs.json.Writes
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import logic.SpeechPart
import play.api.Logger

case class AdjectivePair(plInd:String,plAdvInd:Option[String],plCmp:Option[String],plAdvCmp:Option[String],
                         plMode:String,plAdvMode:String,plExceptions:Option[String],
                         nsInd:String,nsAdvInd:Option[String],nsCmp:Option[String],nsAdvCmp:Option[String],
                         nsExceptions:Option[String],cmpIgnored:Option[String]) 
  extends SpeechPartPair[Adjective]{
  
  def pl: Adjective = {	
	val cmp = plCmp match {
	  case None => plInd
	  case Some(str) => str
	}
    val adjInd = plAdvInd match {
      case None => plInd
      case Some(str) => str
    }
    val adjCmp = plAdvCmp match {
      case None => adjInd
      case Some(str) => str
    }
    
    val ignored = cmpIgnored.getOrElse("").equals("on")
    val word = PLAdjective.word(plInd, cmp, adjInd, adjCmp, plMode, plAdvMode, ignored)
    
    plExceptions match {
      case Some(str) => AdjectivePair.parse(str).foreach(word.except);
      case None => 
    } 
    return word;
  }

  def ns: Adjective = {
	val cmp = nsCmp match {
	  case None => nsInd
	  case Some(str) => str
	}
    val advInd = nsAdvInd match {
      case None => nsInd
      case Some(str) => str
    }
    val advCmp = nsAdvCmp match {
      case None => advInd
      case Some(str) => str
    }
    
    val ignored = cmpIgnored.getOrElse("").equals("on")
    val word = NSAdjective.word(nsInd,cmp,advInd,advCmp,ignored);
    
    plExceptions match {
      case Some(str) => AdjectivePair.parse(str).foreach(word.except);
      case None => 
    }
    return word;
  }
  
  implicit val writes = new Writes[AdjectiveException] {
    def writes(ae: AdjectiveException): JsValue = 
      Json.obj( "gender" -> ae.gender, "degree" -> ae.degree, "declCase" -> ae.declCase.toString(), "word" -> ae.word );
  }
  
}

object AdjectivePair{
  // exceptions should be in the format "gender1:degree1:case1:word1,gender2:degree2:case2:word2,..."
  def parse(exceptions: String):Array[AdjectiveException] = {
    val log = play.Logger.of("application")
    log.info("parsing exceptions!: " + exceptions)
    exceptions.split(",").map(str => {
      log.debug("trying to parse exception: " + str)
      println(str)
      val t = str.split(":");
      val key = t(0)
      val gender = key.substring(0, 1)
      val degree = key.substring(1, 2)
      val declCase: Decl.Value = key.substring(2)
      val word = t(1)
      new AdjectiveException(gender,degree,declCase,word);
    });
  }
  
  def main(args: Array[String]): Unit = {
    val exs = parse("piNOMP:lekcy,pcNOMP:lżejsi,")
    exs.foreach(println)
    
    val pair = new AdjectivePair("lekk",None,Option("lżej"),Option("lż"),"SOFT","HARD",Option("piNOMP:lekcy,pcNOMP:lżejsi"),
                                 "legk",None,None,None,None,None)
    val pl = pair.pl
    println(pl)
    pl.exceptions.foreach(println)
    
    val ns = pair.ns
    println(ns)
    ns.exceptions.foreach(println)
    
    pl.translateTo(ns)
  }
}