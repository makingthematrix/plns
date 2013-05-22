package models

import logic.Verb
import logic.PLVerb
import logic.NSVerb
import logic.VerbException
            
case class VerbPair(plInfRoot: String, plImpRoot: String, plPattern: String, plExceptions: Option[String],
                    nsInfRoot: String, nsImpRoot: String, nsPattern: String, nsExceptions: Option[String])
  extends SpeechPartPair[Verb] {
  
  def pl:Verb = {
    val word = PLVerb.word(plInfRoot, plImpRoot, plPattern)
	plExceptions match {
      case Some(str) => VerbPair.parse(str).foreach(word.except)
      case None => 
	}
	return word
  }
  
  def ns:Verb = {
    val word = NSVerb.word(nsInfRoot, nsImpRoot, nsPattern)
	nsExceptions match {
      case Some(str) => VerbPair.parse(str).foreach(word.except)
      case None => 
	}
	return word    
  }

}

object VerbPair {
  // exceptions should be in the format "case1:word1,case2:word2,..."
  def parse(exceptions: String):Seq[VerbException] = {
    val log = play.Logger.of("application")
    log.info("parsing exceptions!: " + exceptions)
    exceptions.split(",").map(str => {
      log.debug("trying to parse exception: " + str)
      println(str)
      val t = str.split(":");
      val key = t(0)
      val word = t(1)
      new VerbException(key,word);
    });
  }
}