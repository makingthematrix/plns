package models

import logic.Noun
import logic.PLNoun
import logic.NSNoun
import logic.NounException

case class NounPair(plRoot: String,plPattern: String,plExceptions: Option[String], plIgnored: String,
                    nsRoot: String,nsPattern: String,nsExceptions: Option[String], nsIgnored: String) 
    extends SpeechPartPair[Noun] {
	def pl: Noun = {
	  println("plIgnored: " + plIgnored)
	  val ignored = plIgnored match {
	    case "none" => (false,false)
	    case "singular" => (true,false)
	    case "plural" => (false,true)
	    case _ => throw new IllegalArgumentException("NounPair.pl, invalid 'ignored' value: " + plIgnored)
	  }
	  val word = PLNoun.word(plRoot, plPattern, ignored._1, ignored._2)
	  plExceptions match {
      	case Some(str) => NounPair.parse(str).foreach(word.except)
      	case None => 
	  }
	  return word
	}
	
	def ns: Noun = {
	  val ignored = nsIgnored match {
	    case "none" => (false,false)
	    case "singular" => (true,false)
	    case "plural" => (false,true)
	    case _ => throw new IllegalArgumentException("NounPair.ns, invalid 'ignored' value: " + nsIgnored)
	  }
	  val word = NSNoun.word(nsRoot, nsPattern, ignored._1, ignored._2)
	  nsExceptions match {
      	case Some(str) => NounPair.parse(str).foreach(word.except)
      	case None => 
	  }
	  return word
	}

}

object NounPair{
  // exceptions should be in the format "case1:word1,case2:word2,..."
  def parse(exceptions: String):Seq[NounException] = {
    val log = play.Logger.of("application")
    log.info("parsing exceptions!: " + exceptions)
    exceptions.split(",").map(str => {
      log.debug("trying to parse exception: " + str)
      println(str)
      val t = str.split(":");
      val key = t(0)
      val word = t(1)
      new NounException(key,word);
    });
  }
  
  val ignoredOptions = Map("none" -> "nie ignoruj", "singular" -> "pojedynczej", "plural" -> "mnogiej")
}