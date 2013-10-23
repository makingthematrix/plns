package models

import logic.SpeechPart
import logic.NSTranslator
import logic.DictionaryFactory
import logic.Contentized

abstract class SpeechPartPair[T <: SpeechPart[T]](val id: Long, val speechPart: String) extends Contentized {
  val fromLang = "pl"
  val toLang = "ns" // and let's leave it like this for some time
    
  def this(speechPart: String) = this(-1L, speechPart)
  def copy = copyWithId(id)
  def copyWithId(id: Long): SpeechPartPair[T]

  def pl: T
  def ns: T
	
  def add() = {
    val id = DictionaryFactory.dict.addPair(this)
    val translations = pl.generate(ns, id)
    translations.foreach{ entry => DictionaryFactory.dict.add(entry) }
    Seq((pl.mainRoot,ns.mainRoot))
  }
  
  def addExceptions(word: SpeechPart[T], exceptions: Option[String]) = exceptions match {
    case Some(str) => SpeechPartPair.parseExceptions(str).foreach( ex => word.except(ex._1, ex._2) )
    case None => 
  } 
  
}

object SpeechPartPair {
  val noId = -1L;
  /**
   * split the exceptions string into a sequence of (key,value). Add a prefix to each if necessary
   * @param exceptions should be in the format "case1:word1,case2:word2,..."
   * @param prefix a prefix which should be fixed to each generated VerbException
   */
    
  def parseExceptions(exceptions: String,prefix:String):Seq[(String,String)] = {
    exceptions.split(",").map(str => {
      val t = str.split(":");
      val key = t(0)
      val word = t(1)
      (key,prefix+word);
    });
  }
  
  def parseExceptions(exceptions: String):Seq[(String,String)] = parseExceptions(exceptions,"")
}