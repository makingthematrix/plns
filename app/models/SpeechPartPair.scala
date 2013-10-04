package models

import logic.SpeechPart
import logic.NSTranslator

abstract class SpeechPartPair[T <: SpeechPart[T]] {
  def pl: T;
  def ns: T;
	
  def add():Seq[(String,String)] = {
    NSTranslator.add(pl,ns);
    Seq((pl.mainRoot,ns.mainRoot))    
  }
}

object SpeechPartPair {
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