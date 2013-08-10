package models

import logic.SpeechPart
import logic.NSTranslator

trait SpeechPartPair[T <: SpeechPart[T]] {
  def pl: T;
  def ns: T;
	
  def add():Seq[(String,String)] = {
    NSTranslator.add(pl,ns);
    Seq((pl.mainRoot,ns.mainRoot))    
  }
}