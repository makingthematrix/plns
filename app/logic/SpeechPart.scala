package logic

import scala.collection.mutable

abstract class SpeechPart[T <: SpeechPart[T]] {
  def translateTo(speechPart: T,rootId1: Long,rootId2: Long)
  def mainRoot: String
  val speechPart: String
  val lang: String
  
  def toRoot():Root
	
  def addRoots(t: T):(Long,Long) = NSTranslator.addRoots(this.toRoot(), t.toRoot())
  
  def translateTo(t: T){ 
    val (rootId1,rootId2) = addRoots(t)
    translateTo(t,rootId1,rootId2)
  }
  
  override def toString = toRoot().toString
}