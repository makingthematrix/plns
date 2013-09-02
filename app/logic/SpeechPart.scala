package logic

trait SpeechPart[T <: SpeechPart[T]] {
  def translateTo(speechPart: T,rootId1: Long,rootId2: Long)
  def mainRoot: String
  val speechPart: String
  val lang: String
  
  def toRoot():Root
	
  def addRoots(t: T):(Long,Long) = NSTranslator.addRoots(this.toRoot(), t.toRoot())
  
  def translateTo(verb: T){ 
    val (rootId1,rootId2) = addRoots(verb)
    translateTo(verb,rootId1,rootId2)
  }
}