package logic

case class Root(id: Long, root: String, speechPart: SpeechPart.Value, lang: String, speechPartId: Long){
  def this(root: String, speechPart: SpeechPart.Value, lang: String, speechPartId: Long) 
      = this(Root.noId, root, speechPart, lang, speechPartId)
  
  def copy = copyWithId(id)  
  def copyWithId(id: Long) = Root(id, root, speechPart, lang, speechPartId)
}

object Root {
  val noId = -1L
}