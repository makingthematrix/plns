package logic

trait SpeechPart[T <: SpeechPart[T]] {
  def translateTo(speechPart: T): Boolean;
  def mainRoot: String;
  val speechPart: String;
  val lang: String;
	
  protected def addRoot():Option[Int] = NSTranslator.addRoot(mainRoot, speechPart, lang)

  def addRoots(t: T):Option[(Int,Int)] = this.addRoot() match {
    case Some(rootid1) => t.addRoot() match {
      case Some(rootid2) => Some(rootid1,rootid2)
      case None => None
    }
    case None => None
  }
}