package logic

class UnInflected(val word: String,override val lang: String) extends SpeechPart[UnInflected] {
  override def mainRoot = word
  override val speechPart = "uninflected"
    
  override def translateTo(un: UnInflected) = addRoots(un) match {
    case Some((rootId1,rootId2)) => {
      NSTranslator.add(word, rootId1, un.word, rootId2)
      true
    }
    case None => false
  }
}

object UnInflected {
  def word(_word: String,lang: String) = new UnInflected(_word,lang);
  
  def main(args: Array[String]): Unit = {
    NSTranslator.add(new UnInflected("w","pl"),new UnInflected("v","ns"));
    NSTranslator.example("w");
  }
}