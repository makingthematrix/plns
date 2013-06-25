package logic

class UnInflected(val word: String,override val lang: String) extends SpeechPart[UnInflected] {
  override def mainRoot = word
  override val speechPart = "uninflected"
    
  override def toRoot():Root = new Root(mainRoot,speechPart,lang)
    
  override def translateTo(un: UnInflected){
    val (rootId1,rootId2) = addRoots(un)
    NSTranslator.add(new Word(word,lang,rootId1,""),new Word(un.word,un.lang,rootId2,""))
  }
}

object UnInflected {
  def word(_word: String,lang: String) = new UnInflected(_word,lang);
  
  def main(args: Array[String]): Unit = {
    NSTranslator.add(new UnInflected("w","pl"),new UnInflected("v","ns"));
    NSTranslator.example("w");
  }
}