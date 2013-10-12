package logic

class UnInflected(val word: String,override val lang: String) extends SpeechPart[UnInflected] {
  override def mainRoot = word
  override val speechPart = "uninflected"
    
  override def toRoot():Root = new Root(mainRoot,speechPart,lang)  
  override def generate(un: UnInflected) = Seq(new DictEntry(word,lang,un.word,un.lang))
  override def validateExceptionKey(key: String): String = key // uninflected don't have exceptions
}

object UnInflected {
  def word(_word: String,lang: String) = new UnInflected(_word,lang);
  
  def main(args: Array[String]): Unit = {
    NSTranslator.add(new UnInflected("w","pl"),new UnInflected("v","ns"));
    NSTranslator.example("w");
  }
}