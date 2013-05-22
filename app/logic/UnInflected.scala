package logic

class UnInflected(val word: String) extends SpeechPart[UnInflected] {
  override def translateTo(singleWord: UnInflected) = NSTranslator.add(word, singleWord.word);
  
  override def mainRoot = word;
}

object UnInflected {
  def word(_word: String) = new UnInflected(_word);
  
  def main(args: Array[String]): Unit = {
    NSTranslator.add(new UnInflected("w"),new UnInflected("v"));
    NSTranslator.example("w");
  }
}