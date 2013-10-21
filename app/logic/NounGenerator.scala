package logic
import IgnoredNumber._

abstract class NounGenerator(val lang: String) {
  protected val patternMap = scala.collection.mutable.HashMap[String,DeclensionPattern]();
  
  def word(stem: String,pattern: DeclensionPattern, speechPartId: Long): Noun = word(stem, pattern, NONE)
  def word(stem: String, patternId: String, speechPartId: Long): Noun = word(stem, patternMap(patternId), NONE)
  def word(stem: String,patternId: String,ignored: IgnoredNumber):Noun 
    = word(stem, patternMap(patternId), ignored)
  def word(stem: String,pattern: DeclensionPattern,ignored: IgnoredNumber):Noun 
    = new Noun(stem, pattern, ignored, lang)
  
  def examples = patternMap.values.map(d => d.example)
  def ids = patternMap.keys
  def idsExamples = {
    val map = patternMap.map( t=> (t._1,"" + t._1 + ":" + t._2.example) )
    map.toMap
  }
  def patterns = patternMap.values
}