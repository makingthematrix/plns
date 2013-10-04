package logic
import IgnoredNumber._

abstract class NounGenerator(val lang: String) {
  protected val patternMap = scala.collection.mutable.HashMap[String,DeclensionPattern]();
  
  def word(root: String,pattern: DeclensionPattern):Noun = word(root,pattern,NONE)
  def word(root: String,patternId: String):Noun = word(root,patternMap(patternId),NONE)
  def word(root: String,patternId: String,ignored: IgnoredNumber):Noun 
    = word(root,patternMap(patternId),ignored: IgnoredNumber)
  def word(root: String,pattern: DeclensionPattern,ignored: IgnoredNumber):Noun 
    = new Noun(root,pattern,ignored,lang)
  
  def examples = patternMap.values.map(d => d.example)
  def ids = patternMap.keys
  def idsExamples = {
    val map = patternMap.map( t=> (t._1,"" + t._1 + ":" + t._2.example) )
    map.toMap
  }
  def patterns = patternMap.values
}