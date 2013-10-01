package logic

import scala.collection._

abstract class VerbGenerator(val lang: String) {
  protected val patternMap = mutable.HashMap[String,ConjugationPattern]();
  
  def word(infRoot: String, impRoot:String, patternId: String,perfective: Boolean):Verb
    = new Verb(infRoot,impRoot,patternMap(patternId),lang,perfective);

  def examples = patternMap.values.map(d => d.example)
  def ids = patternMap.keys
  def idsExamples = patternMap.map( t=> (t._1,"" + t._1 + ":" + t._2.example) ).toMap
  def patterns = patternMap.values
  
  def getCopula(c: Conj.Value):String
  def getCopulaFuture(c: Conj.Value):String
}