package logic

import Conj._
import PLMode._
import scala.collection.mutable

private object ConjugationType extends Enumeration {
  type conjugationType = Value
  val TYPE_INF, TYPE_IMP, TYPE_COND = Value
  
  implicit def toString(t: ConjugationType.Value):String = t.toString()
  
  implicit def parse(str: String) = str.toLowerCase() match {
    case "type_inf" => TYPE_INF
    case "type_imp" => TYPE_IMP
    case "type_cond" => TYPE_COND
  }
}

import ConjugationType._

case class VerbException(val conjCase: Conj.Value, val word: String);

class Verb (val infRoot: String, val impRoot: String, val conjugation: ConjugationPattern, 
				 override val lang: String, val perfective: Boolean) extends SpeechPart[Verb] {
  override def mainRoot = conjugate(INF)
  override val speechPart = "verb"
  override def toRoot() = new Root(mainRoot,speechPart,lang)

  private val exceptions = mutable.Map[Conj.Value,String]()
  
  def except(ex: VerbException): Verb = {
	exceptions.put(ex.conjCase,ex.word)
	return this
  }
    
  def except(conjCase: Conj.Value, word: String): Verb = {
    exceptions.put(conjCase, word)
    return this
  }
  
  def except(conjCases: Seq[Conj.Value], word:String): Verb = {
    conjCases.foreach{ exceptions.put(_,word) }
    return this
  }

  private def infConjugation = conjugation.conjugate(infRoot, Verb.infConjCases)
  private def impConjugation = conjugation.conjugate(impRoot, Verb.impConjCases)
  private def condConjugation = conjugation.conjugate(infRoot, Verb.condConjCases)
  
  private def getConjugatedWord(c: Conj.Value,conj: Map[Conj.Value,String]) = exceptions.get(c) match {
    case Some(ex) => ex
	case None => conj.get(c) match {
	  case Some(from) => from
	  case _ => throw new IllegalArgumentException("The case " + c + " does not exist in the conjugation of the verb " + this.mainRoot)
    }
  }
	
  private def getConjugatedWord(c: Conj.Value,word: String) = exceptions.get(c) match {
    case Some(ex) => ex
	case None => word
  }
  
  private def translateConjugation(t: ConjugationType.Value,verb: Verb,rootId1: Long, rootId2: Long){
    lazy val thisConjugation = this.conjugationByType(t)
    lazy val thatConjugation = verb.conjugationByType(t)
    Verb.casesSet(t).foreach(c => {
	  val from = getConjugatedWord(c,thisConjugation)
	  val to = verb.getConjugatedWord(c,thatConjugation)
	  NSTranslator.add(new Word(from,lang,rootId1,c),new Word(to,verb.lang,rootId2,c))
	})
  }
  
  private def conjugationByType(t: ConjugationType.Value) = t match {
    case TYPE_INF => this.infConjugation
    case TYPE_IMP => this.impConjugation
    case TYPE_COND => this.condConjugation
  }
  
  private def getRoot(c: Conj.Value) = 
    if(Verb.infConjCases.contains(c)) infRoot
    else if(Verb.impConjCases.contains(c)) impRoot
    else if(Verb.condConjCases.contains(c)) infRoot
    else if(c == NOUN) infRoot
    else if(c == PERFECT) infRoot
    else throw new IllegalArgumentException("The case " + c + " does not belong to any conjugation.")
	
  private def conjugate(c: Conj.Value) = getConjugatedWord(c,conjugation.conjugate(getRoot(c),c))
  
  private def adjParticiple(verb: Verb,rootId1: Long,rootId2: Long,c: Conj.Value) = { 
    val from = conjugation.adjParticiple(conjugate(c))
    val to = verb.conjugation.adjParticiple(verb.conjugate(c))
    from.translateTo(to,rootId1,rootId2)
  }
  
  private def nounParticiple(verb: Verb,rootId1: Long,rootId2: Long) = {
	val from = conjugation.nounParticiple(conjugate(NOUN))
	val to = verb.conjugation.nounParticiple(verb.conjugate(NOUN))
	from.translateTo(to,rootId1,rootId2)
  }

  private def perfectParticiple(verb: Verb, rootId1: Long, rootId2: Long){
	val word1 = conjugation.conjugate(infRoot, PERFECT)
	val word2 = verb.conjugation.conjugate(verb.infRoot, PERFECT)
	val from = getConjugatedWord(PERFECT,word1)
	val to = verb.getConjugatedWord(PERFECT,word2)
	NSTranslator.add(new Word(from,lang,rootId1,PERFECT),new Word(to,verb.lang,rootId2,PERFECT))
  }
  
  override def translateTo(verb: Verb, rootId1: Long, rootId2: Long){ 
    translateConjugation(TYPE_INF,verb,rootId1,rootId2)	
	translateConjugation(TYPE_IMP,verb,rootId1,rootId2)
	translateConjugation(TYPE_COND,verb,rootId1,rootId2)
	
	if(perfective) perfectParticiple(verb, rootId1, rootId2) else adjParticiple(verb,rootId1,rootId2,ACTIVE)
	// zrobić unittest, czy adjParticiple odmienia przysłówkowy imiesłów -ąc
	
	adjParticiple(verb,rootId1,rootId2,PASSIVE)
	nounParticiple(verb,rootId1,rootId2)
  }
  
}

private object Verb {
  val infConjCases = Set(INF, PAST1SM, PAST1SF, PAST2SM, PAST2SF, PAST3SM, PAST3SF, PAST3SN,
					   PAST1PM, PAST1PF, PAST2PM, PAST2PF, PAST3PM, PAST3PF, PASSIVE)
  val impConjCases = Set(PRES1S, PRES2S, PRES3S,PRES1P, PRES2P, PRES3P,ACTIVE, IMP2S, IMP1P, IMP2P)  
  val condConjCases = Set(COND1SM, COND1SF, COND2SM, COND2SF, COND3SM, COND3SF, COND3SN,
					   COND1PM, COND1PF, COND2PM, COND2PF, COND3PM, COND3PF)
  val pastConjCases = Set(PAST1SM, PAST1SF, PAST2SM, PAST2SF, PAST3SM, PAST3SF, PAST3SN,
					   PAST1PM, PAST1PF, PAST2PM, PAST2PF, PAST3PM, PAST3PF)
  
  val cond2Past = Map(
      COND1SM -> PAST3SM, 
      COND1SF -> PAST3SF, 
      COND2SM -> PAST3SM, 
      COND2SF -> PAST3SF, 
      COND3SM -> PAST3SM, 
      COND3SF -> PAST3SF, 
      COND3SN -> PAST3SN,
	  COND1PM -> PAST3PM, 
	  COND1PF -> PAST3PF, 
	  COND2PM -> PAST3PM, 
	  COND2PF -> PAST3PF, 
	  COND3PM -> PAST3PM, 
	  COND3PF -> PAST3PF
  )
  
  val past2Present = Map(
      PAST1SM -> PRES1S, 
      PAST1SF -> PRES1S, 
      PAST2SM -> PRES2S, 
      PAST2SF -> PRES2S, 
      PAST3SM -> PRES3S, 
      PAST3SF -> PRES3S, 
      PAST3SN -> PRES3S,
	  PAST1PM -> PRES1P, 
	  PAST1PF -> PRES1P, 
	  PAST2PM -> PRES2P, 
	  PAST2PF -> PRES2P, 
	  PAST3PM -> PRES3P, 
	  PAST3PF -> PRES3P
  )

  val casesSet = Map(
      TYPE_INF -> infConjCases,
      TYPE_IMP -> impConjCases,
      TYPE_COND -> condConjCases
  )

}

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