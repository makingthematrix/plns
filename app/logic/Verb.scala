package logic

import Conj._;
import PlMode._
import scala.collection.mutable;

case class VerbException(val conjCase: Conj.Value, val word: String);

case class Verb (val infRoot: String,val impRoot: String,val conjugation: ConjugationPattern) extends SpeechPart[Verb]{
  override def mainRoot = exceptions.getOrElse(INF,infConjugation()(INF));
	
  private val exceptions = mutable.Map[Conj.Value,String]();
  
  def except(ex: VerbException): Verb = {
	exceptions.put(ex.conjCase,ex.word)
	return this
  }
    
  def except(conjCase: Conj.Value, word: String): Verb = {
    exceptions.put(conjCase, word)
    return this
  }
  
  def except(conj: Seq[Conj.Value], word:String): Verb = {
    conj.foreach{ conjCase => exceptions.put(conjCase,word) }
    return this
  }

  def infConjugation() = conjugation.conjugate(infRoot, Verb.infConj)
	
  def impConjugation() = conjugation.conjugate(impRoot, Verb.impConj)
	
  private def translateTo(verb: Verb, cases: Seq[Conj.Value], fromConj: Map[Conj.Value,String], toConj: Map[Conj.Value,String]): Unit = {
	cases.foreach(c => {
	  val from = exceptions.getOrElse(c,fromConj.getOrElse(c, null));
	  if(from != null){
		val to = verb.exceptions.getOrElse(c,toConj.getOrElse(c, null));
		if(to != null) NSTranslator.add(from,to);
	  } 	 
	});
  }
	
  override def translateTo(verb: Verb): Unit = {
	//println("Verb.translateTo, " + infRoot + " -> " + verb.infRoot);
	lazy val fromInfConj = infConjugation();
	lazy val toInfConj = verb.infConjugation();
	translateTo(verb, Verb.infConj, fromInfConj, toInfConj);
		
	//println("Verb.translateTo, " + impRoot + " -> " + verb.impRoot);
	val fromImpConj = impConjugation();
	val toImpConj = verb.impConjugation();
	translateTo(verb, Verb.impConj, fromImpConj, toImpConj);
  }
}

object Verb {
  val infConj = Seq(INF, PAST1SM, PAST1SF, PAST2SM, PAST2SF, PAST3SM, PAST3SF, PAST3SN,
					   PAST1PM, PAST1PF, PAST2PM, PAST2PF, PAST3PM, PAST3PF);
  val impConj = Seq(PRES1S, PRES2S, PRES3S,PRES1P, PRES2P, PRES3P,ACTIVE, IMP2S, IMP1P, IMP2P);
  // NOUN - this form is not added automatically, but the GUI may ask the user if they want to open addNoun with this form
  // PASSIVE - as above, but with addAdjective
  // from active - the original form ("-ąc") is added, but the derivate forms ("-ący","-ąca",etc.) are only proposed, as above, with addAdjective
  
}

trait VerbGenerator {
  protected val patternMap = scala.collection.mutable.HashMap[String,ConjugationPattern]();
  
  def word(infRoot: String, impRoot:String, conj: ConjugationPattern):Verb = new Verb(infRoot,impRoot,conj);
  def word(infRoot: String, conj: ConjugationPattern):Verb = word(infRoot,infRoot,conj);
  def word(infRoot: String, impRoot:String, patternId: String):Verb =  word(infRoot,impRoot,patternMap(patternId));
  def word(infRoot: String, patternId: String):Verb = word(infRoot,infRoot,patternId);
  
  def examples = patternMap.values.map(d => d.example)
  def ids = patternMap.keys
  def idsExamples = {
    val map = patternMap.map( t=> (t._1,"" + t._1 + ":" + t._2.example) )
    map.toMap
  }
  def patterns = patternMap.values
}