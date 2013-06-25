package logic

import Conj._;
import PlMode._
import scala.collection.mutable;

case class VerbException(val conjCase: Conj.Value, val word: String);

case class Verb (val infRoot: String,val impRoot: String,val conjugation: ConjugationPattern,
				 override val lang: String) extends SpeechPart[Verb]{
  override def mainRoot = exceptions.getOrElse(INF,infConjugation()(INF));
  override val speechPart = "verb"
    
  override def toRoot():Root = new Root(mainRoot,speechPart,lang)

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
	
  private def translateTo(verb: Verb, cases: Seq[Conj.Value], 
		  				  fromConj: Map[Conj.Value,String], 
		  				  toConj: Map[Conj.Value,String],
		  				  rootId1: Long, rootId2: Long
		  				 ): Unit = {
	cases.foreach(c => {
	  val from = exceptions.get(c) match {
	    case Some(ex) => ex
	    case None => fromConj.get(c) match {
	      case Some(from) => from
	      case _ => throw new IllegalArgumentException("The case " + c + " does not exist in the conjugation of the verb " + this)
	    }
	  }
	  
	  val to = verb.exceptions.get(c) match {
	    case Some(ex) => ex
	    case None => toConj.get(c) match {
	      case Some(to) => to
	      case _ => throw new IllegalArgumentException("The case " + c + " does not exist in the conjugation of the verb " + verb)
	    }
	  }
	  
	  NSTranslator.add(new Word(from,lang,rootId1,c),new Word(to,verb.lang,rootId2,c))
	});
  }
	
  override def translateTo(verb: Verb){ 
    val (rootId1,rootId2) = addRoots(verb)
    //println("Verb.translateTo, " + infRoot + " -> " + verb.infRoot)
	lazy val fromInfConj = infConjugation()
	lazy val toInfConj = verb.infConjugation()
	translateTo(verb, Verb.infConj, fromInfConj, toInfConj,rootId1,rootId2)
		
	//println("Verb.translateTo, " + impRoot + " -> " + verb.impRoot)
	val fromImpConj = impConjugation()
	val toImpConj = verb.impConjugation()
	translateTo(verb, Verb.impConj, fromImpConj, toImpConj,rootId1,rootId2)
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

abstract class VerbGenerator(val lang: String) {
  protected val patternMap = scala.collection.mutable.HashMap[String,ConjugationPattern]();
  
  def word(infRoot: String, impRoot:String, conj: ConjugationPattern):Verb = new Verb(infRoot,impRoot,conj,lang);
  def word(infRoot: String, conj: ConjugationPattern):Verb = word(infRoot,infRoot,conj);
  def word(infRoot: String, impRoot:String, patternId: String):Verb = word(infRoot,impRoot,patternMap(patternId));
  def word(infRoot: String, patternId: String):Verb = word(infRoot,infRoot,patternId);
  
  def examples = patternMap.values.map(d => d.example)
  def ids = patternMap.keys
  def idsExamples = {
    val map = patternMap.map( t=> (t._1,"" + t._1 + ":" + t._2.example) )
    map.toMap
  }
  def patterns = patternMap.values
}