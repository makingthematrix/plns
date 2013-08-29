package logic

import Conj._;
import PLMode._
import scala.collection.mutable;

case class VerbException(val conjCase: Conj.Value, val word: String);

case class Verb (val infRoot: String,val impRoot: String,val conjugation: ConjugationPattern,
				 override val lang: String,val ignoreActive: Boolean) extends SpeechPart[Verb]{
  override def mainRoot = exceptions.getOrElse(INF,infConjugation(INF));
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

  private def infConjugation = conjugation.conjugate(infRoot, Verb.infConj)
	
  private def impConjugation = conjugation.conjugate(impRoot, Verb.impConj)
  
  private def getConjugatedWord(c: Conj.Value,conj: Map[Conj.Value,String]) = exceptions.get(c) match {
    case Some(ex) => ex
	case None => conj.get(c) match {
	  case Some(from) => from
	  case _ => throw new IllegalArgumentException("The case " + c + " does not exist in the conjugation of the verb " + this)
    }
  }
	
  private def translateTo(verb: Verb, cases: Seq[Conj.Value], 
		  				  fromConj: Map[Conj.Value,String], 
		  				  toConj: Map[Conj.Value,String],
		  				  rootId1: Long, rootId2: Long
		  				 ): Unit = {
	cases.foreach(c => {
	  val from = getConjugatedWord(c,fromConj)
	  val to = verb.getConjugatedWord(c, toConj)
	  NSTranslator.add(new Word(from,lang,rootId1,c),new Word(to,verb.lang,rootId2,c))
	});
  }
  
  private def infTranslate(verb: Verb,rootId1: Long, rootId2: Long){
    println("Verb.translateTo, " + infRoot + " -> " + verb.infRoot)
	Verb.infConj.foreach(c => {
	  val from = getConjugatedWord(c,this.infConjugation)
	  val to = verb.getConjugatedWord(c,verb.infConjugation)
	  NSTranslator.add(new Word(from,lang,rootId1,c),new Word(to,verb.lang,rootId2,c))
	});
  }
  
  private def impTranslate(verb: Verb,rootId1: Long, rootId2: Long){
    println("Verb.translateTo, " + impRoot + " -> " + verb.impRoot)
	Verb.impConj.foreach(c => {
	  val from = getConjugatedWord(c,this.impConjugation)
	  val to = verb.getConjugatedWord(c,verb.impConjugation)
	  NSTranslator.add(new Word(from,lang,rootId1,c),new Word(to,verb.lang,rootId2,c))
	});
  }
  
  private def participle(c: Conj.Value) = {
	val word = getConjugatedWord(c,conjugation.conjugate(impRoot,c))
	lang match {
	  case "pl" => PLAdjective.participle(word)
	  case "ns" => NSAdjective.participle(word)
	}
  }
	
  override def translateTo(verb: Verb){ 
    val (rootId1,rootId2) = addRoots(verb)
    infTranslate(verb,rootId1,rootId2)	
	impTranslate(verb,rootId1,rootId2)
	
	// active
	if(!ignoreActive){
	  val fromActive = participle(ACTIVE)
	  val toActive = verb.participle(ACTIVE)
	  fromActive.translateTo(toActive)
  	}
	// passive
	val fromPassive = participle(PASSIVE)
	val toPassive = verb.participle(PASSIVE)
	fromPassive.translateTo(toPassive)
	
	// noun
	val fromNoun = getConjugatedWord(NOUN,conjugation.conjugate(impRoot,NOUN))
	val plNoun = PLNoun.participle(fromNoun)
	val toNoun = verb.getConjugatedWord(NOUN,verb.conjugation.conjugate(verb.impRoot,NOUN))
	val nsNoun = NSNoun.participle(toNoun)
	plNoun.translateTo(nsNoun)
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
  
  def word(infRoot: String, impRoot:String, conj: ConjugationPattern):Verb = new Verb(infRoot,impRoot,conj,lang,false);
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