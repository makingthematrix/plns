package logic

import Conj._
import PLMode._
import ConjugationType._
import scala.collection.mutable

/**
 * The Verb class; encapsulates logic for generating of verb cases, participles and verb-derived nouns 
 * @constructor creates a new verb
 * @param infStem the stem for TYPE_INF and TYPE_COND cases
 * @param impStem the stem for TYPE_IMP cases
 * @param conjugation encapsulates logic for generating cases
 * @param lang the language of the verb
 * @param perfective a flag denoting the aspect of the verb: perfective or not
 * @see ConjugationType
 */
class Verb (val infStem: String, val impStem: String, val conjugation: ConjugationPattern, 
				 override val lang: String, val perfective: Boolean) extends SpeechPart[Verb] {
  override val speechPart = "verb"
  override def mainRoot = conjugate(INF)
  override def toRoot() = new Root(mainRoot,speechPart,lang)
  
  /** generates cases for TYPE_INF, TYPE_IMP and TYPE_COND, the passive participle and the noun and add them all to the dictionar
   *  @param verb a verb of another language which this one should be translated to
   *  @param rootId1 the id of the Root object of this verb in the DB [@todo: this should be VerbPair id]
   *  @param rootId1 the id of the Root object of that verb in the DB [@todo: this should be VerbPair id]
   */
  override def translateTo(verb: Verb, rootId1: Long, rootId2: Long){ 
    translateConjugation(TYPE_INF,verb,rootId1,rootId2)	
	translateConjugation(TYPE_IMP,verb,rootId1,rootId2)
	translateConjugation(TYPE_COND,verb,rootId1,rootId2)
	adjParticiple(verb,rootId1,rootId2,PASSIVE)
	noun(verb,rootId1,rootId2)
	
	// if the aspect is perfective, generate the perfect participle, otherwise, the active participle
	if(perfective) perfectParticiple(verb, rootId1, rootId2) 
	else adjParticiple(verb,rootId1,rootId2,ACTIVE)
	
  }
  
  /**
   * adds a verb exception
   * @param conjCase the conjugation case
   * @param word the exception form for this case
   */
  def except(conjCase: Conj.Value, word: String): Verb = {
    exceptions.put(conjCase, word)
    this
  }
  
  /**
   * in rare cases where the same exception form exists for many cases we can add them all
   * @param conjCases a sequence of conjugation cases
   * @param word the exception form, identical for all these cases
   */
  def except(conjCases: Seq[Conj.Value], word:String): Verb = {
    conjCases.foreach{ exceptions.put(_,word) }
    this
  }
  
  /** the map of exceptions, case -> word */
  private val exceptions = mutable.Map[Conj.Value,String]()
  
  /** maps for case -> word regular forms of TYPE_INF, TYPE_IMP and TYPE_COND cases */
  private lazy val infConjugation = conjugation.conjugate(infStem, Verb.infConjCases)
  private lazy val impConjugation = conjugation.conjugate(impStem, Verb.impConjCases)
  private lazy val condConjugation = conjugation.conjugate(infStem, Verb.condConjCases)

  /** choose the proper conjugation map
   *  @param t conjugation type
   *  @return a case -> word map of regular forms for this verb 
   */
  private def conjugationByType(t: ConjugationType.Value) = t match {
    case TYPE_INF => this.infConjugation
    case TYPE_IMP => this.impConjugation
    case TYPE_COND => this.condConjugation
  }
  
  /** for the given case and the conjugated regular form of the verb, return either the exception of this case, or that form
   *  @param c conjugation case
   *  @param word a regular form for this case
   *  @return either exception for this case or (if there is no exception) the regular form
   */
  private def getConjugatedWord(c: Conj.Value,word: String) = exceptions.get(c) match {
    case Some(ex) => ex
	case None => word
  }
  
  /** get the proper stem for the given conjugation case 
   * @param c conjugation case
   * @return either infStem or impStem  
   */
  private def stem(c: Conj.Value) = if(Verb.impConjCases.contains(c)) impStem else infStem
  
  /** get the conjugated form (regular or exception) for the given case
   *  @param c conjugation case
   *  @return the conjugated form
   */ 
  private def conjugate(c: Conj.Value) = getConjugatedWord(c,conjugation.conjugate(stem(c),c))
  
  /** conjugate and add to the dictionary all cases in the given conjugation type
   *  @param t conjugation type
   *  @param verb a verb of another language which this one should be translated to
   *  @param rootId1 the id of the Root object of this verb in the DB [@todo: this should be VerbPair id]
   *  @param rootId1 the id of the Root object of that verb in the DB [@todo: this should be VerbPair id] 
   */
  private def translateConjugation(t: ConjugationType.Value,verb: Verb,rootId1: Long, rootId2: Long){
    // get conjugation maps for both of them
    lazy val thisConjugation = this.conjugationByType(t)
    lazy val thatConjugation = verb.conjugationByType(t)
    // for each case belonging to this conjugation type conjugate both verbs in their respective languages and add them to the dictionary
    Verb.casesTypeMap(t).foreach(c => {
	  val from = getConjugatedWord(c,thisConjugation(c))
	  val to = verb.getConjugatedWord(c,thatConjugation(c))
	  NSTranslator.add(new Word(from,lang,rootId1,c),new Word(to,verb.lang,rootId2,c))
	})
  }
  
  /** create participles of both verbs and execute Adjective.translateTo on them
   *  @param verb a verb of another language which this one should be translated to
   *  @param rootId1 the id of the Root object of this verb in the DB [@todo: this should be VerbPair id]
   *  @param rootId1 the id of the Root object of that verb in the DB [@todo: this should be VerbPair id] 
   *  @param c conjugation case of the participle; ACTIVE or PASSIVE
   */
  private def adjParticiple(verb: Verb,rootId1: Long,rootId2: Long,c: Conj.Value){ 
    val from = conjugation.adjParticiple(conjugate(c))
    val to = verb.conjugation.adjParticiple(verb.conjugate(c))
    from.translateTo(to,rootId1,rootId2)
  }
  
  /** create verb-derived nouns of both verbs and execute Noun.translateTo on them
   *  @param verb a verb of another language which this one should be translated to
   *  @param rootId1 the id of the Root object of this verb in the DB [@todo: this should be VerbPair id]
   *  @param rootId1 the id of the Root object of that verb in the DB [@todo: this should be VerbPair id] 
   */  
  private def noun(verb: Verb,rootId1: Long,rootId2: Long) = {
	val from = conjugation.nounParticiple(conjugate(NOUN))
	val to = verb.conjugation.nounParticiple(verb.conjugate(NOUN))
	from.translateTo(to,rootId1,rootId2)
  }

  /** create the perfect participles of both verbs and add them to the dictionary
   *  the perfect participle is not an adjective really, as active and passive participles, but an uniflected word
   *  @param verb a verb of another language which this one should be translated to
   *  @param rootId1 the id of the Root object of this verb in the DB [@todo: this should be VerbPair id]
   *  @param rootId1 the id of the Root object of that verb in the DB [@todo: this should be VerbPair id] 
   */  
  private def perfectParticiple(verb: Verb, rootId1: Long, rootId2: Long){
    val from = conjugate(PERFECT)
    val to = verb.conjugate(PERFECT)
	NSTranslator.add(new Word(from,lang,rootId1,PERFECT),new Word(to,verb.lang,rootId2,PERFECT))
  }
  
}

/** info about which cases belong to which conjugations, etc. */
object Verb {
  /** @see ConjugationType for explanation */
  val infConjCases = Set(INF, PAST1SM, PAST1SF, PAST2SM, PAST2SF, PAST3SM, PAST3SF, PAST3SN,
					   PAST1PM, PAST1PF, PAST2PM, PAST2PF, PAST3PM, PAST3PF, PASSIVE)
  val impConjCases = Set(PRES1S, PRES2S, PRES3S,PRES1P, PRES2P, PRES3P,ACTIVE, IMP2S, IMP1P, IMP2P)  
  val condConjCases = Set(COND1SM, COND1SF, COND2SM, COND2SF, COND3SM, COND3SF, COND3SN,
					   COND1PM, COND1PF, COND2PM, COND2PF, COND3PM, COND3PF)
  val pastConjCases = Set(PAST1SM, PAST1SF, PAST2SM, PAST2SF, PAST3SM, PAST3SF, PAST3SN,
					   PAST1PM, PAST1PF, PAST2PM, PAST2PF, PAST3PM, PAST3PF)
  
  val casesTypeMap = Map(
      TYPE_INF -> infConjCases,
      TYPE_IMP -> impConjCases,
      TYPE_COND -> condConjCases
  )

  /** used in order to conjugate conditionals, where [conditional] = [copula for cond] + [past tense] or [past tense]+[suffix for cond] */
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
  /** used in order to conjugate the "long past tense" as [copula for present] + [past tense]*/
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

}
