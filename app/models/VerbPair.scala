package models

import logic.Verb
import logic.PLVerb
import logic.NSVerb
import logic.NSTranslator
import logic.DictionaryFactory
import logic.DictEntry

/**
 * Holds all data needed for generating translations of all cases of a set of verbs.
 * @constructor creates a verb pair
 * @param plInfStem the stem for TYPE_INF and TYPE_COND cases of the main verb in the source language
 * @param plImpStem the stem for TYPE_IMP cases of the main verb in the source language
 * @param plPattern the id of the verb's conjugation pattern in the source language
 * @param plExceptions a string composed of exceptions from the regular conjugation in the source language, if there are any, or None
 * @param nsInfStem the stem for TYPE_INF and TYPE_COND cases of the main verb in the target language
 * @param nsImpStem the stem for TYPE_IMP cases of the main verb in the target language
 * @param nsPattern the id of the verb's conjugation pattern in the target language
 * @param nsExceptions a string composed of exceptions from the regular conjugation in the target language, if there are any, or None
 * @param prefixes a string composed of prefixes of derived verbs and markers if these verbs are in the perfective aspect
 */
case class VerbPair(override val id: Long,
                    plInfStem: String, plImpStem: String, plPattern: String, plExceptions: Option[String],
                    nsInfStem: String, nsImpStem: String, nsPattern: String, nsExceptions: Option[String],
                    prefixes: Option[String]) extends SpeechPartPair[Verb] {
  def this(plInfStem: String, plImpStem: String, plPattern: String, plExceptions: Option[String],
           nsInfStem: String, nsImpStem: String, nsPattern: String, nsExceptions: Option[String], prefixes: Option[String]) =
    this(SpeechPartPair.noId, plInfStem, plImpStem, plPattern, plExceptions,
         nsInfStem, nsImpStem, nsPattern, nsExceptions, prefixes)
  /**
   * Build a Verb of the source language based on the available data
   */
  override def pl = {
    // if the prefixes string starts with the perfective marker, it means that the verb is in the perfective aspect
    val perfective = prefixes.getOrElse("").startsWith(VerbPair.perfectiveMarker)
    // create the Verb of the source language
    val word = PLVerb.word(plInfStem, plImpStem, plPattern, perfective)
    // add exceptions, ignore prefixes
	addExceptions(word,plExceptions)
    // return the Verb
	word
  }
  
  /**
   * Build a Verb of the target language based on the available data
   */
  override def ns:Verb = {
    val perfective = prefixes.getOrElse("").startsWith(VerbPair.perfectiveMarker)
    // create the Verb of the target language
    val word = NSVerb.word(nsInfStem, nsImpStem, nsPattern, perfective)
	addExceptions(word,nsExceptions)
	word    
  }
  
  /**
   * Build a sequence of pairs of verbs (source,target), one pair of verbs for each pair of prefixes, 
   * and add them to the dictionary
   * @return a sequence of pairs of main roots; one for each pair of prefixes
   */
  override def add() = { 
    val (plRootId,nsRootId) = DictionaryFactory.dict.addRoots(pl.toRoot, ns.toRoot)
    val translations = prefixes match {
      case Some(pre) => VerbPair.prefixesAsSeq(pre).flatMap( tuple => generate(tuple._1,tuple._2) ).toSeq
      case None => generate("","")
    }
    translations.foreach{ entry => DictionaryFactory.dict.add(entry.wordPair(plRootId, nsRootId)) }
    Seq(((pl.mainRoot,ns.mainRoot)))
  }
  
  /**
   * For the given pair of prefixes, build a pair of verbs with given prefixes and generate their cases
   * @param plPrefix a prefix for the source verb - may be empty and may contain the perfective marker
   * @param nsPrefix a prefix for the target verb - may be empty
   * @return a sequence of all cases of the prefixed verbs
   */
  private def generate(plPrefix: String,nsPrefix: String) = {
    // if plPrefix contains the perfective marker, we have to extract it
    val (realPlPrefix,perfective) = if(!plPrefix.startsWith(VerbPair.perfectiveMarker)) (plPrefix,false)
    							    else (plPrefix.substring(1),true)
    // if this is the main verb (ie. no prefixes) just generate the verbs,
    // otherwise create a VerbPair for the verbs with the prefixes and only then generate the verbs
    val (plVerb,nsVerb) = if(realPlPrefix.isEmpty()) (this.pl,this.ns) 
    					  else prefixPair(realPlPrefix,nsPrefix,perfective)
    
    // generate all cases of the prefixed verbs
    plVerb.generate(nsVerb)
  }
  
  /**
   * if the prefix pair is non-empty, create a verb pair where the prefixes will be fixed to their stems
   * and use it to generate a pair of verbs
   * @param plPrefix a prefix for the source verb - must not be empty and not contain the perfective marker
   * @param nsPrefix a prefix for the target verb - must not be empty
   * @param perfective indicates that the new verbs will be in the perfective aspect
   */
  private def prefixPair(plPrefix:String,nsPrefix:String,perfective:Boolean):(Verb,Verb) = {
    if(plPrefix.isEmpty() || plPrefix == VerbPair.perfectiveMarker) 
      throw new IllegalArgumentException("VerbPair.prefixPair for ("+plInfStem+"->"+nsInfStem+"): " +
          "the method was called even though plPrefix is empty")
    if(nsPrefix.isEmpty() || nsPrefix == VerbPair.perfectiveMarker) 
      throw new IllegalArgumentException("VerbPair.prefixPair for ("+plInfStem+"->"+nsInfStem+"): " +
          "the method was called even though nsPrefix is empty")
    
    val plInf = plPrefix + plInfStem
    val plImp = plPrefix + plImpStem
    val plEx = VerbPair.prefixExceptions(plExceptions,plPrefix)
    
    val nsInf = nsPrefix + nsInfStem
    val nsImp = nsPrefix + nsImpStem
    val nsEx = VerbPair.prefixExceptions(nsExceptions,nsPrefix)
    
    // the new prefixes string will either be empty or contain only the perfective marker,
    // so the new pair will not try to add any new prefixes to the stem - which already has a prefix fixed to it
    val prefixes = VerbPair.pp("","",perfective)
    // create the new VerbPair
    val pair = VerbPair(SpeechPartPair.noId, plInf, plImp, plPattern, plEx, nsInf, nsImp, nsPattern, nsEx, Some(prefixes))
    // generate a pair of verbs with prefixes
    (pair.pl,pair.ns)
  }  
}

object VerbPair {
 
  /**
   * insert the prefix before each exception in the string
   * used when we have exceptions for the main verb and we know that the prefixed verb will have the same exceptions
   * - but each of such derived exceptions will have the given prefix
   * @param exceptions a string of exceptions, or None
   * @param prefix which needs to be added to each exception
   * @return a string of prefixed exceptions, or None
   */
  private def prefixExceptions(exceptions:Option[String],prefix:String):Option[String] = exceptions match {
    case Some(str) =>
      val parsed = SpeechPartPair.parseExceptions(str,prefix)
      val serialized = parsed.map(ve => "" + ve._1 + prefixSplitMarker + ve._2)
      val reduced = serialized.reduce( (t1,t2) => t1 + "," + t2)
      Some(reduced)
    case None => None
  }
  
  /**
   * takes the string of prefixes and perfective markers and parse them into prefixes pairs
   * ie. "_,*prze_pre" -> Seq(("",""),("*prze","pre"))
   * @param prefixes a string composed of prefixes pairs and perfective markers
   * @return a sequence of pairs of prefixes
   */
  private def prefixesAsSeq(prefixes: String):Seq[(String,String)] = prefixes match {
    case "" => Seq(("",""))
    case `prefixSplitMarker` => Seq(("",""))
    case str if str.contains(",") =>  {
      val tab = str.split(",")
      val chunks = tab.flatMap(prefixesAsSeq(_))
      chunks.toSeq
    }
    case prefix if prefix.contains(prefixSplitMarker) => {
      val arr = prefix.split(prefixSplitMarker) 
      Seq((arr(0),arr(1)))
    }
    case other => throw new IllegalArgumentException("VerbPair.prefixesAsSeq, unable to parse: " + other)
  }
  
  // all popular prefixes of verbs
  val allPrefixes = Seq(("",""),("po","po"),("za","s"),("w","v"),("s","iz"),
      ("za","za"),("do","do"),("prze","pre"),("przy","pri"),("u","u"),("od","ot"),("wy","vy") )
  
  val perfectiveMarker = "*"
  val prefixSplitMarker = "_"

  /** A shorthand to create a string composed of a prefix pair */
  def pp(from: String,to: String):String = pp(from,to,false)
  
  /** A shorthand to create a string composed of a prefix pair and a perfective marker */
  def pp(from: String,to: String,perfective: Boolean):String = 
    (if(perfective) perfectiveMarker else "") + from + prefixSplitMarker + to
}