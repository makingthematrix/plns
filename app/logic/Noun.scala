package logic

import Decl._;
import IgnoredNumber._

/**
 * The Noun class; encapsulates logic for generating noun cases 
 * @constructor creates a new noun
 * @param stem the stem of all cases
 * @param declension encapsulates logic for generating cases
 * @param ignored marks if the noun can be used in both singular and plural forms, only singular (eg. capitalism), or only plural (eg. pants)
 * @param lang the language of the verb
 */
class Noun(val stem: String,val declension: DeclensionPattern,val ignored: IgnoredNumber.Value,
           override val lang: String) extends SpeechPart[Noun] {
  override val speechPart = "noun"
  override def mainRoot = decline(NOMS);
  override def toRoot() = new Root(mainRoot,speechPart,lang)

  /** generates cases with the given declension and add them all to the dictionary
   *  @param noun a noun of another language which this one should be translated to
   *  @param rootId1 the id of the Root object of this noun in the DB [@todo: this should be NounPair id]
   *  @param rootId1 the id of the Root object of that noun in the DB [@todo: this should be NounPair id]
   */
  override def translateTo(noun: Noun, rootId1: Long, rootId2: Long){ 
	lazy val thisDeclension = declensionByIgnored()
	lazy val thatDeclension = noun.declensionByIgnored()
	Noun.casesTypeMap(ignored).foreach(d => {
	  val from = getDeclinedWord(d,thisDeclension(d))  
	  val to = noun.getDeclinedWord(d,thatDeclension(d))
	  NSTranslator.add(new Word(from,lang,rootId1,d),new Word(to,noun.lang,rootId2,d))
	})
  }

  def decline(d: Decl.Value):String = getDeclinedWord(d,declension.decline(stem,d))

  /** maps for case -> word regular forms of all, singular and plural cases */
  private lazy val bothDeclensions = declension.decline(stem,Noun.declension)
  private lazy val singularDeclension = declension.decline(stem,Noun.singularDeclension)
  private lazy val pluralDeclension = declension.decline(stem,Noun.pluralDeclension)

  /** choose the proper declension based on the ignored marker
   *  @return a case -> word map of regular forms for this noun 
   */
  private def declensionByIgnored():Map[Decl.Value,String] = ignored match {
    case NONE => bothDeclensions
    case SINGULAR => pluralDeclension
    case PLURAL => singularDeclension
  }

  /** for the given case and the declined regular form of the verb, return either the exception of this case, or that form
   *  @param d declension case
   *  @param word a regular form for this case
   *  @return either exception for this case or (if there is no exception) the regular form
   */
  protected def getDeclinedWord(d: Decl.Value,word: String) = exceptions.get(d) match {
    case Some(ex) => ex
	case None => word
  }
  
  override def validateExceptionKey(key: String): String = Decl.parse(key).toString
}

/** info about which cases belong to singluar and which to plural declensions */
object Noun{
  lazy val declension = Seq( NOMS, GENS, DATS, ACCS, INSS, LOCS, VOCS, 
	                      	   NOMP, GENP, DATP, ACCP, INSP, LOCP, VOCP );  
  lazy val singularDeclension = Seq( NOMS, GENS, DATS, ACCS, INSS, LOCS, VOCS );
  lazy val pluralDeclension = Seq( NOMP, GENP, DATP, ACCP, INSP, LOCP, VOCP );
  
  def casesTypeMap = Map(
    NONE -> declension,
    SINGULAR -> pluralDeclension,
    PLURAL -> singularDeclension
  )
}