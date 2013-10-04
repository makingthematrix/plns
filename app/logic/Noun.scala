package logic

import Decl._;
import IgnoredNumber._
import scala.collection.mutable;

class Noun(val stem: String,val declension: DeclensionPattern,val ignored: IgnoredNumber.Value,
           override val lang: String) extends SpeechPart[Noun] {
  override val speechPart = "noun";
  override def mainRoot = decline(NOMS);
  override def toRoot():Root = new Root(mainRoot,speechPart,lang)

  override def translateTo(noun: Noun, rootId1: Long, rootId2: Long){ 
	lazy val thisDeclension = declensionByIgnored()
	lazy val thatDeclension = noun.declensionByIgnored()
	Noun.casesTypeMap(ignored).foreach(d => {
	  val from = getDeclinedWord(d,thisDeclension(d))  
	  val to = noun.getDeclinedWord(d,thatDeclension(d))
	  NSTranslator.add(new Word(from,lang,rootId1,d),new Word(to,noun.lang,rootId2,d))
	})
  }
  
  /** maps for case -> word regular forms of all, singular and plural cases */
  private lazy val bothDeclensions = declension.decline(stem,Noun.declension)
  private lazy val singularDeclension = declension.decline(stem,Noun.singularDeclension)
  private lazy val pluralDeclension = declension.decline(stem,Noun.pluralDeclension)

  /** choose the proper conjugation map
   *  @param t conjugation type
   *  @return a case -> word map of regular forms for this verb 
   */
  private def declensionByIgnored():Map[Decl.Value,String] = ignored match {
    case NONE => bothDeclensions
    case SINGULAR => pluralDeclension
    case PLURAL => singularDeclension
  }
  
  def except(declCase: Decl.Value, word: String): Noun = {
	exceptions.put(declCase, word);
	return this;
  }
	
  def except(decl: Seq[Decl.Value], word: String): Noun = { 
	decl.foreach{ d => exceptions.put(d, word); }
	return this;
  }
  
  private val exceptions = new mutable.HashMap[Decl.Value,String]();

  private def decline(d: Decl.Value):String = getDeclinedWord(d,declension.decline(stem,d))

  private def getDeclinedWord(d: Decl.Value,word: String) = exceptions.get(d) match {
    case Some(ex) => ex
	case None => word
  }
}

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