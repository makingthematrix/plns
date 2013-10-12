package logic

import Decl._
import AdjectiveCase._
import AdjectiveGender._
import AdjectiveDegree._

/**
 * encapsulates logic for generating adjective cases 
 * @constructor creates a new adjective
 * @param ind the stem of INDICATIVE cases
 * @param ind the stem of COMPARATIVE cases
 * @param ind the stem of SUPERLATIVE cases
 * @param adverb the associated adverb, or None
 * @param declension encapsulates logic for generating cases
 * @param ignored marks if the noun can be used in both singular and plural forms, only singular (eg. capitalism), or only plural (eg. pants)
 * @param lang the language of the verb
 */
class Adjective(val ind:String,val cmp:String,val sup:String,val adverb:Option[Adverb],
				val indDeclension:Map[AdjectiveGender.Value,DeclensionPattern],
				val cmpDeclension:Map[AdjectiveGender.Value,DeclensionPattern],
				val cmpIgnored: Boolean,override val lang:String) 
  extends SpeechPart[Adjective]{
  override val speechPart = "adjective"
  override def mainRoot = decline(AdjectiveCase(MASCULINE, INDICATIVE, NOMS))
  override def toRoot() = new Root(mainRoot,speechPart,lang)
  
  override def generate(adj: Adjective):Seq[DictEntry] = {
	if((adverb == None && adj.adverb != None) || (adverb != None && adj.adverb == None))  
	  throw new IllegalArgumentException("Adjective.generate " + mainRoot + " -> " + adj.mainRoot + 
	                                     ": Associated adverbs of both adjectives must either exist or be None.") 
	
    val indSeq = generate(adj,INDICATIVE)
    
    val (cmpSeq,supSeq) = if(!cmpIgnored){
      val cmpSeq = generate(adj,COMPARATIVE)
      val supSeq = generate(adj,SUPERLATIVE)
      (cmpSeq,supSeq)
    } else (Seq(),Seq())   
    
    val advSeq = adverb match {
      case Some(adv) => adv.generate(adj.adverb.get)
      case _ => Seq()
    } 
	
    Seq(indSeq, cmpSeq, supSeq, advSeq).flatten
  }
  
  def decline(ac: AdjectiveCase) = {
    val (stem, declension) = stemAndDecl(ac.degree)
    getDeclinedWord(ac,declension(ac.gender).decline(stem,ac.declCase))
  }

  private def stemAndDecl(ad: AdjectiveDegree.Value) = ad match {
    case INDICATIVE => (ind, indDeclension)
    case COMPARATIVE if !cmpIgnored => (cmp, cmpDeclension)
    case SUPERLATIVE if !cmpIgnored => (sup, cmpDeclension)
    case _ => throw new IllegalArgumentException("I can't give you that declination, with ac="+ad+" and cmpIgnored="+cmpIgnored)
  }
    
  protected def getDeclinedWord(ac: AdjectiveCase,word: String) = exceptions.get(ac) match {
    case Some(ex) => ex
	case _ => word
  }
    
  private def generate(adj: Adjective,ac: AdjectiveCase,
		  			   thisStem: String, thatStem: String,
		  			   thisDecl: DeclensionPattern,thatDecl: DeclensionPattern):DictEntry = {
    val from = getDeclinedWord(ac,thisDecl.decline(thisStem,ac.declCase))
    val to = adj.getDeclinedWord(ac, thatDecl.decline(thatStem,ac.declCase)) 
    DictEntry(from,lang,to,adj.lang,ac.toString)
  }
  
  private def generate(adj: Adjective, ad: AdjectiveDegree.Value, ag: AdjectiveGender.Value, 
                       thisStem: String, thatStem: String,
                       thisDecl: DeclensionPattern,thatDecl: DeclensionPattern):Seq[DictEntry] = 
    Adjective.declensionByGender(ag).map( d => { 
    	val ac = AdjectiveCase(ag,ad,d)
        generate(adj,ac,thisStem,thatStem,thisDecl,thatDecl) 
    })
    
  
  private def generate(adj: Adjective, ad: AdjectiveDegree.Value):Seq[DictEntry] = {
    val (thisStem, thisDeclension) = stemAndDecl(ad)
    val (thatStem, thatDeclension) = adj.stemAndDecl(ad)
  
    AdjectiveGender.values.flatMap( ag => {
      val thisDecl = thisDeclension(ag)
      val thatDecl = thatDeclension(ag)
      generate(adj, ad, ag, thisStem, thatStem, thisDecl, thatDecl) 
    }).toSeq
  }
  
  override def validateExceptionKey(key: String): String = AdjectiveCase.parse(key).toString
}

object Adjective {
  def declMap(m:DeclensionPattern,f:DeclensionPattern,
			n:DeclensionPattern,p:DeclensionPattern):Map[AdjectiveGender.Value,DeclensionPattern] = declMap(m,f,n,p,p)
  
  def declMap(m:DeclensionPattern,f:DeclensionPattern,
			n:DeclensionPattern,p:DeclensionPattern,
			P:DeclensionPattern):Map[AdjectiveGender.Value,DeclensionPattern] = 
	Map(MASCULINE->m,FEMININE->f,NEUTER->n,PLURAL_MASCULINE->p,PLURAL_NONMASCULINE->P)	
	
  lazy val declensionByGender = Map(
    MASCULINE -> Noun.singularDeclension,
    FEMININE -> Noun.singularDeclension,
    NEUTER -> Noun.singularDeclension,
    PLURAL_MASCULINE -> Noun.pluralDeclension,
    PLURAL_NONMASCULINE -> Noun.pluralDeclension
  )
}