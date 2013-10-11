package logic

import Decl._
import AdjectiveCase._
import AdjectiveGender._
import AdjectiveDegree._

class Adjective(val ind:String,val cmp:String,val sup:String,val adverb:Adverb,
				val indDeclension:Map[AdjectiveGender.Value,DeclensionPattern],
				val cmpDeclension:Map[AdjectiveGender.Value,DeclensionPattern],
				val cmpIgnored: Boolean,override val lang:String) 
  extends SpeechPart[Adjective]{
  
  override val speechPart = "adjective"
  override def mainRoot = decline(AdjectiveCase(MASCULINE, INDICATIVE, NOMS))
  override def toRoot() = new Root(mainRoot,speechPart,lang)
  
  override def translateTo(adj: Adjective, rootId1: Long, rootId2: Long) {
    translateDegreeTo(adj,INDICATIVE,rootId1,rootId2)
    if(!cmpIgnored){
      translateDegreeTo(adj,COMPARATIVE,rootId1,rootId2)
      translateDegreeTo(adj,SUPERLATIVE,rootId1,rootId2)
    }    
    if(adverb != null) adverb.translateTo(adj.adverb)
  }
  
  def decline(ac: AdjectiveCase):String = {
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
	case None => word
  }
    
  private def translateDegreeTo(adj: Adjective, ad: AdjectiveDegree.Value,rootId1: Long,rootId2: Long){
    val (thisStem, thisDeclension) = stemAndDecl(ad)
    val (thatStem, thatDeclension) = adj.stemAndDecl(ad)
  
    AdjectiveGender.singular.foreach( ag => {
      val thisDecl = thisDeclension(ag)
      val thatDecl = thatDeclension(ag)
      Noun.singularDeclension.foreach(d => {
        val ac = AdjectiveCase(ag,ad,d)
        val from = getDeclinedWord(ac,thisDecl.decline(thisStem,d))
        val to = adj.getDeclinedWord(ac, thatDecl.decline(thatStem,d))
	    NSTranslator.add(new Word(from,lang,rootId1,d),new Word(to,adj.lang,rootId2,d))
	  })
    })
    
    AdjectiveGender.plural.foreach( ag => {
      val thisDecl = thisDeclension(ag)
      val thatDecl = thatDeclension(ag)
      Noun.pluralDeclension.foreach(d => {
        val ac = AdjectiveCase(ag,ad,d)
        val from = getDeclinedWord(ac,thisDecl.decline(thisStem,d))
        val to = adj.getDeclinedWord(ac, thatDecl.decline(thatStem,d))
	    NSTranslator.add(new Word(from,lang,rootId1,d),new Word(to,adj.lang,rootId2,d))
	  })
    })
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
}