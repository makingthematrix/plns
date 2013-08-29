package logic

import Decl._;
import scala.collection.mutable;

case class AdjectiveException(val gender:String, val degree: String, val declCase: Decl.Value, val word: String);

case class Adjective (val ind:String,val cmp:String,val sup:String,val adverb:Adverb,
				      val indDeclension:Map[String,DeclensionPattern],
				      val cmpDeclension:Map[String,DeclensionPattern],
				      val cmpIgnored: Boolean,override val lang:String) 
                extends SpeechPart[Adjective]{
  
  override def mainRoot:String = { 
    val exOpt:Option[String] = exceptions.get(CaseDescription.key("m", "i", NOMS)); 
    exOpt match {
      case Some(str) => str
      case None => indDeclension("m").decline(ind, NOMS)
    }
  }
  
  override val speechPart = "adjective"
    
  override def toRoot():Root = new Root(mainRoot,speechPart,lang)
  
  val exceptions = new mutable.HashMap[String,String]()
  
  def except(patternCase: Array[(String,String,Decl.Value)], word: String): Adjective = {
    patternCase.foreach{ t => except(t._1,t._2,t._3,word) }
    return this;
  }
  
  def except(ex: AdjectiveException):Adjective = except(ex.gender, ex.degree, ex.declCase, ex.word);
  
  def except(gender: String, degree: String, declCase: Decl.Value, word: String): Adjective = {
    val key = CaseDescription.key(gender,degree,declCase)
    exceptions.put(key,word)
    return this;
  }
  
  def this(ind:String,cmp:String,sup:String,adv:Adverb,declMap:Map[String,DeclensionPattern],lang:String) 
    = this(ind,cmp,sup,adv,declMap,declMap,true,lang);
  def this(ind:String,cmp:String,sup:String,adv:Adverb,indDeclension:Map[String,DeclensionPattern],cmpDeclension:Map[String,DeclensionPattern],lang:String) 
    = this(ind,cmp,sup,adv,indDeclension,cmpDeclension,true,lang);
  def this(ind:String,adv:Adverb,declMap:Map[String,DeclensionPattern],lang:String)
    = this(ind,null,null,adv,declMap,null,false,lang);
  
  private def getDeclPattern(degree: String) = degree match {
    case "i" => (ind,indDeclension)
    case "c" if !cmpIgnored => (cmp,cmpDeclension)
    case "s" if !cmpIgnored => (sup,cmpDeclension)
    case _ => throw new IllegalArgumentException("I can't give you that declination, with degree="+degree+" and cmpIgnored="+cmpIgnored)
  }

  private def translateDegreeTo(adj: Adjective, degree: String,rootId1: Long,rootId2: Long): Unit = {
    val (fromRoot,fromDeclPattern) = getDeclPattern(degree)
    val (toRoot,toDeclPattern) = adj.getDeclPattern(degree)
  
    CaseDescription.gendersSeq.foreach( gender => {
      val fromDecl = fromDeclPattern(gender).decline(fromRoot)
      val toDecl = toDeclPattern(gender).decline(toRoot)
        
      Noun.declension.foreach(decl => {
        val key = CaseDescription.key(gender, degree, decl)
        val from = exceptions.getOrElse(key,fromDecl.getOrElse(decl, null));
        if(from != null){
          val to = adj.exceptions.getOrElse(key,toDecl.getOrElse(decl, null)) 
          if(to != null) NSTranslator.add(new Word(from,lang,rootId1,decl),new Word(to,adj.lang,rootId2,decl))
        }
      })
    })
  }
  
  override def translateTo(adj: Adjective) {
    val (rootId1, rootId2) = addRoots(adj)
    if(cmpIgnored || adj.cmpIgnored) translateDegreeTo(adj,"i",rootId1,rootId2)
    else CaseDescription.degrees.keys.foreach( degree => translateDegreeTo(adj,degree,rootId1,rootId2) )    
    if(adverb != null) adverb.translateTo(adj.adverb)
  }
}

object Adjective {
  def declMap(m:DeclensionPattern,f:DeclensionPattern,
			n:DeclensionPattern,p:DeclensionPattern):Map[String,DeclensionPattern] = declMap(m,f,n,p,p)
  
  def declMap(m:DeclensionPattern,f:DeclensionPattern,
			n:DeclensionPattern,p:DeclensionPattern,
			P:DeclensionPattern):Map[String,DeclensionPattern] = Map("m"->m,"f"->f,"n"->n,"p"->p,"P"->P)	
}