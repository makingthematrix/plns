package logic

import Decl._;
import scala.collection.mutable;

case class NounException(val declCase: Decl.Value, val word: String);

case class Noun(val root: String,val declension: DeclensionPattern,val singIgnored: Boolean, val pluralIgnored: Boolean,
                override val lang: String) 
  extends SpeechPart[Noun] {
  //println("New Noun. root: " + root + ", declension: " + declension.id + ", singIgnored: " + singIgnored + ", pluralIgnored: " + pluralIgnored)
  def this(root: String,declension: DeclensionPattern,lang: String) = this(root,declension,false,false,lang)
  
  override def mainRoot = exceptions.getOrElse(NOMS,decline()(NOMS));
  override val speechPart = "noun";
  
	
  private val exceptions = new mutable.HashMap[Decl.Value,String]();

  def except(ex: NounException): Noun = {
	exceptions.put(ex.declCase,ex.word)
	return this
  }
	
  def except(declCase: Decl.Value, word: String): Noun = {
	exceptions.put(declCase, word);
	return this;
  }
	
  def except(decl: Seq[Decl.Value], word: String): Noun = { 
	decl.foreach{ d => exceptions.put(d, word); }
	return this;
  }
	
  private def decline(): Map[Decl.Value,String] = {
    if(singIgnored) declension.decline(root,Noun.pluralDeclension)
    else if(pluralIgnored) declension.decline(root,Noun.singDeclension)
    else declension.decline(root,Noun.declension)
  }
	
  override def translateTo(noun: Noun): Boolean = addRoots(noun) match {
    case Some((rootId1,rootId2)) => {
      lazy val fromDecl = decline()
	  lazy val toDecl = noun.decline();
	  Noun.declension.foreach(c => {
	    val from = exceptions.getOrElse(c,fromDecl.getOrElse(c, null));
	    if(from != null){
	  	  val to = noun.exceptions.getOrElse(c,toDecl.getOrElse(c, null)) 
	  	  if(to != null) NSTranslator.add(from,rootId1,to,rootId2);
	    }	 
	  });
      true
    }
    case None => false
  } 
}

object Noun{
  lazy val declension = Seq( NOMS, GENS, DATS, ACCS, INSS, LOCS, VOCS, 
	                      	   NOMP, GENP, DATP, ACCP, INSP, LOCP, VOCP );  
  lazy val singDeclension = Seq( NOMS, GENS, DATS, ACCS, INSS, LOCS, VOCS );
  lazy val pluralDeclension = Seq( NOMP, GENP, DATP, ACCP, INSP, LOCP, VOCP );
}

abstract class NounGenerator(val lang: String) {
  protected val patternMap = scala.collection.mutable.HashMap[String,DeclensionPattern]();
  
  def word(root: String,pattern: DeclensionPattern):Noun = word(root,pattern,false,false)
  def word(root: String,patternId: String):Noun = word(root,patternMap(patternId),false,false)
  def word(root: String,patternId: String,singIgnored: Boolean, pluralIgnored: Boolean):Noun 
    = word(root,patternMap(patternId),singIgnored,pluralIgnored)
  def word(root: String,pattern: DeclensionPattern,singIgnored: Boolean, pluralIgnored: Boolean):Noun 
    = new Noun(root,pattern,singIgnored,pluralIgnored,lang)
  
  def examples = patternMap.values.map(d => d.example)
  def ids = patternMap.keys
  def idsExamples = {
    val map = patternMap.map( t=> (t._1,"" + t._1 + ":" + t._2.example) )
    map.toMap
  }
  def patterns = patternMap.values
}