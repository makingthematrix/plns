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
  
  override def toRoot():Root = new Root(mainRoot,speechPart,lang)
	
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
	
  override def translateTo(noun: Noun){ 
	val (rootId1,rootId2) = addRoots(noun)
	lazy val fromDecl = decline()
	lazy val toDecl = noun.decline();
	Noun.declension.foreach(c => {
	  val from = exceptions.get(c) match {
	    case Some(ex) => ex
	    case None => fromDecl.get(c) match {
	      case Some(from) => from
	      case _ => throw new IllegalArgumentException("The case " + c + " does not exist in the declension of the noun " + this)
	    }
	  }
	    
	  val to = noun.exceptions.get(c) match {
	    case Some(ex) => ex
	    case None => toDecl.get(c) match {
	      case Some(to) => to
	      case _ => throw new IllegalArgumentException("The case " + c + " does not exist in the declension of the noun " + noun)
	    }
	  }
	   
	  NSTranslator.add(new Word(from,lang,rootId1,c),new Word(to,noun.lang,rootId2,c))
	})
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