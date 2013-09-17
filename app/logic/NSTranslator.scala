package logic

import Decl._
import PLMode._
import scala.collection.mutable;
import models.AdjectivePair

object NSTranslator {
  private var dictionary:AbstractDictionary = DictionaryFactory.DB_DICT;
  
  def changeDictionary(name: String):AbstractDictionary = name match {
    case "debug" => dictionary = DictionaryFactory.DEBUG_DICT; dictionary;
    case "db" => dictionary = DictionaryFactory.DB_DICT; dictionary
    case str => throw new IllegalArgumentException("Dictionary unknown: " + str)
  }
  
  def example(str: String) = {
    val (translation,untranslated) = dictionary.translate(str);
    println("'" + str + "' -> '" + translation + "'");
    untranslated.foreach(println);
  }
   
  lazy val pronouns = Array(
    ("ja","ja"), ("ty","ty"), ("on","on"), ("ona","ona"), ("ono", "ono"), ("my","my"), ("wy","vy"), ("oni","oni"), ("one","oni"),
	("mnie","mne"), ("ciebie","tebe"), ("siebie", "sebe"), ("jego","jego"), ("niego", "njego"), ("jej","jej"), ("niej","njej"), 
	("nas","nas"), ("was","vas"), ("ich","jih"), ("nich","njih"),
	("mi","mi"), ("tobie", "tebie"), ("ci","ti"), ("sobie", "sebie"), ("mu", "jemu"), ("jemu","jemu"), ("niemu","njemu"), 
	("nam", "nam"), ("wam","vam"), ("im","jim"), ("nim","njim"),
	("ją","jej"), ("nią", "njej"), ("nami","nami"),("wami","vami"), ("nimi","njimi"),
	("kto","kto"),("kogo","kogo"), ("komu","komu"), ("kim","kiem"),
	("co","čto"), ("czego","čego"), ("czemu","čemu"), ("czym","čim"),
	("ten","toj"), ("ta","ta"), ("to", "to"), ("ci","ti"),
	("tego","togo"),("tej","toj"),("tych","tieh"),
	("temu","tomu"), ("tym","tiem"),
    ("tą", "toj"), ("tymi", "tiemi")
  );
	
  lazy val toBe = Array(
	("być", "byti"), ("bycie","bytije"),
	("jestem","jesm"),("jesteś","jesi"),("jest","je"),("jesteśmy","jesme"),("jesteście","jeste"),("są","sut"),
	("będę", "budu"), ("będziesz","budeš"), ("będzie","bude"), ("będziemy","budeme"), ("będziecie","budete"), ("będą","budut"),
	("będąc","buduč"), ("będący","budučy"), ("będąca","buduča"), ("będące","buduče"), ("bycie","bytije"), ("bądź","budi"),
	("bądźmy","budime"), ("bądźcie","budite"), ("bym","byh"), ("byś","bys"), ("by","by"), ("byśmy","byhom"), 
	("byście","byste"), ("byłem","byl"), ("byłam","byla"), ("byłeś","byl"), ("byłaś","byla"), ("był","byl"),
	("była","byla"), ("było","bylo"), ("byliśmy","byli"), ("byłyśmy","byle"), ("byliście","byli"), ("byłyście","byle"),
	("byli","byli"), ("były","byle")
  ); 
	
  def add(plRoot: String, plPattern: DeclensionPattern, nsRoot: String, nsPattern: DeclensionPattern): Unit = {
	lazy val plDeclension = plPattern.decline(plRoot)
	lazy val nsDeclension = nsPattern.decline(nsRoot)
	Noun.declension.foreach(c => {
	  val pl = plDeclension.getOrElse(c, null);
	  if(pl != null){
		val ns = nsDeclension.getOrElse(c, null);
		val w1 = new Word(pl,"pl",-1,c)
		val w2 = new Word(ns,"ns",-1,c)
		if(ns != null) dictionary.add(w1,w2)
	  }	 
	});
  }
  	
  def addAdjective(plInd: String, plCmp: String,nsInd: String, nsCmp: String,mode: PLMode.Value, isRoot: Boolean): Unit =  {
	val pl = PLAdjective.word(plInd, plCmp, plInd, plCmp, mode, mode, false);
	val ns = NSAdjective.word(nsInd, nsCmp);
	pl.translateTo(ns);
  }

  def addAdjective(plRoot: String, nsRoot: String,mode: PLMode.Value, isRoot: Boolean): Unit =  {
	val pl = PLAdjective.word(plRoot, mode);
	val ns = NSAdjective.word(nsRoot);
	pl.translateTo(ns);
  }
  	
  def add[T <: SpeechPart[T]] (from: T,to: T):Unit = {
    println("NSTranslator.add, from: " + from + ", to: " + to)
    from.translateTo(to);
  }
	
  def addRoots(from: Root, to: Root):(Long,Long) = dictionary.addRoots(from,to)
  
  def add(from: Word, to: Word):Unit = dictionary.add(from,to);
  
  def list:Seq[Root] = dictionary.roots
  
  def rootPairs:Seq[(Root,Root)] = dictionary.rootPairs
  
  def add(from: String, to:String):Unit = dictionary.add(new Word(from,"pl",-1,""),new Word(to,"ns",-1,""))
  def update(from: String, to:String):Unit = dictionary.update(from,to)
  
  def isEmpty = dictionary.isEmpty;
  
  def init() = {
	pronouns.foreach{ t => add(t._1,t._2) }
	toBe.foreach{ t => add(t._1,t._2) } 
	addRoots(new Root("być","verb","pl"),new Root("byti","verb","ns"));
	add("mo",PLAdjective.PLURAL_MASCULINE_SOFT,"moj",NSAdjective.PLURAL);
	add("moj","moj",HARD,false);
	update("mój","moj");
	add("two",PLAdjective.PLURAL_MASCULINE_SOFT,"tvoj",NSAdjective.PLURAL);
	add("twoj","tvoj",HARD,false);
	update("twój","tvoj");
	add("swo",PLAdjective.PLURAL_MASCULINE_SOFT,"svoj",NSAdjective.PLURAL);
	add("swoj","svoj",HARD,false);
	update("swój","svoj");
	add("nasz","naš",HARD,false);
	add("wasz","vaš",HARD,false);
	update("kogo","koj");
	update("komu","koju");
	update("czyim","kojim");    
  }
  
  def translate(source: String): (String,Array[String]) = dictionary.translate(source);
  
  private def add(pl: String, ns: String, mode: PLMode.Value, isRoot: Boolean = true) = addAdjective(pl,ns,mode,isRoot);

  
  def all() = {
  }
  


}