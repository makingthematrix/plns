package logic

import Decl._
import PlMode._
import scala.collection.mutable;
import models.AdjectivePair

object NSTranslator {
  private var dictionary:AbstractDictionary = DictionaryFactory.DB_DICT;
  
  def changeDictionary(name: String) = name match {
    case "debug" => dictionary = DictionaryFactory.DEBUG_DICT;
    case "db" => dictionary = DictionaryFactory.DB_DICT;
    case _ =>
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
  	
  def addAdjective(plInd: String, plCmp: String,nsInd: String, nsCmp: String,mode: PlMode.Value, isRoot: Boolean): Unit =  {
	val pl = PLAdjective.word(plInd, plCmp, plInd, plCmp, mode, mode, false);
	val ns = NSAdjective.word(nsInd, nsCmp);
	pl.translateTo(ns);
  }

  def addAdjective(plRoot: String, nsRoot: String,mode: PlMode.Value, isRoot: Boolean): Unit =  {
	val pl = PLAdjective.word(plRoot, mode);
	val ns = NSAdjective.word(nsRoot);
	pl.translateTo(ns);
  }
  	
  def add[T <: SpeechPart[T]] (from: T,to: T):Unit = {
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
  
  private def add(pl: String, ns: String, mode: PlMode.Value, isRoot: Boolean = true) = addAdjective(pl,ns,mode,isRoot);
  private def plNoun(root: String, pattern: DeclensionPattern): Noun = PLNoun.word(root,pattern);
  private def nsNoun(root: String, pattern: DeclensionPattern): Noun = NSNoun.word(root,pattern);
  private def plVerb(root1: String, pattern: ConjugationPattern): Verb = PLVerb.word(root1,pattern);
  private def plVerb(root1: String, root2: String, pattern: ConjugationPattern): Verb = 
    PLVerb.word(root1,root2,pattern);
  private def nsVerb(root1: String, pattern: ConjugationPattern): Verb = NSVerb.word(root1,pattern);
  private def nsVerb(root1: String, root2: String, pattern: ConjugationPattern): Verb = 
    NSVerb.word(root1,root2,pattern);
  private def plAdjective(root: String, mode: PlMode.Value) = PLAdjective.word(root, mode)
  private def nsAdjective(root: String) = NSAdjective.word(root)
  private def plAdverb(root: String, mode: PlMode.Value) = PLAdverb.word(root, mode, false)
  private def nsAdverb(root: String) = NSAdverb.word1(root)
  private def plAdverb(ind: String, cmp: String, pattern: PlMode.Value) = PLAdverb.word(ind, cmp, pattern, false)
  private def nsAdverb(ind: String, cmp: String) = NSAdverb.word(ind, cmp, false)
  private def plWord(word: String) = UnInflected.word(word,"pl");
  private def nsWord(word: String) = UnInflected.word(word,"ns");
  
  def testInit() = {
    add("przykładow","priměrn",HARD);
    add("polsk","polsk",SOFT);
    add("międzysłowiańsk","medžuslovjansk",SOFT);
    add("nowosłowiańsk","novoslověnsk",SOFT);
    add(plNoun("zdani",PLNoun.SOFT_NEUTER),nsNoun("izrečenij",NSNoun.SOFT_NEUTER));
    add(plNoun("język",PLNoun.SOFT_MASCULINE_OBJECT_I),nsNoun("jazyk",NSNoun.HARD_MASCULINE_OBJECT));
    add("w","v");
    add(plAdverb("ciężk","cięż",HARD), nsAdverb("težk"));
    add(plVerb("pracow", "pracuj", PLVerb.II),nsVerb("dela","delaj",NSVerb.HARD));
    add(plWord("razem"),nsWord("zajedno"));
    add("ze","s");
    add(plNoun("męż",PLNoun.SOFT_MASCULINE_PERSON).except(NOMS, "mąż"),
        nsNoun("muž",NSNoun.SOFT_MASCULINE_PERSON));
    add(plNoun("żon",PLNoun.HARD_FEMININE_PERSON),nsNoun("žen",NSNoun.HARD_FEMININE)); 
    add(plVerb("powiad",PLVerb.I),nsVerb("povědaj", NSVerb.HARD)); 
    add("że","iže");
    add(plAdjective("jedn", HARD).except("m", "i", NOMS, "jeden"),
        nsAdjective("jedn").except("m", "i", NOMS, "jedin"));
    add(plAdjective("pewn",HARD),
        nsAdjective("jedn").except("m", "i", NOMS, "jedin"));
    add(plNoun("dni", PLNoun.SOFT_MASCULINE_OBJECT_I).except(NOMS,"dzień"),
        nsNoun("dn",NSNoun.HARD_MASCULINE_OBJECT).except(NOMS,"den"));
    add("mniej-więcej","kolě");
    add(plNoun("rok",PLNoun.HARD_MASCULINE_OBJECT),
        nsNoun("god", NSNoun.HARD_MASCULINE_OBJECT));
    add("jak","jako");
    add(plVerb("zerw", "zryw", PLVerb.I),nsVerb("razbi", NSVerb.SOFT));
    add("ze","s");
    add("po","po");
    add("i","i");
    add(plNoun("kontakt", PLNoun.HARD_MASCULINE_OBJECT),
        nsNoun("kontakt",NSNoun.HARD_MASCULINE_OBJECT));
    add(plAdjective("dawn",HARD),nsAdjective("davn"));
    add(plNoun("nauczyciel",PLNoun.SOFT_MASCULINE_PERSON),
        nsNoun("učitel",NSNoun.SOFT_MASCULINE_PERSON));
    add(plNoun("szymon", PLNoun.HARD_MASCULINE_PERSON),
        nsNoun("šymon",NSNoun.HARD_MASCULINE_PERSON));
    add(plVerb("chodz","chodź",PLVerb.I),
        nsVerb("hod",NSVerb.SOFT));
    add(plVerb("przychodz","przychodź",PLVerb.I),
        nsVerb("prihod",NSVerb.SOFT));
    add(plVerb("","",PLVerb.Xa),
        nsVerb("se",NSVerb.SOFT));
    add(plVerb("przy","przy",PLVerb.Xa),
        nsVerb("prise",NSVerb.SOFT));
    add("do","do");
    add(plNoun("kłódz",PLNoun.ACCORD_FEMININE).except(NOMS, "kłódź"),
        nsNoun("ključ",NSNoun.ACCORD_FEMININE));
    add(plNoun("strumieni",PLNoun.SOFT_MASCULINE_OBJECT_E).except(NOMS, "strumień"),
        nsNoun("tok",NSNoun.SOFT_MASCULINE_OBJECT));
    add("biał","běl",HARD);
    add(plNoun("wijen",PLNoun.HARD_FEMININE_PERSON),
        nsNoun("vijen",NSNoun.HARD_FEMININE));
    add("nad","nad");
    add("by","aby");
    add("aby","aby");
    add("tam","tamo");
    add(plNoun("cisz",PLNoun.HARD_FEMININE_PERSON).except(Array(GENS,LOCS), "ciszy"),
        nsNoun("tišin",NSNoun.HARD_FEMININE))
    add(plVerb("zgłębi","zgłębi",PLVerb.I),nsVerb("se uči",NSVerb.SOFT));
    add(plVerb("zgłęb","zgłąb",PLVerb.I),nsVerb("se nauči",NSVerb.SOFT));
    add(plNoun("sztuk",PLNoun.HARD_FEMININE_OBJECT).except(Array(DATS,LOCS),"sztuce"),
        nsNoun("umenij",NSNoun.SOFT_NEUTER));
    add("magiczn","čarovn",HARD);
    add(plAdverb("bardz", SOFT),nsAdverb("zěl"))
    add("wstępn","načatkov",HARD);
    add(plNoun("wersj",PLNoun.HARD_FEMININE_OBJECT)
    	.except(Array(DATS,LOCS),"wersji").except(NOMP,"wersje").except(VOCP,"wersje"),
    	nsNoun("versij",NSNoun.SOFT_FEMININE)
    );
    add(plNoun("tłumacz",PLNoun.HARD_MASCULINE_PERSON),nsNoun("prěložitel",NSNoun.HARD_MASCULINE_PERSON));
    add(plVerb("tłumacz",PLVerb.II),nsVerb("prěloži",NSVerb.SOFT))
    add("na","na");
  }
  
  def all() = {
  }
  
  def main(args: Array[String]): Unit = {
    //init();
    //testInit();
    //example("To jest przykładowe zdanie w języku polskim.");
    //example("To jest przykładowe blablabla w języku polskim.");
    //example("To są przykładowe zdania w językach polskim i nowosłowiańskim.");
   // example("Ten mąż ciężko pracuje razem ze swoją żoną.");
    //example("Powiadają, że dnia pewnego, mniej-więcej rok po tym jak zerwał kontakty ze swoim dawnym nauczycielem,");
    //example("Szymon przyszedł do Kłódzi, nad strumień Białej Wijeny, by tam w ciszy zgłębiać sztukę magiczną.")
    //example("Bardzo wstępna wersja tłumacza języka polskiego na nowosłowiański.");
    add(PLAdjective.word("lekk","lżej",SOFT),NSAdjective.word("legk"));
    example("lekki");
    dictionary.tuples.foreach(println);
    println(dictionary.roots.size);
  }

}