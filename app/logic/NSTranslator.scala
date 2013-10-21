package logic
import scala.collection.mutable;
import SpeechPart._

object NSTranslator {
   
  lazy val pronouns = Seq(
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
  )
	
  lazy val toBe = Seq(
	("być", "byti"), ("bycie","bytije"),
	("jestem","jesm"),("jesteś","jesi"),("jest","je"),("jesteśmy","jesme"),("jesteście","jeste"),("są","sut"),
	("będę", "budu"), ("będziesz","budeš"), ("będzie","bude"), ("będziemy","budeme"), ("będziecie","budete"), ("będą","budut"),
	("będąc","buduč"), ("będący","budučy"), ("będąca","buduča"), ("będące","buduče"), ("bycie","bytije"), ("bądź","budi"),
	("bądźmy","budime"), ("bądźcie","budite"), ("bym","byh"), ("byś","bys"), ("by","by"), ("byśmy","byhom"), 
	("byście","byste"), ("byłem","byl"), ("byłam","byla"), ("byłeś","byl"), ("byłaś","byla"), ("był","byl"),
	("była","byla"), ("było","bylo"), ("byliśmy","byli"), ("byłyśmy","byle"), ("byliście","byli"), ("byłyście","byle"),
	("byli","byli"), ("były","byle")
  ) 
	  
  def add(from: String, to:String) 
    = DictionaryFactory.dict.add(new DictEntry(from, "pl", to, "ns", UnInflected.caseId, UNINFLECTED, -1L))
  
  def init() = {
	pronouns.foreach{ t => add(t._1,t._2) }
	toBe.foreach{ t => add(t._1,t._2) }    
  }
  
}