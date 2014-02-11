package logic
import scala.collection.mutable
import SpeechPart._
import models.UninflectedPair
import play.api.Logger

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
	("ten","toj"), ("ta","ta"), ("to", "to"), 
	("tego","togo"),("tej","toj"),("tych","tieh"),
	("temu","tomu"), ("tym","tiem"),
    ("tą", "toj"), ("tymi", "tiemi")
  )
	
  lazy val toBe = Seq(
	("być", "byti"), ("bycie","bytije"),
	("jestem","jesm"),("jesteś","jesi"),("jest","je"),("jesteśmy","jesme"),("jesteście","jeste"),("są","sut"),
	("będę", "budu"), ("będziesz","budeš"), ("będzie","bude"), ("będziemy","budeme"), ("będziecie","budete"), ("będą","budut"),
	("będąc","buduč"), ("będący","budučy"), ("będąca","buduča"), ("będące","buduče"), ("bądź","budi"),
	("bądźmy","budime"), ("bądźcie","budite"), ("bym","byh"), ("byś","bys"), ("by","by"), ("byśmy","byhom"), 
	("byście","byste"), ("byłem","byl"), ("byłam","byla"), ("byłeś","byl"), ("byłaś","byla"), ("był","byl"),
	("była","byla"), ("było","bylo"), ("byliśmy","byli"), ("byłyśmy","byle"), ("byliście","byli"), ("byłyście","byle"),
	("byli","byli"), ("były","byle")
  ) 
	  
  private def add(from: String, to:String) = DictionaryFactory.dict.add(new UninflectedPair(from, to))
  
  def init() = {
    Logger("MyApp").debug("NSTranslator.init. pronouns: " + pronouns.size)
	pronouns.foreach{ t => add(t._1,t._2) }
    Logger("MyApp").debug("toBe: " + toBe.size)
	toBe.foreach{ t => add(t._1,t._2) }    
    Logger("MyApp").debug("NSTranslator.init done")
  }
  
}