package logic

import logic.Decl._

case class CaseDescription(id:String, lang:String, gender: String, degree: String, c: String, short:String, long:String, question:String){
  def this(l: String, g: String, d: String, c: String, decl: String) = this(CaseDescription.key(l, g, d, c), l, g, d, c, "", "", "")
}

object CaseDescription{
  private def cd1(id: String) = {
    val lang = id.substring(0,2)
    val gender = id.substring(2,3)
    val degree = id.substring(3,4)
    val longDescr = degrees(degree) + ", " + numbers(gender) + ", " + genders(gender)
    val cd = new CaseDescription(id,lang,gender,degree,"","",longDescr,"")
    id -> cd
  }
  
 private def cd2(id: String) = {
   val lang = id.substring(0,2)
   val decl = id.substring(2).toUpperCase()
   id -> new CaseDescription(id,lang,"","",decl,declShort(decl),declLong(decl),declQuestion(decl));
 }
 
  lazy val languages:Map[String,String] = Map(
    "pl" -> "język polski",
    "ns" -> "jezyk novosloviensky"
  )
  
  lazy val genders:Map[String,String] = Map(
    "m" -> "rodzaj męski",
    "M" -> "rodzaj męski",
    "f" -> "rodzaj żeński",
    "F" -> "rodzaj żeński",
    "n" -> "rodzaj nijaki",
    "N" -> "rodzaj nijaki",
    "p" -> "rodzaj męskoosobowy",
    "P" -> "rodzaj niemięskoosobowy" //!
  )
  
  lazy val numbers:Map[String,String] = Map(
    "m" -> "liczba pojedyncza",
    "M" -> "liczba pojedyncza",
    "f" -> "liczba pojedyncza",
    "F" -> "liczba pojedyncza",
    "n" -> "liczba pojedyncza",
    "N" -> "liczba pojedyncza",
    "s" -> "liczba pojedyncza",
    "S" -> "liczba pojedyncza",
    "p" -> "liczba mnoga",
    "P" -> "liczba mnoga"
  )
  
  lazy val degrees:Map[String,String] = Map(
    "i" -> "stopień równy",
    "c" -> "stopień wyższy",
    "s" -> "stopień najwyższy"
  )
  
  lazy val tenses:Map[String,String] = Map(
    "PRES" -> "czas teraźniejszy",
    "PAST" -> "czas przeszły",
    "INF" -> "bezokolicznik",
    "IMP" -> "tryb rozkazujący",
    "ACTIVE" -> "imiesłów czynny",
    "PASSIVE" -> "imiesłów bierny",
    "NOUN" -> "rzeczownik",
    "PERFECT" -> "imiesłów uprzedni"
  )
  
  lazy val personsShort:Map[String,String] = Map(
    "1S" -> "1 os lp",
    "2S" -> "2 os lp",
    "3S" -> "3 os lp",
    "1P" -> "1 os lmn",
    "2P" -> "2 os lmn",
    "3P" -> "3 os lmn"
  )
  
  lazy val personsLong:Map[String,String] = Map(
    "1S" -> "1 osoba liczby pojedynczej",
    "2S" -> "2 osoba liczby pojedynczej",
    "3S" -> "3 osoba liczby pojedynczej",
    "1P" -> "1 osoba liczby mnogiej",
    "2P" -> "2 osoba liczby mnogiej",
    "3P" -> "3 osoba liczby mnogiej"
  )
  
  lazy val gendersSeq:Seq[String] = Seq("m","f","n","P")
  lazy val personsSeq:Seq[String] = Seq( "1S", "2S", "3S", "1P", "2P", "3P" )
  lazy val impPersonsSeq:Seq[String] = Seq( "2S", "1P", "2P" )
  
  lazy val conjShort:Map[String,String] = Map(
    "INF" -> "bezokolicznik",
    "PRES1S" -> "cz ter 1 os lp",
    "PRES2S" -> "cz ter 2 os lp",
    "PRES3S" -> "cz ter 3 os lp",
    "PRES1P" -> "cz ter 1 os lmn",
    "PRES2P" -> "cz ter 2 os lmn",
    "PRES3P" -> "cz ter 3 os lmn",
    "PAST1SM" -> "cz prze 1 os lp rm", 
    "PAST1SF" -> "cz prze 1 os lp rż", 
    "PAST2SM" -> "cz prze 2 os lp rm",  
    "PAST2SF" -> "cz prze 2 os lp rż", 
    "PAST3SM" -> "cz prze 3 os lp rm", 
    "PAST3SF" -> "cz prze 3 os lp rż", 
    "PAST3SN" -> "cz prze 3 os lp rn",
    "PAST1PM" -> "cz prze 1 os lmn rm", 
    "PAST1PF" -> "cz prze 1 os lmn rż", 
    "PAST2PM" -> "cz prze 2 os lmn rm", 
    "PAST2PF" -> "cz prze 2 os lmn rż", 
    "PAST3PM" -> "cz prze 3 os lmn rm", 
    "PAST3PF" -> "cz prze 3 os lmn rż",
    "ACTIVE" -> "im czynny", 
    "PASSIVE" -> "im bierny",
    "IMP2S" -> "tr roz 2 os lp", 
    "IMP1P" -> "tr roz 1 os lmn", 
    "IMP2P" -> "tr roz 2 os lmn",
    "NOUN" -> "rzeczownik"
  )
  
  lazy val conjLong:Map[String,String] = Map(
    "INF" -> "bezokolicznik",
    "PRES1S" -> "czas teraźniejszy, 1 osoba liczby pojedynczej",
    "PRES2S" -> "czas teraźniejszy, 2 osoba liczby pojedynczej",
    "PRES3S" -> "czas teraźniejszy, 3 osoba liczby pojedynczej",
    "PRES1P" -> "czas teraźniejszy, 1 osoba liczby mnogiej",
    "PRES2P" -> "czas teraźniejszy, 2 osoba liczby mnogiej",
    "PRES3P" -> "czas teraźniejszy, 3 osoba liczby mnogiej",
    "PAST1SM" -> "czas przeszły, 1 osoba liczby pojedynczej, rodzaj męski", 
    "PAST1SF" -> "czas przeszły, 1 osoba liczby pojedynczej, rodzaj żeński", 
    "PAST2SM" -> "czas przeszły, 2 osoba liczby pojedynczej, rodzaj męski",  
    "PAST2SF" -> "czas przeszły, 2 osoba liczby pojedynczej, rodzaj żeński", 
    "PAST3SM" -> "czas przeszły, 3 osoba liczby pojedynczej, rodzaj męski", 
    "PAST3SF" -> "czas przeszły, 3 osoba liczby pojedynczej, rodzaj żeński", 
    "PAST3SN" -> "czas przeszły, 3 osoba liczby pojedynczej, rodzaj nijaki",
    "PAST1PM" -> "czas przeszły, 1 osoba liczby mnogiej, rodzaj męskoosobowy", 
    "PAST1PF" -> "czas przeszły, 1 osoba liczby mnogiej, rodzaj żeńskoosobowy", 
    "PAST2PM" -> "czas przeszły, 2 osoba liczby mnogiej, rodzaj męskoosobowy", 
    "PAST2PF" -> "czas przeszły, 2 osoba liczby mnogiej, rodzaj żeńskoosobowy", 
    "PAST3PM" -> "czas przeszły, 3 osoba liczby mnogiej, rodzaj męskoosobowy", 
    "PAST3PF" -> "czas przeszły, 3 osoba liczby mnogiej, rodzaj żeńskoosobowy",
    "ACTIVE" -> "imiesłów czynny", 
    "PASSIVE" -> "imiesłów bierny",
    "IMP2S" -> "tryb rozkazujący, 2 osoba liczby pojedynczej", 
    "IMP1P" -> "tryb rozkazujący, 1 osoba liczby mnogiej", 
    "IMP2P" -> "tryb rozkazujący, 2 osoba liczby mnogiej",
    "NOUN" -> "rzeczownik"
  )
  
  lazy val declShort:Map[String,String] = Map(
    "NOM" -> "Mn",
    "NOMS" -> "Mn",
    "NOMP" -> "Mn",
    "GEN" -> "D",
    "GENS" -> "D",
    "GENP" -> "D",
    "DAT" -> "C",
    "DATS" -> "C",
    "DATP" -> "C",
    "ACC" -> "B",
    "ACCS" -> "B",
    "ACCP" -> "B",
    "VOC" -> "W",
    "VOCS" -> "W",
    "VOCP" -> "W",
    "LOC" -> "Mc",
    "LOCS" -> "Mc",
    "LOCP" -> "Mc",
    "INS" -> "N",
    "INSS" -> "N",
    "INSP" -> "N"
  )
  
  lazy val declLong:Map[String,String] = Map(
    "NOM" -> "Mianownik",
    "NOMS" -> "Mianownik",
    "NOMP" -> "Mianownik",
    "GEN" -> "Dopełniacz",
    "GENS" -> "Dopełniacz",
    "GENP" -> "Dopełniacz",
    "DAT" -> "Celownik",
    "DATS" -> "Celownik",
    "DATP" -> "Celownik",
    "ACC" -> "Biernik",
    "ACCS" -> "Biernik",
    "ACCP" -> "Biernik",
    "VOC" -> "Wołacz",
    "VOCS" -> "Wołacz",
    "VOCP" -> "Wołacz",
    "LOC" -> "Miejscownik",
    "LOCS" -> "Miejscownik",
    "LOCP" -> "Miejscownik",
    "INS" -> "Narzędnik",
    "INSS" -> "Narzędnik",
    "INSP" -> "Narzędnik"
  )
  
  lazy val declQuestion:Map[String,String] = Map(
    "NOM" -> "Kto? Co?",
    "NOMS" -> "Kto? Co?",
    "NOMP" -> "Kto? Co?",
    "GEN" -> "Kogo? Czego?",
    "GENS" -> "Kogo? Czego?",
    "GENP" -> "Kogo? Czego?",
    "DAT" -> "Komu? Czemu?",
    "DATS" -> "Komu? Czemu?",
    "DATP" -> "Komu? Czemu?",
    "ACC" -> "Kogo? Co?",
    "ACCS" -> "Kogo? Co?",
    "ACCP" -> "Kogo? Co?",
    "VOC" -> "O!",
    "VOCS" -> "O!",
    "VOCP" -> "O!",
    "LOC" -> "O kim? O czym?",
    "LOCS" -> "O kim? O czym?",
    "LOCP" -> "O kim? O czym?",
    "INS" -> "Z kim? Z czym?",
    "INSS" -> "Z kim? Z czym?",
    "INSP" -> "Z kim? Z czym?"
  )
  
  lazy val cases:Map[String,CaseDescription] = Map(
    cd1("plmi"),cd1("plfi"),cd1("plni"),cd1("plpi"),cd1("plPi"),
    cd1("nsmi"),cd1("nsfi"),cd1("nsni"),cd1("nspi"),
    cd1("plmc"),cd1("plfc"),cd1("plnc"),cd1("plpc"),cd1("plPc"),
    cd1("nsmc"),cd1("nsfc"),cd1("nsnc"),cd1("nspc"),
    cd2("plnom"),cd2("plNOMS"),cd2("plNOMP"),
    cd2("plgen"),cd2("plGENS"),cd2("plGENP"),
    cd2("pldat"),cd2("plDATS"),cd2("plDATP"),
    cd2("placc"),cd2("plACCS"),cd2("plACCP"),
    cd2("plvoc"),cd2("plVOCS"),cd2("plVOCP"),
    cd2("plloc"),cd2("plLOCS"),cd2("plLOCP"),
    cd2("plins"),cd2("plINSS"),cd2("plINSP"),
    cd2("nsnom"),cd2("nsNOMS"),cd2("nsNOMP"),
    cd2("nsgen"),cd2("nsGENS"),cd2("nsGENP"),
    cd2("nsdat"),cd2("nsDATS"),cd2("nsDATP"),
    cd2("nsacc"),cd2("nsACCS"),cd2("nsACCP"),
    cd2("nsvoc"),cd2("nsVOCS"),cd2("nsVOCP"),
    cd2("nsloc"),cd2("nsLOCS"),cd2("nsLOCP"),
    cd2("nsins"),cd2("nsINSS"),cd2("nsINSP")
  );
  
  def get(id: String) = cases.getOrElse(id,null);
  
  def ids() = cases.keySet;
  
  def key(lang: String, gender: String, degree: String, decl: String): String = lang + gender + degree + decl.toUpperCase()
  def key(lang: String, decl: String): String = lang + decl.toUpperCase()
  def key(lang: String, decl: Decl.Value): String = lang + decl
  
}