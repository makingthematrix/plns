package logic

import Decl._;
import models.AdjectiveTemplate

object NSAdjective {
  val MASCULINE = new DeclensionPattern("ns","MASCULINE","dobry") { // "dobry"
    override def suffices = Map(
      NOMS -> "y", GENS -> "ogo", DATS -> "omu", ACCS -> "ogo", 
      VOCS -> "y", LOCS -> "em", INSS -> "im" 
    )
  }
  
  val FEMININE = new DeclensionPattern("ns","FEMININE","dobra") { // "dobra"
    override def suffices = Map(
      NOMS -> "a", GENS -> "ej", DATS -> "ej", ACCS -> "u", 
      VOCS -> "a", LOCS -> "ej", INSS -> "ej"
    )
  }
  
  val NEUTER = new DeclensionPattern("ns","NEUTER","dobre") { // "dobre"
    override def suffices = Map(
      NOMS -> "e", GENS -> "ego", DATS -> "emu", ACCS -> "e", 
      VOCS-> "e", LOCS -> "em", INSS -> "im" 
    )
  }
  
  val PLURAL = new DeclensionPattern("ns","PLURAL","dobri") { // "dobri"
    override def suffices = Map(
      NOMP -> "i", GENP -> "ih" , DATP -> "im", ACCP -> "e", 
      VOCP -> "i", LOCP -> "ih", INSP -> "imi" 
    )
  }
  
  lazy val declMap = Adjective.declMap(MASCULINE,FEMININE,NEUTER,PLURAL)
  
  def word(ind:String):Adjective = word(ind,ind)
  def word(ind:String,cmp:String):Adjective = word(ind,cmp,ind,cmp,false)
  def word(ind:String,cmp:String,advInd:String,advCmp:String,cmpIgnored:Boolean):Adjective = 
    new Adjective(ind,cmp+"iejš","naj"+cmp+"iejš",NSAdverb.word(advInd, advCmp, cmpIgnored),declMap,declMap,cmpIgnored,"ns")
  def participle(ind:String) = new Adjective(ind,null,null,null,declMap,null,true,"ns")
  def template = new AdjectiveTemplate("ns","","iejš",MASCULINE.template,FEMININE.template,NEUTER.template,PLURAL.template,PLURAL.template)
}