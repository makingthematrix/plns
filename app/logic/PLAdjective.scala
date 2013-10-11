package logic

import Decl._
import models.AdjectiveTemplate

object PLMode extends Enumeration {
  type PLMode = Value;
  val HARD, SOFT = Value;
  
  implicit def toString(v: PLMode.Value) = v.toString()
  
  implicit def parse(str: String): PLMode.Value = str.toLowerCase() match {
    case "hard" => HARD
    case "soft" => SOFT
    case _ => throw new IllegalArgumentException("Unrecognized adjective mode: " + str)
  }
}

import PLMode._

object PLAdjective {
	val MASCULINE_HARD = new DeclensionPattern("pl","MASCULINE_HARD","twardy") { // "twardy"
	  override def suffices() = Map(
	    NOMS -> "y", GENS -> "ego", DATS -> "emu", ACCS -> "ego", 
	    VOCS -> "y", LOCS -> "ym", INSS -> "ym" 
	  )
	}
	
	val MASCULINE_SOFT = new DeclensionPattern("pl","MASCULINE_SOFT","miękki") { // "miękki"
	  override def suffices() = Map(
	    NOMS -> "i", GENS -> "iego", DATS -> "iemu", ACCS -> "iego", 
	    VOCS -> "i", LOCS -> "im", INSS -> "im" 
	  )
	}
  
	val FEMININE_HARD = new DeclensionPattern("pl","FEMININE_HARD","twarda") { // "twarda"
	  override def suffices() = Map(
	    NOMS -> "a", GENS -> "ej", DATS -> "ej", ACCS -> "ą", 
	    VOCS -> "a", LOCS -> "ej", INSS -> "ą"
	  );
	}
	
	val FEMININE_SOFT = new DeclensionPattern("pl","FEMININE_SOFT","miękka") { // "miękka"
	  override def suffices() = Map(
	    NOMS -> "a", GENS -> "iej", DATS -> "iej", ACCS -> "ą", 
	    VOCS -> "a", LOCS -> "iej", INSS -> "ą"
	  )
	}
	
	val NEUTER_HARD = new DeclensionPattern("pl","NEUTER_HARD","twarde") { // "twarde"
	  override def suffices() = Map(
	    NOMS -> "e", GENS -> "ego", DATS -> "emu", ACCS -> "e", 
	    VOCS -> "e", LOCS -> "ym", INSS -> "ym" 
	  )
	}
	
	val NEUTER_SOFT = new DeclensionPattern("pl","NEUTER_SOFT","miękkie") { // "miękkie"
	  override def suffices() = Map(
	    NOMS -> "ie", GENS -> "iego", DATS -> "iemu", ACCS -> "ie", 
	    VOCS -> "ie", LOCS -> "im", INSS -> "im" 
	  )
	}
	
	val PLURAL_MASCULINE_HARD = new DeclensionPattern("pl","PLURAL_MASCULINE_HARD","twardzi") { // "twardzi"
	  override def suffices() = Map(
	    NOMP -> "i", GENP -> "ych" , DATP -> "ym", ACCP -> "ych", 
	    VOCP -> "i", LOCP -> "ych", INSP -> "ymi" 
	  )
	}
	
	val PLURAL_MASCULINE_SOFT = new DeclensionPattern("pl","PLURAL_MASCULINE_SOFT","miękcy") { // "miękcy"
	  override def suffices() = Map(
	      NOMP -> "y", GENP -> "ich" , DATP -> "im", ACCP -> "ich", 
	      VOCP -> "y", LOCP -> "ich", INSP -> "imi" 
	  )
	}
	
	val PLURAL_NONMASCULINE_HARD = new DeclensionPattern("pl","PLURAL_NONMASCULINE_HARD","twarde") { // "twarde"
	  override def suffices() = Map(
	    NOMP -> "e", GENP -> "ych" , DATP -> "ym", ACCP -> "e", 
	    VOCP -> "e", LOCP -> "ych", INSP -> "ymi" 
	  )
	}
	
	val PLURAL_NONMASCULINE_SOFT = new DeclensionPattern("pl","PLURAL_NONMASCULINE_SOFT","miękkie") { // "miękkie"
	  override def suffices() = Map(
	    NOMP -> "ie", GENP -> "ich" , DATP -> "im", ACCP -> "ie", 
	    VOCP -> "ie", LOCP -> "ich", INSP -> "imi" 
	  )
	}
	
	lazy val hardModeMap = Adjective.declMap(MASCULINE_HARD,FEMININE_HARD,NEUTER_HARD,
	                             PLURAL_MASCULINE_HARD,PLURAL_NONMASCULINE_HARD)
	lazy val softModeMap = Adjective.declMap(MASCULINE_SOFT,FEMININE_SOFT,NEUTER_SOFT,
	                             PLURAL_MASCULINE_SOFT,PLURAL_NONMASCULINE_SOFT)
	
	def word(ind:String,mode:PLMode.Value): Adjective = word(ind,null,ind,null,mode,mode,true) // testing purposes only
	def word(ind:String,cmp:String,mode:PLMode.Value): Adjective = word(ind,cmp,ind,cmp,mode,mode,false) // testing purposes only
	
	def word(ind:String,cmp:String,advInd:String,advCmp:String,mode: PLMode.Value, advMode: PLMode.Value, cmpIgnored: Boolean): Adjective 
	  = mode match {
	  case HARD => new Adjective(ind,cmp+"sz","naj"+cmp+"sz",
	                             PLAdverb.word(advInd, advCmp, advMode,cmpIgnored),
	                             hardModeMap,hardModeMap,cmpIgnored,"pl")
	  case SOFT => new Adjective(ind,cmp+"sz","naj"+cmp+"sz",
	                             PLAdverb.word(advInd, advCmp, advMode,cmpIgnored),
	                             softModeMap,hardModeMap,cmpIgnored,"pl")
	}
	
	def participle(ind:String) = new Adjective(ind,null,null,null,hardModeMap,null,true,"pl")
	
	def template(mode: PLMode.Value) = mode match {
	  case HARD => new AdjectiveTemplate("pl",HARD,"sz",MASCULINE_HARD.template, FEMININE_HARD.template, NEUTER_HARD.template, 
	      PLURAL_MASCULINE_HARD.template, PLURAL_NONMASCULINE_HARD.template)
	  case SOFT => new AdjectiveTemplate("pl",SOFT,"sz",MASCULINE_SOFT.template, FEMININE_SOFT.template, NEUTER_SOFT.template, 
	      PLURAL_MASCULINE_SOFT.template, PLURAL_NONMASCULINE_SOFT.template)
	}
	
}