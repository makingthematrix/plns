package logic

import Decl._;

object PLNoun extends NounGenerator("pl"){
	val HARD_MASCULINE_PERSON = new DeclensionPattern("pl","HARD_MASCULINE_PERSON","brat") { // "brat"
		override def suffices = Map(
		    NOMS -> "", GENS -> "a", DATS -> "u", ACCS -> "a", VOCS -> "ie", LOCS -> "ie", INSS -> "em",
		    NOMP -> "ia", GENP -> "i", DATP -> "om", ACCP -> "i", VOCP -> "ia", LOCP -> "ach", INSP -> "ami"
		);
		
		patternMap.put(this.id, this);
	}
	
	val MASCULINE_A = new DeclensionPattern("pl","MASCULINE_A","sędzia") { // "sędzia"
		override def suffices = Map(
		    NOMS -> "a", GENS -> "i", DATS -> "i", ACCS -> "ego", VOCS -> "io", LOCS -> "i", INSS -> "om",
		    NOMP -> "owie", GENP -> "ów", DATP -> "om", ACCP -> "ów", VOCP -> "owie", LOCP -> "ach", INSP -> "ami"
		);
		
		patternMap.put(this.id, this);
	}
	
	val HARD_MASCULINE_OBJECT = new DeclensionPattern("pl","HARD_MASCULINE_OBJECT","gród") { // "gród"
		override def suffices = Map(
		    NOMS -> "", GENS -> "u", DATS -> "owi", ACCS -> "", VOCS -> "ie", LOCS -> "ie", INSS -> "em",
		    NOMP -> "y", GENP -> "ów", DATP -> "om", ACCP -> "y", VOCP -> "y", LOCP -> "ach", INSP -> "ami"
		);
		
		patternMap.put(this.id, this);
	}
	// additional rule: if the Polish word ends with "i", it might be a NOMP of the hmo type
	// workaround: if no translation is found, change "i" to "y" and search again
	
	val SOFT_MASCULINE_PERSON = new DeclensionPattern("pl","SOFT_MASCULINE_PERSON","mąż") { // "mąż"
		override def suffices = Map(
		    NOMS -> "", GENS -> "a", DATS -> "owi", ACCS -> "a", VOCS -> "u", LOCS -> "u", INSS -> "em",
		    NOMP -> "owie", GENP -> "ów", DATP -> "om", ACCP -> "ów", VOCP -> "owie", LOCP -> "ach", INSP -> "ami" 
		);
		
		patternMap.put(this.id, this);
	}
	
	val SOFT_MASCULINE_OBJECT_I = new DeclensionPattern("pl","SOFT_MASCULINE_OBJECT_I","ołówek") { // "ołówek"
		override def suffices = Map(
		    NOMS -> "", GENS -> "a", DATS -> "owi", ACCS -> "", VOCS -> "u", LOCS -> "u", INSS -> "iem",
		    NOMP -> "i", GENP -> "ów", DATP -> "om", ACCP -> "i", VOCP -> "i", LOCP -> "ach", INSP -> "ami"
		);
		
		patternMap.put(this.id, this);
	}
	
	val SOFT_MASCULINE_OBJECT_E = new DeclensionPattern("pl","SOFT_MASCULINE_OBJECT_E","kraj") { // "kraj"
		override def suffices = Map(
		    NOMS -> "", GENS -> "u", DATS -> "owi", ACCS -> "", VOCS -> "u", LOCS -> "u", INSS -> "em",
		    NOMP -> "e", GENP -> "ów", DATP -> "om", ACCP -> "e", VOCP -> "e", LOCP -> "ach", INSP -> "ami"
		);
		
		patternMap.put(this.id, this);
	}
	

	// additional rule: if the Polish word ends with "y", it might be a GENP of the smo type
	// workaround: if no translation is found, change "y" to "ów" and search again
	
	val HARD_FEMININE_PERSON = new DeclensionPattern("pl","HARD_FEMININE_PERSON","żona") { // "żona"
		override def suffices = Map(
		    NOMS -> "a", GENS -> "y", DATS -> "ie", ACCS -> "ę", VOCS -> "o", LOCS -> "ie", INSS -> "ą",
		    NOMP -> "y", GENP -> "", DATP -> "om", ACCP -> "y", VOCP -> "y", LOCP -> "ach", INSP -> "ami"
		);
		
		patternMap.put(this.id, this);
	}
	
  val HARD_FEMININE_OBJECT = new DeclensionPattern("pl","HARD_FEMININE_OBJECT","teka") { // "teka"
	override def suffices = Map(
	  NOMS -> "a", GENS -> "i", DATS -> "ie", ACCS -> "ę", VOCS -> "o", LOCS -> "e", INSS -> "ą",
	  NOMP -> "i", GENP -> "", DATP -> "om", ACCP -> "i", VOCP -> "i", LOCP -> "ach", INSP -> "ami"
	);
		
	patternMap.put(this.id, this);
  }

  val SOFT_FEMININE = new DeclensionPattern("pl","SOFT_FEMININE","dusza") { // "dusza"
		override def suffices = Map(
		    NOMS -> "a", GENS -> "y", DATS -> "y", ACCS -> "ę", VOCS -> "o", LOCS -> "y", INSS -> "ą",
		    NOMP -> "e", GENP -> "", DATP -> "om", ACCP -> "e", VOCP -> "e", LOCP -> "ach", INSP -> "ami"
		);
	}
	
	val ACCORD_FEMININE = new DeclensionPattern("pl","ACCORD_FEMININE","kość") { // "kość"
		override def suffices = Map(
		    NOMS -> "", GENS -> "i", DATS -> "i", ACCS -> "", VOCS -> "i", LOCS -> "ach", INSS -> "ią",
		    NOMP -> "i", GENP -> "i", DATP -> "iom", ACCP -> "i", VOCP -> "i", LOCP -> "ach", INSP -> "ami"
		);
		
		patternMap.put(this.id, this);
	}
	
	val HARD_NEUTER = new DeclensionPattern("pl","HARD_NEUTER","koło") { // "koło"
		override def suffices = Map(
		    NOMS -> "o", GENS -> "a", DATS -> "u", ACCS -> "o", VOCS -> "e", LOCS -> "e", INSS -> "em",
		    NOMP -> "a", GENP -> "", DATP -> "om", ACCP -> "a", VOCP -> "a", LOCP -> "ach", INSP -> "ami"
		);
		
		patternMap.put(this.id, this);
	}
	
	val SOFT_NEUTER = new DeclensionPattern("pl","SOFT_NEUTER","pole") { // "pole"
		override def suffices = Map(
		    NOMS -> "e", GENS -> "a", DATS -> "u", ACCS -> "e", VOCS -> "e", LOCS -> "u", INSS -> "em",
		    NOMP -> "a", GENP -> "", DATP -> "om", ACCP -> "a", VOCP -> "a", LOCP -> "ach", INSP -> "ami"
		);
		
		patternMap.put(this.id, this);
	}
	
	def participle(noun: String) = {
	  val root = noun.substring(0, noun.length()-1)
	  val genpException = if(noun.endsWith("nie")){
	    noun.substring(0, noun.length()-3) + "ń"
	  } else if(noun.endsWith("cie")){
	    noun.substring(0, noun.length()-3) + "ć"
	  } else throw new IllegalArgumentException("The noun's suffix does not fit the Polish noun participle: " + noun)
	  
	  val participle = new Noun(root,SOFT_NEUTER,false,false,"pl")
	  participle.except(GENP, genpException)
	}
}