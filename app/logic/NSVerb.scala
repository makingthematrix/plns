package logic

import Conj._;

object NSVerb extends VerbGenerator("ns"){
	val HARD = new ConjugationPattern("ns","HARD","imati"){ // imati
	  override def suffices() = Map(
	    INF -> "ti", // ima|ti 
	    NOUN -> "nije", // ima|nije
	    PRES1S -> "u", PRES2S -> "eš", PRES3S -> "e", // imaj|u, imaješ, imaje 
	    PRES1P -> "eme", PRES2P -> "ete", PRES3P -> "ut", // imaj|emy, imajete, imajut
	    PAST1SM -> "l", PAST1SF -> "la", PAST2SM -> "l", PAST2SF -> "la", 
	    PAST3SM -> "l", PAST3SF -> "la", PAST3SN -> "lo", // imal, imala, imalo
	    PAST1PM -> "li", PAST1PF -> "li", PAST2PM -> "li", PAST2PF -> "li", 
	    PAST3PM -> "li", PAST3PF -> "li", // ima|li
	    PASTIMP -> "no", // ima|no
	    ACTIVE -> "uč", PASSIVE -> "n", // imaj|uč, ima|n
	    IMP2S -> "", IMP1P -> "me", IMP2P -> "te", // imaj, imaj|me, imaj|te,
	    COND1SM -> "l byh", COND1SF -> "la bym", COND2SM -> "l bys", COND2SF -> "la bys", 
	    COND3SM -> "l by", COND3SF -> "la by", COND3SN -> "lo by", 
	    COND1PM -> "li byhom", COND1PF -> "li byhom", COND2PM -> "li byste", COND2PF -> "li byste", 
	    COND3PM -> "li by", COND3PF -> "li by" 
	  );
	  
	  patternMap.put(this.id, this);
	}
	// ima, imaj
	
	val SOFT = new ConjugationPattern("ns","SOFT","variti"){ // variti
	  override def suffices() = Map(
	    INF -> "ti", // vari|ti 
	    NOUN -> "enije", // vari|enije
	    PRES1S -> "ju", PRES2S -> "iš", PRES3S -> "i", // var|u, variš, vari 
	    PRES1P -> "ime", PRES2P -> "ite", PRES3P -> "jut", // var|iemy, varite, varjut
	    PAST1SM -> "l", PAST1SF -> "la", PAST2SM -> "l", PAST2SF -> "la", 
	    PAST3SM -> "l", PAST3SF -> "la", PAST3SN -> "lo", // vari|l, varila, varilo
	    PAST1PM -> "li", PAST1PF -> "li", PAST2PM -> "li", PAST2PF -> "li", 
	    PAST3PM -> "li", PAST3PF -> "li", // vari|li
	    PASTIMP -> "no", // vari|no
	    ACTIVE -> "juč", PASSIVE -> "en", // var|juč, varien
	    IMP2S -> "", IMP1P -> "me", IMP2P -> "te", // var, var|me, var|te
	    COND1SM -> "l byh", COND1SF -> "la bym", COND2SM -> "l bys", COND2SF -> "la bys", 
	    COND3SM -> "l by", COND3SF -> "la by", COND3SN -> "lo by", 
	    COND1PM -> "li byhom", COND1PF -> "li byhom", COND2PM -> "li byste", COND2PF -> "li byste", 
	    COND3PM -> "li by", COND3PF -> "li by" 
	  );
	  
	  patternMap.put(this.id, this);
	}
	// vari, var
}