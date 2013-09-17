package logic

import Conj._;

abstract class PLConjugationPattern(id: String, example: String) extends ConjugationPattern("ns",id,example){
  private def conjugateConditional(root: String, c: Conj.Value) = {
    println("PLConjugationPattern.conjugateConditional, case is " + c + ", lang is " + lang)
    val conditional = PLVerb.getCopula(c)
    println("conditional is " + conditional)
    val pastC = Verb.cond2Past(c)
    println("past case is " + pastC)
    val condRoot = super.conjugate(root, pastC)
    println("cond. root is " + condRoot)
    condRoot + conditional
  }

  override def conjugate(root: String, c: Conj.Value):String = {
    if(Verb.condConjCases.contains(c)) conjugateConditional(root,c)
    else super.conjugate(root, c)
  }
  
  override def adjParticiple(word: String) = PLAdjective.participle(word)
  override def nounParticiple(word: String) = PLNoun.participle(word)
}

object PLVerb extends VerbGenerator("pl") {
  	val I = new PLConjugationPattern("I","czytać"){ // czyt|ać
	  override def suffices() = Map(
	      INF -> "ać", // czyt|ać 
	      NOUN -> "anie", // czyt|anie
	      PRES1S -> "am", PRES2S -> "asz", PRES3S -> "a", // czyt|am, czytasz, czyta 
	      PRES1P -> "amy", PRES2P -> "acie", PRES3P -> "ają", // czytamy, czytacie, czytają
	      PAST1SM -> "ałem", PAST1SF -> "ałam", PAST2SM -> "ałeś", PAST2SF -> "ałaś", 
	      PAST3SM -> "ał", PAST3SF -> "ała", PAST3SN -> "ało", 
	      PAST1PM -> "aliśmy", PAST1PF -> "ałyśmy", PAST2PM -> "aliście", PAST2PF -> "ałyście", 
	      PAST3PM -> "ali", PAST3PF -> "ały", 
	      PASTIMP -> "ano",
	      ACTIVE -> "ając", PASSIVE -> "an", PERFECT -> "awszy", // czyt|ając|y, czyt|an|y, czyt|awszy
	      IMP2S -> "aj", IMP1P -> "ajmy", IMP2P -> "ajcie" // czyt|aj, czyt|ajmy, czyt|ajcie
	  );
	  
	  patternMap.put(this.id, this);
	}
	// czyt, czyt
  	
  	val II = new PLConjugationPattern("II","słyszeć"){ // słysz|eć
	  override def suffices() = Map(
	      INF -> "eć", // słysz|eć 
	      NOUN -> "enie", // słysz|enie
	      PRES1S -> "ę", PRES2S -> "ysz", PRES3S -> "y", // słysz|ę, wisi|sz, wisi| 
	      PRES1P -> "ymy", PRES2P -> "ycie", PRES3P -> "ą", // słysz|ymy, wisi|cie, wisz|ą
	      PAST1SM -> "ałem", PAST1SF -> "ałam", PAST2SM -> "ałeś", PAST2SF -> "ałaś", 
	      PAST3SM -> "ał", PAST3SF -> "ała", PAST3SN -> "ało", 
	      PAST1PM -> "eliśmy", PAST1PF -> "ałyśmy", PAST2PM -> "eliście", PAST2PF -> "ałyście", 
	      PAST3PM -> "eli", PAST3PF -> "ały", 
	      PASTIMP -> "ano",
	      ACTIVE -> "ąc", PASSIVE -> "an", PERFECT -> "awszy",
	      IMP2S -> "", IMP1P -> "my", IMP2P -> "cie"
	  );	 
	  
	  patternMap.put(this.id, this);
	}
	// słysz, słysz
	

	val IV = new PLConjugationPattern("IV","malować"){ // malow|a|ć
	  override def suffices() = Map(
	      INF -> "ać", // malow|a|ć 
	      NOUN -> "anie", // malow|a|nie
	      PRES1S -> "ę", PRES2S -> "esz", PRES3S -> "e", // ciągn|ę, ciągn|e|sz, ciągn|e 
	      PRES1P -> "emy", PRES2P -> "ecie", PRES3P -> "ą", // ciągn|e|my, ciągn|e|cie, ciągn|ą
	      PAST1SM -> "ałem", PAST1SF -> "ałam", PAST2SM -> "ałeś", PAST2SF -> "ałaś", 
	      PAST3SM -> "ał", PAST3SF -> "ała", PAST3SN -> "ało", // malow|ałem, malowałam, malowałeś, malowałaś, malował, malowała, malowało
	      PAST1PM -> "aliśmy", PAST1PF -> "ałyśmy", PAST2PM -> "aliście", PAST2PF -> "ałyście", 
	      PAST3PM -> "ali", PAST3PF -> "ały", // malow|aliśmy, malowałyśmy, malowaliście, malowałyście, malowali, malowały
	      PASTIMP -> "ano",
	      ACTIVE -> "ąc", PASSIVE -> "an", PERFECT -> "awszy", // ciągn|ąc, malow|an
	      IMP2S -> "", IMP1P -> "my", IMP2P -> "cie" // ciągn, ciągn|my, ciągn|cie
	  );
	  
	  patternMap.put(this.id, this);
	}
	// malow, maluj
	
	val Va = new PLConjugationPattern("Va","ciągnać"){ // ciąg|nąć
	  override def suffices() = Map(
	      INF -> "nąć", // ciąg|nąć 
	      NOUN -> "nięcie", // ciąg|nięcie
	      PRES1S -> "nę", PRES2S -> "niesz", PRES3S -> "nie", // ciąg|nę, ciąg|niesz, ciąg|nie 
	      PRES1P -> "niemy", PRES2P -> "niecie", PRES3P -> "ną", // ciąg|niemy, ciąg|niecie, ciąg|ną
	      PAST1SM -> "nąłem", PAST1SF -> "nęłam", PAST2SM -> "nąłeś", PAST2SF -> "nęłaś", 
	      PAST3SM -> "nął", PAST3SF -> "nęła", PAST3SN -> "nęło", // ciąg|nąłem, ciąg|nęłam, ciąg|nąłeś, ciąg|nęłaś, ciąg|nął, ciąg|nęła, ciąg|nęło
	      PAST1PM -> "nęliśmy", PAST1PF -> "nęłyśmy", PAST2PM -> "nęliście", PAST2PF -> "nęłyście", 
	      PAST3PM -> "nęli", PAST3PF -> "nęły", // ciąg|nęliśmy, ciąg|nęłyśmy, ciąg|nęliście, ciąg|nęłyście, ciąg|nęli, ciąg|nęły
	      PASTIMP -> "niono", // ciąg|niono
	      ACTIVE -> "nąc", PASSIVE -> "nięt", PERFECT -> "nąwszy", // ciąg|nąc, ciąg|nięt(y/a/o)
	      IMP2S -> "nij", IMP1P -> "nijmy", IMP2P -> "nijcie" // ciąg|nij, ciąg|nijmy, ciąg|nijcie
	  );
	  
	  patternMap.put(this.id, this);
	}
	// ciąg, ciąg

	val Vb = new PLConjugationPattern("Vb","płynąć"){ // pły|nąć
	  override def suffices() = Map(
	      INF -> "nąć", // pły|nąć 
	      NOUN -> "nięcie", // pły|nięcie
	      PRES1S -> "nę", PRES2S -> "niesz", PRES3S -> "nie", // pły|nę, pły|niesz, pły|nie 
	      PRES1P -> "niemy", PRES2P -> "niecie", PRES3P -> "ną", // pły|niemy, pły|niecie, pły|ną
	      PAST1SM -> "nąłem", PAST1SF -> "nęłam", PAST2SM -> "nąłeś", PAST2SF -> "nęłaś", 
	      PAST3SM -> "nął", PAST3SF -> "nęła", PAST3SN -> "nęło", // pły|nąłem, pły|nęłam, pły|nąłeś, pły|nęłaś, pły|nął, pły|nęła, pły|nęło
	      PAST1PM -> "nęliśmy", PAST1PF -> "nęłyśmy", PAST2PM -> "nęliście", PAST2PF -> "nęłyście", 
	      PAST3PM -> "nęli", PAST3PF -> "nęły", // pły|nęliśmy, pły|nęłyśmy, pły|nęliście, pły|nęłyście, pły|nęli, pły|nęły
	      PASTIMP -> "nięto", // pły|nięto
	      ACTIVE -> "nąc", PASSIVE -> "nięt", PERFECT -> "nąwszy", // pły|nąc, pły|nięt(y/a/o)
	      IMP2S -> "ń", IMP1P -> "ńmy", IMP2P -> "ńcie" // pły|ń, pły|ńmy, pły|ńcie
	  );
	  
	  patternMap.put(this.id, this);
	}
	// pły, pły

	val Vc = new PLConjugationPattern("Vc","chudnąć"){ // chud|nąć
	  override def suffices() = Map(
	      INF -> "nąć", // chud|nąć 
	      NOUN -> "nięcie",  // chud|nięcie
	      PRES1S -> "nę", PRES2S -> "niesz", PRES3S -> "nie", // chud|nę, chud|niesz, chud|nie 
	      PRES1P -> "niemy", PRES2P -> "niecie", PRES3P -> "ną", // chud|niemy, chud|niecie, chud|ną
	      PAST1SM -> "łem", PAST1SF -> "łam", PAST2SM -> "łeś", PAST2SF -> "łaś", 
	      PAST3SM -> "ł", PAST3SF -> "ła", PAST3SN -> "ło", // chud|łem, chud|łam, chud|łeś, chud|łaś, chud|ł, chud|ła, chud|ło
	      PAST1PM -> "liśmy", PAST1PF -> "łyśmy", PAST2PM -> "liście", PAST2PF -> "łyście", 
	      PAST3PM -> "li", PAST3PF -> "ły", // chud|liśmy, chud|łyśmy, chud|liście, chud|łyście, chud|li, chud|ły
	      PASTIMP -> "nięto", // chud|nięto
	      ACTIVE -> "nąc", PASSIVE -> "nięt", PERFECT -> "nąwszy", // chud|nąc, chud|nięt(y/a/o)
	      IMP2S -> "nij", IMP1P -> "nijmy", IMP2P -> "nijcie" // chud|nij, chud|nijmy, chud|nijcie
	  );
	  
	  patternMap.put(this.id, this);
	}
	// chud, chud
	
	val VIa = new PLConjugationPattern("VIa","robić"){ // rob|ić
	  override def suffices() = Map(
	      INF -> "ić", // rob|ić 
	      NOUN -> "ienie", // rob|ienie
	      PRES1S -> "ię", PRES2S -> "isz", PRES3S -> "i", // rob|ię, robisz, robi 
	      PRES1P -> "imy", PRES2P -> "icie", PRES3P -> "ią", // rob|imy, robicie, robią
	      PAST1SM -> "iłem", PAST1SF -> "iłam", PAST2SM -> "iłeś", PAST2SF -> "iłaś", 
	      PAST3SM -> "ił", PAST3SF -> "iła", PAST3SN -> "iło", // rob|iłem, robiłam, robiłeś, robiłaś, robił, robiła, robiło
	      PAST1PM -> "iliśmy", PAST1PF -> "iłyśmy", PAST2PM -> "iliście", PAST2PF -> "iłyście", 
	      PAST3PM -> "ili", PAST3PF -> "iły", // rob|iliśmy, robiłyśmy, robiliście, robiłyście, robili, robiły
	      PASTIMP -> "iono", // rob|iono
	      ACTIVE -> "iąc", PASSIVE -> "ion", PERFECT -> "iwszy", // rob|iąc, rob|ion
	      IMP2S -> "", IMP1P -> "my", IMP2P -> "cie" // rób, rób|my, rób|cie
	  );
	  
	  patternMap.put(this.id, this);
	}
	// rob, rob
	
	val VIb = new PLConjugationPattern("VIb","wierzyć"){ // wierz|yć
	  override def suffices() = Map(
	      INF -> "yć", // wierz|yć 
	      NOUN -> "enie", // wierz|enie
	      PRES1S -> "ę", PRES2S -> "ysz", PRES3S -> "y", // wierz|ę, wierzysz, wierzy
	      PRES1P -> "ymy", PRES2P -> "ycie", PRES3P -> "ą", // wierz|ymy, wierzycie, wierzą
	      PAST1SM -> "yłem", PAST1SF -> "yłam", PAST2SM -> "yłeś", PAST2SF -> "yłaś", 
	      PAST3SM -> "ył", PAST3SF -> "yła", PAST3SN -> "yło", // wierz|yłem, wierzyłam, wierzyłeś, wierzyłaś, wierzył, wierzyła, wierzyło
	      PAST1PM -> "yliśmy", PAST1PF -> "yłyśmy", PAST2PM -> "yliście", PAST2PF -> "yłyście", 
	      PAST3PM -> "yli", PAST3PF -> "yły", // wierz|iliśmy, wierziłyśmy, wierziliście, wierziłyście, wierzili, wierziły
	      PASTIMP -> "ono", // wierz|iono
	      ACTIVE -> "ąc", PASSIVE -> "on", PERFECT -> "ywszy", // wierz|ąc, wierz|on
	      IMP2S -> "", IMP1P -> "my", IMP2P -> "cie" // wierz, wierz|my, wierz|cie
	  );
	  
	  patternMap.put(this.id, this);
	}
	// wierz, wierz
	
	val VIIa = new PLConjugationPattern("VIIa","widzieć"){ // widz|ieć
	  override def suffices() = Map(
	      INF -> "ieć", // widz|ieć 
	      NOUN -> "enie", // widz|enie
	      PRES1S -> "ę", PRES2S -> "isz", PRES3S -> "i", // widz|i, widz|isz, widz|i 
	      PRES1P -> "imy", PRES2P -> "icie", PRES3P -> "ą", // widz|imy, widz|icie, widz|ą
	      PAST1SM -> "iałem", PAST1SF -> "iałam", PAST2SM -> "iałeś", PAST2SF -> "iałaś", 
	      PAST3SM -> "iał", PAST3SF -> "iała", PAST3SN -> "iało", 
	      PAST1PM -> "ieliśmy", PAST1PF -> "iałyśmy", PAST2PM -> "ieliście", PAST2PF -> "iałyście", 
	      PAST3PM -> "ieli", PAST3PF -> "iały", 
	      PASTIMP -> "iano",
	      ACTIVE -> "ąc", PASSIVE -> "ian", PERFECT -> "iawszy", 
	      IMP2S -> "", IMP1P -> "my", IMP2P -> "cie"
	  );	 
	  
	  patternMap.put(this.id, this);
	}
	// widz, widz
	
	val VIIb = new PLConjugationPattern("VIIb","leżeć"){ // leż|eć
	  override def suffices() = Map(
	      INF -> "eć", // leż|eć 
	      NOUN -> "enie", // leż|enie
	      PRES1S -> "ę", PRES2S -> "ysz", PRES3S -> "y", // leż|ę, leż|ysz, leż|y 
	      PRES1P -> "ymy", PRES2P -> "ycie", PRES3P -> "ą", // leż|ymy, leż|ycie, leż|ą
	      PAST1SM -> "ałem", PAST1SF -> "ałam", PAST2SM -> "iałeś", PAST2SF -> "iałaś", 
	      PAST3SM -> "ał", PAST3SF -> "ała", PAST3SN -> "ało", 
	      PAST1PM -> "eliśmy", PAST1PF -> "ałyśmy", PAST2PM -> "eliście", PAST2PF -> "ałyście", 
	      PAST3PM -> "eli", PAST3PF -> "ały", 
	      PASTIMP -> "ano",
	      ACTIVE -> "ąc", PASSIVE -> "an", PERFECT -> "awszy",
	      IMP2S -> "", IMP1P -> "my", IMP2P -> "cie"
	  );	 
	  
	  patternMap.put(this.id, this);
	}
	// leż, leż
	
	val Xa = new PLConjugationPattern("Xa","pić"){ // pi|ć
	  override def suffices() = Map(
	      INF -> "ć", // pi|ć 
	      NOUN -> "cie", // pi|cie
	      PRES1S -> "ję", PRES2S -> "jesz", PRES3S -> "je", // pi|ję, pijesz, pije 
	      PRES1P -> "jemy", PRES2P -> "jecie", PRES3P -> "ją", // pi|jemy, pi|jecie, pi|ją
	      PAST1SM -> "łem", PAST1SF -> "łam", PAST2SM -> "łeś", PAST2SF -> "łaś", 
	      PAST3SM -> "ł", PAST3SF -> "ła", PAST3SN -> "ło", // pi|łem, pi|łam, pi|łeś, pi|łaś, pi|ł, pi|ła, pi|ło
	      PAST1PM -> "liśmy", PAST1PF -> "łyśmy", PAST2PM -> "liście", PAST2PF -> "łyście", 
	      PAST3PM -> "li", PAST3PF -> "ły", // pi|liśmy, pi|łyśmy, pi|liście, pi|łyście, pi|li, pi|ły
	      PASTIMP -> "to", // pi|to
	      ACTIVE -> "jąc", PASSIVE -> "t", PERFECT -> "ywszy", // pi|jąc, pi|t
	      IMP2S -> "j", IMP1P -> "jmy", IMP2P -> "jcie" // pi|j, pi|jmy, pi|jcie
	  );
	  
	  patternMap.put(this.id, this);
	}
	// pi, pi
		 
	val Xb = new PLConjugationPattern("Xb","lać"){ // la|ć
	  override def suffices() = Map(
	      INF -> "ć", // la|ć 
	      NOUN -> "nie", // la|nie
	      PRES1S -> "ję", PRES2S -> "jesz", PRES3S -> "je", // le|ję, lejesz, leje 
	      PRES1P -> "jemy", PRES2P -> "jecie", PRES3P -> "ją", // le|jemy, le|jecie, le|ją
	      PAST1SM -> "łem", PAST1SF -> "łam", PAST2SM -> "łeś", PAST2SF -> "łaś", 
	      PAST3SM -> "ł", PAST3SF -> "ła", PAST3SN -> "ło", // la|łem, la|łam, la|łeś, la|łaś, la|ł, la|ła, la|ło
	      PAST1PM -> "liśmy", PAST1PF -> "łyśmy", PAST2PM -> "liście", PAST2PF -> "łyście", 
	      PAST3PM -> "li", PAST3PF -> "ły", // la|liśmy, la|łyśmy, la|liście, la|łyście, la|li, la|ły
	      PASTIMP -> "no", // la|no
	      ACTIVE -> "jąc", PASSIVE -> "n", PERFECT -> "awszy", // le|jąc, la|n
	      IMP2S -> "j", IMP1P -> "jmy", IMP2P -> "jcie" // le|j, le|jmy, le|jcie
	  );
	  
	  patternMap.put(this.id, this);
	}
	// la, le
	
	val Xc = new PLConjugationPattern("Xc","wziąć"){ // wzi|ąć
	  override def suffices() = Map(
	      INF -> "ąć", // wzi|ąć 
	      NOUN -> "ęcie", // wzi|ęcie
	      PRES1S -> "mę", PRES2S -> "miesz", PRES3S -> "mie", // wez|mę, weźmiesz, weźmie 
	      PRES1P -> "miemy", PRES2P -> "miecie", PRES3P -> "mą", // weź|miemy, weź|miecie, wez|mą
	      PAST1SM -> "ąłem", PAST1SF -> "ęłam", PAST2SM -> "ąłeś", PAST2SF -> "ęłaś", 
	      PAST3SM -> "ął", PAST3SF -> "ęła", PAST3SN -> "ęło", // wzi|ąłem, wzi|ęłam, wzi|ąłeś, wzi|ęłaś, wzi|ął, wzi|ęła, wzi|ęło
	      PAST1PM -> "ęliśmy", PAST1PF -> "ęłyśmy", PAST2PM -> "ęliście", PAST2PF -> "ęłyście", 
	      PAST3PM -> "ęli", PAST3PF -> "ęły", // wzi|ęliśmy, wzi|ęłyśmy, wzi|ęliście, wzi|ęłyście, wzi|ęli, wzi|ęły
	      PASTIMP -> "ęto", // wzi|ęto
	      ACTIVE -> "mąc", PASSIVE -> "ęt", PERFECT -> "ąwszy",// weź|mąc, wzi|ęt
	      IMP2S -> "", IMP1P -> "my", IMP2P -> "cie" // weź|, weź|my, weź|cie
	  );
	  
	  patternMap.put(this.id, this);
	}
	// wzi, weź
	
	val XI = new PLConjugationPattern("Xc","wieźć"){ // wieź|ć
	  override def suffices() = Map(
	      INF -> "ć", // wieź|ć 
	      NOUN -> "enie", // wiezi|enie
	      PRES1S -> "ę", PRES2S -> "iesz", PRES3S -> "ie", // wioz|ę, wieziesz, wiezie 
	      PRES1P -> "emy", PRES2P -> "ecie", PRES3P -> "ą", // wiezi|emy, wiezi|ecie, wioz|ą
	      PAST1SM -> "łem", PAST1SF -> "łam", PAST2SM -> "łeś", PAST2SF -> "łaś", 
	      PAST3SM -> "ł", PAST3SF -> "ła", PAST3SN -> "ło", // wioz|łem, wioz|łam, wioz|łeś, wioz|łaś, wióz|ł, wioz|ła, wioz|ło
	      PAST1PM -> "liśmy", PAST1PF -> "łyśmy", PAST2PM -> "liście", PAST2PF -> "łyście", 
	      PAST3PM -> "li", PAST3PF -> "ły", // wieź|liśmy, wioz|łyśmy, wieź|liście, wioz|łyście, wieź|li, wioz|ły
	      PASTIMP -> "ono", // wiezi|ono
	      ACTIVE -> "ąc", PASSIVE -> "on", PERFECT -> "ąwszy", // wioz|ąc, wioz|on
	      IMP2S -> "", IMP1P -> "my", IMP2P -> "cie" // wieź|, wieź|my, wieź|cie
	  );
	  
	  patternMap.put(this.id, this);
	}
	// wioz, wieź
	
	override def getCopula(c: Conj.Value) = c match {
	  case PRES1S => "jestem"
	  case PRES2S => "jesteś"
	  case PRES3S => "jest"
	  case PRES1P => "jesteśmy"
	  case PRES2P => "jesteście"
	  case PRES3P => "są"
	  case PAST1SM => "byłem"
	  case PAST1SF => "byłam"
	  case PAST2SM => "byłeś"
	  case PAST2SF => "byłaś"
	  case PAST3SM => "był"
	  case PAST3SF => "była"
	  case PAST3SN => "było"
	  case PAST1PM => "byliśmy"
	  case PAST1PF => "byłyśmy"
	  case PAST2PM => "byliście"
	  case PAST2PF => "byłyście"
	  case PAST3PM => "byli"
	  case PAST3PF => "były"
	  case PASTIMP => "byto"
	  case IMP2S => "bądź"
	  case IMP1P => "bądźmy" 
	  case IMP2P => "bądźcie"
	  case COND1SM => "bym"
	  case COND1SF => "bym"
	  case COND2SM => "byś"
	  case COND2SF => "byś"
	  case COND3SM => "by"
	  case COND3SF => "by"
	  case COND3SN => "by"
	  case COND1PM => "byśmy"
	  case COND1PF => "byśmy"
	  case COND2PM => "byście"
	  case COND2PF => "byście"
	  case COND3PM => "by"
	  case COND3PF => "by"
	  case ACTIVE => "będąc" 
	  case PASSIVE => "byt"
	  case NOUN => "bycie"
	}
	
	override def getCopulaFuture(c: Conj.Value) = c match {
	  case PRES1S => "będę"
	  case PRES2S => "będziesz"
	  case PRES3S => "będzie"
	  case PRES1P => "będziemy"
	  case PRES2P => "będziecie"
	  case PRES3P => "będą"
	}
	
	val ppRootEndings = Set()
}