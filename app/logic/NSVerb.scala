package logic

import Conj._;

abstract class NSConjugationPattern(id: String, example: String) extends ConjugationPattern("ns",id,example){
  private def conjugateConditional(root: String, c: Conj.Value) = {
    println("NSConjugationPattern.conjugateConditional, case is " + c + ", lang is " + lang)
    val conditional = NSVerb.getCopula(c)
    println("conditional is " + conditional)
    val pastC = Verb.cond2Past(c)
    println("past case is " + pastC)
    val condRoot = super.conjugate(root, pastC)
    println("cond. root is " + condRoot)
    condRoot + " " + conditional
  }
  
  private def conjugatePast(root: String, c: Conj.Value) = {
    val pastRoot = super.conjugate(root, c)
    val presentC = Verb.past2Present(c)
    val copula = NSVerb.getCopula(presentC)
    copula + " " + pastRoot
  }

  override def conjugate(root: String, c: Conj.Value):String = {
    if(Verb.condConj.contains(c)) conjugateConditional(root,c)
    else if(Verb.pastConj.contains(c)) conjugatePast(root,c)
    else super.conjugate(root, c)
  }
  
  override def adjParticiple(word: String) = NSAdjective.participle(word)
  override def nounParticiple(word: String) = NSNoun.participle(word)
}

object NSVerb extends VerbGenerator("ns"){
	val HARD = new NSConjugationPattern("HARD","imati"){ // imati
	  override def suffices() = Map(
	    INF -> "ti", // ima|ti 
	    NOUN -> "nje", // ima|nje
	    PRES1S -> "u", PRES2S -> "eš", PRES3S -> "e", // imaj|u, imaješ, imaje 
	    PRES1P -> "eme", PRES2P -> "ete", PRES3P -> "ut", // imaj|emy, imajete, imajut
	    PAST1SM -> "l", PAST1SF -> "la", PAST2SM -> "l", PAST2SF -> "la", 
	    PAST3SM -> "l", PAST3SF -> "la", PAST3SN -> "lo", // imal, imala, imalo
	    PAST1PM -> "li", PAST1PF -> "li", PAST2PM -> "li", PAST2PF -> "li", 
	    PAST3PM -> "li", PAST3PF -> "li", // ima|li
	    PASTIMP -> "no", // ima|no
	    ACTIVE -> "uč", PASSIVE -> "n", PERFECT -> "všy", // imaj|uč, ima|n
	    IMP2S -> "", IMP1P -> "me", IMP2P -> "te" // imaj, imaj|me, imaj|te,
	  );
	  
	  patternMap.put(this.id, this);
	}
	// ima, imaj
	
	val SOFT = new NSConjugationPattern("SOFT","variti"){ // variti
	  override def suffices() = Map(
	    INF -> "ti", // vari|ti 
	    NOUN -> "enje", // vari|enje
	    PRES1S -> "ju", PRES2S -> "iš", PRES3S -> "i", // var|u, variš, vari 
	    PRES1P -> "ime", PRES2P -> "ite", PRES3P -> "jut", // var|iemy, varite, varjut
	    PAST1SM -> "l", PAST1SF -> "la", PAST2SM -> "l", PAST2SF -> "la", 
	    PAST3SM -> "l", PAST3SF -> "la", PAST3SN -> "lo", // vari|l, varila, varilo
	    PAST1PM -> "li", PAST1PF -> "li", PAST2PM -> "li", PAST2PF -> "li", 
	    PAST3PM -> "li", PAST3PF -> "li", // vari|li
	    PASTIMP -> "no", // vari|no
	    ACTIVE -> "juč", PASSIVE -> "en", PERFECT -> "všy", // var|juč, varien
	    IMP2S -> "", IMP1P -> "me", IMP2P -> "te" // var, var|me, var|te  
	  );
	  
	  patternMap.put(this.id, this);
	}
	// vari, var
	
	override def getCopula(c: Conj.Value) = c match {
	  case PRES1S => "jesm"
	  case PRES2S => "jesi"
	  case PRES3S => "je"
	  case PRES1P => "jesme"
	  case PRES2P => "jeste"
	  case PRES3P => "sut"
	  case PAST1SM => "byl"
	  case PAST1SF => "byla"
	  case PAST2SM => "byl"
	  case PAST2SF => "byla"
	  case PAST3SM => "byl"
	  case PAST3SF => "byla"
	  case PAST3SN => "bylo"
	  case PAST1PM => "byli"
	  case PAST1PF => "byli"
	  case PAST2PM => "byli"
	  case PAST2PF => "byli"
	  case PAST3PM => "byli"
	  case PAST3PF => "byli"
	  case PASTIMP => "budemo"
	  case IMP2S => "budi"
	  case IMP1P => "budime" 
	  case IMP2P => "budite"
	  case COND1SM => "byh"
	  case COND1SF => "byh"
	  case COND2SM => "bys"
	  case COND2SF => "bys"
	  case COND3SM => "by"
	  case COND3SF => "by"
	  case COND3SN => "by"
	  case COND1PM => "byhom"
	  case COND1PF => "byhom"
	  case COND2PM => "byste"
	  case COND2PF => "byste"
	  case COND3PM => "by"
	  case COND3PF => "by"
	  case ACTIVE => "buduč" 
	  case PASSIVE => "budem"
	  case NOUN => "bytije"
	}
	
	override def getCopulaFuture(c: Conj.Value) = c match {
	  case PRES1S => "budu"
	  case PRES2S => "budeš"
	  case PRES3S => "bude"
	  case PRES1P => "budeme"
	  case PRES2P => "budete"
	  case PRES3P => "budut"
	}
}