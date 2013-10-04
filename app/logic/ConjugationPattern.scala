package logic

import models.ConjugationTemplate

object Conj extends Enumeration {
  type Conj = Value
  val INF, // pracow|ać
	PRES1S, PRES2S, PRES3S, // pracuj|ę, pracuj|esz, pracuj|e 
	PRES1P, PRES2P, PRES3P, // pracuj|emy, pracuj|ecie, prac|ują
	PAST1SM, PAST1SF, PAST2SM, PAST2SF, PAST3SM, PAST3SF, PAST3SN, // pracowałem, pracowałam, pracowałeś, pracowałaś, pracował, pracowała, pracowało
	PAST1PM, PAST1PF, PAST2PM, PAST2PF, PAST3PM, PAST3PF, // pracowaliśmy, pracowałyśmy, pracowaliście, pracowałyście, pracowali, pracowały
	PASTIMP, // pracowano
	IMP2S, IMP1P, IMP2P, // pracuj, pracuj|my, pracuj|cie
	COND1SM, COND1SF, COND2SM, COND2SF, COND3SM, COND3SF, COND3SN, // pracowałbym, pracowałabym, pracowałbyś, pracowałabyś, pracowałby, pracowałaby, pracowałoby
	COND1PM, COND1PF, COND2PM, COND2PF, COND3PM, COND3PF, // pracowalibyśmy, pracowałybyśmy, pracowalibyście, pracowałybyście, pracowaliby, pracowałyby
	ACTIVE, // pracuj|ąc 
	PASSIVE, // pracow|an 
	PERFECT, // pracow|awszy
	NOUN // pracow|anie
	 = Value
	
  implicit def toString(v: Conj.Value) = v.toString()
  
  implicit def parse(str: String) = str.toLowerCase() match {
    case "inf" => INF
	case "pres1s" => PRES1S 
	case "pres2s" => PRES2S 
	case "pres3s" => PRES3S 
	case "pres1p" => PRES1P 
	case "pres2p" => PRES2P 
	case "pres3p" => PRES3P
	case "past1sm" => PAST1SM 
	case "past1sf" => PAST1SF 
	case "past2sm" => PAST2SM 
	case "past2sf" => PAST2SF 
	case "past3sm" => PAST3SM 
	case "past3sf" => PAST3SF 
	case "past3sn" => PAST3SN 
	case "past1pm" => PAST1PM 
	case "past1pf" => PAST1PF 
	case "past2pm" => PAST2PM 
	case "past2pf" => PAST2PF 
	case "past3pm" => PAST3PM 
	case "past3pf" => PAST3PF 
	case "cond1sm" => COND1SM 
	case "cond1sf" => COND1SF 
	case "cond2sm" => COND2SM 
	case "cond2sf" => COND2SF 
	case "cond3sm" => COND3SM 
	case "cond3sf" => COND3SF 
	case "cond3sn" => COND3SN 
	case "cond1pm" => COND1PM 
	case "cond1pf" => COND1PF 
	case "cond2pm" => COND2PM 
	case "cond2pf" => COND2PF 
	case "cond3pm" => COND3PM 
	case "cond3pf" => COND3PF 
	case "imp2s" => IMP2S 
	case "imp1p" => IMP1P 
	case "imp2p" => IMP2P
	case "active" => ACTIVE 
	case "passive" => PASSIVE 
	case "perfect" => PERFECT
	case "noun" => NOUN 
    case _ => throw new IllegalArgumentException("Unrecognized conjugation mode: " + str)
  }
}

import Conj._;

abstract class ConjugationPattern(val lang: String, val id: String, val example: String) {
  def suffices:Map[Conj.Value,String]
  def adjParticiple(word: String):Adjective
  def nounParticiple(word: String):Noun
	
  def conjugate(root: String, cases: Set[Conj.Value]): Map[Conj.Value,String] = cases.map(c => (c -> conjugate(root,c))).toMap
  
  def conjugate(root: String, c: Conj.Value) = suffices.get(c) match {
    case Some(suffix) => "" + root + suffix
    case None => throw new IllegalArgumentException("The case " + c + " does not exist in the conjugation")
  }
  
  implicit def template():ConjugationTemplate = ConjugationTemplate(lang,id,example,suffices);
}