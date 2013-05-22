package logic

import models.ConjugationTemplate

object Conj extends Enumeration {
  type Conj = Value;
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
	NOUN // pracow|anie
	 = Value;
	
  implicit def toString(v: Conj.Value):String = v.toString()
  
  implicit def parse(str: String): Conj.Value = str.toLowerCase() match {
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
	case "imp2s" => IMP2S 
	case "imp1p" => IMP1P 
	case "imp2p" => IMP2P
	case "active" => ACTIVE 
	case "passive" => PASSIVE 
	case "noun" => NOUN 
    case _ => throw new IllegalArgumentException("Unrecognized conjugation mode: " + str)
  }
}

import Conj._;

abstract class ConjugationPattern(val lang: String, val id: String, val example: String) {
  def suffices:Map[Conj.Value,String]
	
  def conjugate(root: String): Map[Conj.Value,String] = suffices.mapValues(root+_);
	
  def conjugate(root: String, cases: Seq[Conj.Value]): Map[Conj.Value,String] = 
	suffices.filter(t => cases.contains(t._1)).mapValues(root+_);
	
  implicit def template():ConjugationTemplate = ConjugationTemplate(lang,id,example,suffices);
	
}