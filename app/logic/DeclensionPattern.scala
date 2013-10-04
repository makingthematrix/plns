package logic

import models.DeclensionTemplate

object Decl extends Enumeration {
  type Decl = Value
  val NOMS,GENS,DATS,ACCS,VOCS,LOCS,INSS,NOMP,GENP,DATP,ACCP,VOCP,LOCP,INSP = Value
  
  implicit def toString(v: Decl.Value) = v.toString()
  
  implicit def parse(str: String) = str.toLowerCase() match {
    case "noms" => NOMS
    case "gens" => GENS
    case "dats" => DATS
    case "accs" => ACCS
    case "vocs" => VOCS
    case "locs" => LOCS
    case "inss" => INSS
    case "nomp" => NOMP
    case "genp" => GENP
    case "datp" => DATP
    case "accp" => ACCP
    case "vocp" => VOCP
    case "locp" => LOCP
    case "insp" => INSP
    case _ => throw new IllegalArgumentException("Unrecognized declension mode: " + str)
  }
}

import Decl._

abstract class DeclensionPattern(val lang: String, val id: String, val example: String) {
  def suffices:Map[Decl.Value,String]

  /** @todo: remove after Adjective refactoring */
  def decline(root: String): Map[Decl.Value,String] = suffices.mapValues(root+_);

  def decline(stem: String, cases: Seq[Decl.Value]): Map[Decl.Value,String] = 
    cases.map(d => (d -> decline(stem,d))).toMap
    
  def decline(stem: String, d: Decl.Value): String = suffices.get(d) match {
    case Some(suffix) => stem + suffix
    case None => throw new IllegalArgumentException("The case " + d + " does not exist in the declension")
  }   
  
  override def toString = "(" + lang + "," + id + "," + example + ")"
  
  implicit def template():DeclensionTemplate = DeclensionTemplate(lang,id,example,suffices);  
}
