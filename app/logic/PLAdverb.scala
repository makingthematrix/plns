package logic

import models.AdverbTemplate

import PLMode._

object PLAdverb {
  def word(root: String, mode: PLMode.Value, cmpIgnored: Boolean): Adverb = word(root,root,mode,cmpIgnored);
	
  def word(ind: String, cmp: String, mode: PLMode.Value, cmpIgnored: Boolean): Adverb = mode match {
	case HARD => new Adverb(ind+"o",cmp+"ej","naj"+cmp+"ej",cmpIgnored,"pl");
	case SOFT => new Adverb(ind+"e",cmp+"ej","naj"+cmp+"ej",cmpIgnored,"pl");
  }
  
  def template(mode: PLMode.Value) = mode match {
    case HARD => new AdverbTemplate("pl",HARD,"o","ej");
    case SOFT => new AdverbTemplate("pl",SOFT,"e","ej");
  }
}