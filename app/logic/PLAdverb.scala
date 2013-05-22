package logic

import models.AdverbTemplate

import PlMode._

object PLAdverb {
  def word(root: String, mode: PlMode.Value, cmpIgnored: Boolean): Adverb = word(root,root,mode,cmpIgnored);
	
  def word(ind: String, cmp: String, mode: PlMode.Value, cmpIgnored: Boolean): Adverb = mode match {
	case HARD => new Adverb(ind+"o",cmp+"ej","naj"+cmp+"ej",cmpIgnored);
	case SOFT => new Adverb(ind+"e",cmp+"ej","naj"+cmp+"ej",cmpIgnored);
  }
  
  def template(mode: PlMode.Value) = mode match {
    case HARD => new AdverbTemplate("pl",HARD,"o","ej");
    case SOFT => new AdverbTemplate("pl",SOFT,"e","ej");
  }
}