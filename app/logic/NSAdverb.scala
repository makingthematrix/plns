package logic

import models.AdverbTemplate

object NSAdverb {
  def word1(root: String): Adverb = new Adverb(root+"o",root+"eje","naj"+root+"eje",false,"ns");
  def word(ind: String, cmp: String, cmpIgnored: Boolean): Adverb = new Adverb(ind+"o",cmp+"eje","naj"+cmp+"eje", cmpIgnored,"ns");
  def template = new AdverbTemplate("ns","","o","eje");
}