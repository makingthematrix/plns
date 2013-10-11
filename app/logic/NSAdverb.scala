package logic

import models.AdverbTemplate

object NSAdverb {
  def word(stem: String):Adverb = word(stem,stem,false)
  def word(ind: String, cmp: String, cmpIgnored: Boolean):Adverb = new Adverb(ind+"o",cmp+"eje","naj"+cmp+"eje",cmpIgnored,"ns")
  def template = new AdverbTemplate("ns","","o","eje")
}