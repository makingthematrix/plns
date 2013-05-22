package logic

case class Adverb(val ind: String,val cmp: String,val sup: String,val cmpIgnored: Boolean) extends SpeechPart[Adverb] {
  override def mainRoot = ind;
	
  override def translateTo(adv: Adverb): Unit = {
    NSTranslator.add(ind,adv.ind);
    if(!cmpIgnored){
      NSTranslator.add(cmp,adv.cmp);
	  NSTranslator.add(sup,adv.sup);
    }
  }
  
}