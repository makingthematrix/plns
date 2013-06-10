package logic

case class Adverb(val ind: String,val cmp: String,val sup: String,val cmpIgnored: Boolean,override val lang: String) 
			extends SpeechPart[Adverb] {
  override def mainRoot = ind;
  
  override val speechPart = "adverb"
	
  override def translateTo(adv: Adverb): Boolean = addRoots(adv) match {
    case Some((rootId1,rootId2)) => {
      NSTranslator.add(ind,rootId1,adv.ind,rootId2);
      if(!cmpIgnored){
    	  NSTranslator.add(cmp,rootId1,adv.cmp,rootId2);
    	  NSTranslator.add(sup,rootId1,adv.sup,rootId2);
      }
      true
    }
    case None => false
  }
  
}