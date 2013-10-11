package logic

case class Adverb(val ind: String,val cmp: String,val sup: String,val cmpIgnored: Boolean,override val lang: String) 
			extends SpeechPart[Adverb] {
  override def mainRoot = ind;
  
  override val speechPart = "adverb"
    
  override def toRoot():Root = new Root(mainRoot,speechPart,lang)
  
  override def translateTo(adv: Adverb, rootId1: Long, rootId2: Long) { 
    NSTranslator.add(new Word(ind,lang,rootId1,"ind"),new Word(adv.ind,adv.lang,rootId2,"ind"))
    if(!cmpIgnored){
      NSTranslator.add(new Word(cmp,lang,rootId1,"cmp"),new Word(adv.cmp,adv.lang,rootId2,"cmp"))
      NSTranslator.add(new Word(sup,lang,rootId1,"sup"),new Word(adv.sup,adv.lang,rootId2,"sup"))
    }
  }  
  
  override def validateExceptionKey(key: String): String = key // adverbs don't have exceptions
}