package logic

case class Adverb(val ind: String,val cmp: String,val sup: String,val cmpIgnored: Boolean,override val lang: String) 
			extends SpeechPart[Adverb] {
  override def mainRoot = ind;
  override val speechPart = "adverb"
  override def toRoot():Root = new Root(mainRoot,speechPart,lang)
  
  override def generate(adv: Adverb) = { 
    val indDE = DictEntry(ind,lang,adv.ind,adv.lang,"ind")
    if(!cmpIgnored){
      val cmpDE = DictEntry(cmp,lang,adv.cmp,adv.lang,"cmp")
      val supDE = DictEntry(sup,lang,adv.sup,adv.lang,"sup")
      Seq(indDE,cmpDE,supDE)
    } else Seq(indDE)
  }  
  
  override def validateExceptionKey(key: String): String = key // adverbs don't have exceptions
}