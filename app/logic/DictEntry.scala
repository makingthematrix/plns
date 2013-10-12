package logic

case class DictEntry(val plWord: String,val plLang: String,val nsWord: String,val nsLang: String,val caseId: String) {
  def this(plWord: String,plLang: String,nsWord: String,nsLang: String) = this(plWord,plLang,nsWord,nsLang,DictEntry.undef)

  def makePlWord(rootId: Long) = new Word(plWord,plLang,rootId,caseId)
  def makeNsWord(rootId: Long) = new Word(nsWord,nsLang,rootId,caseId)
  def wordPair(plRootId: Long,nsRootId: Long):(Word,Word) = (makePlWord(plRootId),makeNsWord(nsRootId))
}

object DictEntry {
  val undef = "undef" // undefined case
}