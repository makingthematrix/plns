package logic

import scala.collection.mutable
import models.UninflectedPair
import models.AdverbPair
import models.AdjectivePair
import models.NounPair
import models.VerbPair
import models.SpeechPartPair

class Dictionary extends AbstractDictionary {
  private val pairsMap = new mutable.HashMap[Int, SpeechPartPair[_ <: SpeechPart[_]]]
  private val entriesMap = new mutable.HashMap[Int, DictEntry]
  private val rootsMap = new mutable.HashMap[String, Root]
  
  override def clear(){
    pairsMap.clear
    rootsMap.clear
    entriesMap.clear
  }
  
  override def size:Long = entriesMap.size
  
  override def isEmpty = entriesMap.isEmpty
  
  override def getTranslation(word: String) = {
    val entryOption = entriesMap.values.find{ _.plWord == word }
    entryOption match {
      case Some(entry) => Some(entry.nsWord)
      case None => None
    }
  }
  
  var firstFreePairId = 1
  
  override def addPair[T <: SpeechPart[T]](pair: SpeechPartPair[T]) = {
    val pOption = pair match {
      case un: UninflectedPair => getByContents(un)
      case adv: AdverbPair => getByContents(adv)
      case adj: AdjectivePair => getByContents(adj)
      case noun: NounPair => getByContents(noun)
      case verb: VerbPair => getByContents(verb)
      case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
    }
    
    pOption match {
      case Some(p) => p.id
      case None => {
        val copy = pair.copyWithId(firstFreePairId)
        firstFreePairId += 1
        pairsMap.put(copy.hashCode, copy)
        copy.id
      }
    }
  }
  
  override protected def updatePair[T <: SpeechPart[T]](pair: SpeechPartPair[T]){
    val removedPair = removePair(pair)
    println("Dictionary.update, replacing pair " + removedPair + " with " + pair)
	pairsMap.put(pair.hashCode, pair)
  }
  
  override protected def removePair[T <: SpeechPart[T]](pair : SpeechPartPair[T]) = {
    val pairOption = getPairById(pair)
    pairOption match {
      case Some(p) => pairsMap.remove(p.hashCode); Some(p)
      case None => None
    }
  }
  
  override protected def removePair(speechPart: String, id: Long){
    val pOption = pairsMap.values.find{ e => e.speechPart == speechPart && e.id == id }
    pOption match {
      case Some(p) => pairsMap.remove(p.hashCode)
      case None =>
    }
  }
  
  override def getPairById[T <: SpeechPart[T]](pair: SpeechPartPair[T]): Option[SpeechPartPair[T]] = 
    pairsMap.values.find { _.id == pair.id }.asInstanceOf[Option[SpeechPartPair[T]]]
  
  override def getPairById[T <: SpeechPart[T]](speechPart: String, pairId: Long): Option[SpeechPartPair[T]] = {
    pairsMap.values.find{ e => e.speechPart == speechPart && e.id == pairId }.asInstanceOf[Option[SpeechPartPair[T]]]
  }
    
  /** @todo again, it should be something like filterBySpeechPart[T <: SpeechPartPair[T]] but it doesn't work */
  private def filterBySpeechPart[T](speechPart: String) = 
    pairsMap.values.filter{ _.speechPart == speechPart }.asInstanceOf[Iterable[T]]
    
  private def getByContents(pair: UninflectedPair) = {
    val filtered = filterBySpeechPart[UninflectedPair]("uninflected")
    filtered.find{ _.plWord == pair.plWord }
  }
  
  private def getByContents(pair: AdverbPair) = {
    val filtered = filterBySpeechPart[AdverbPair]("adverb")
    filtered.find{ p => p.plInd == pair.plInd && p.plCmp == pair.plCmp }
  }
  
  private def getByContents(pair: AdjectivePair) = {
    val filtered = filterBySpeechPart[AdjectivePair]("adjective")
    filtered.find{ p => p.plInd == pair.plInd && p.plCmp == pair.plCmp }
  }
  
  private def getByContents(pair: NounPair) = {
    val filtered = filterBySpeechPart[NounPair]("noun")
    filtered.find{ p => p.plStem == pair.plStem && p.plPattern == pair.plPattern }
  }
  
  private def getByContents(pair: VerbPair) = {
    val filtered = filterBySpeechPart[VerbPair]("verb")
    filtered.find{ p => p.plInfStem == pair.plInfStem && p.plImpStem == pair.plImpStem }
  }
  
  /** @todo find a way without using asInstanceOf */
  override def getPairByContents[T <: SpeechPart[T]](pair: SpeechPartPair[T]) = pair match {
    case un: UninflectedPair => getByContents(un).asInstanceOf[Option[SpeechPartPair[T]]]
    case adv: AdverbPair => getByContents(adv).asInstanceOf[Option[SpeechPartPair[T]]]
    case adj: AdjectivePair => getByContents(adj).asInstanceOf[Option[SpeechPartPair[T]]]
    case noun: NounPair => getByContents(noun).asInstanceOf[Option[SpeechPartPair[T]]]
    case verb: VerbPair => getByContents(verb).asInstanceOf[Option[SpeechPartPair[T]]]
    case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
  }
  
  override def listPairs:Seq[SpeechPartPair[_ <: SpeechPart[_]]] = pairsMap.values.toSeq
  
  private var firstFreeEntryId = 1
  
  override protected def addEntry(entry: DictEntry) = getEntryByContents(entry) match {
    case Some(e) => e.id
    case None => {
      val copy = entry.copyWithId(firstFreeEntryId)
      firstFreeEntryId += 1
      entriesMap.put(copy.hashCode, copy)
      copy.id
    }
  }	
  
  override def addEntries(entries: Seq[DictEntry]) = entries.foreach( addEntry(_) )
  
  override protected def updateEntry(entry: DictEntry) {
    val removedEntry = removeEntry(entry.id)
    println("Dictionary.update, replacing entry " + removedEntry + " with " + entry)
	entriesMap.put(entry.hashCode, entry)
  }
  
  override protected def removeEntry(id: Long) = {
    val entryOption = entriesMap.find{ tuple => tuple._2.id == id }
    entryOption match {
      case Some(tuple) => entriesMap.remove(tuple._2.hashCode); Some(tuple._2);
      case None => None
    }
  }
  
  override protected def removeEntries(speechPart: String, pairId: Long){
    val codes = entriesMap.values.filter{ e => e.speechPart == speechPart && e.speechPartId == pairId }.map{ _.hashCode }
    codes.foreach{ entriesMap.remove(_) }
  }
  
  override def getEntryById(id: Long) = {
    val entryOption = entriesMap.find{ tuple => tuple._2.id == id }
    entryOption match {
      case Some(tuple) => Some(tuple._2)
      case None => None
    }
  }
	
  override def getEntryByContents(entry: DictEntry) = getWord(entry.plWord, entry.plLang)
  
  override def getWord(word: String, lang: String) = 
    entriesMap.values.find{ e => e.plWord == word && e.plLang == lang }
    
  var firstFreeRootId = 1
  
  override def addRoot(root: Root) = getRootByWord(root.root) match {
    case Some(r) => throw new IllegalArgumentException("There is already this root in the dictionary: " + r)
    case None => {
      val copy = root.copyWithId(firstFreeRootId)
      firstFreeRootId += 1
      rootsMap.put(copy.root, root)
      copy.id
    }
  }
  
  protected def removeRoots(speechPart: String, pairId: Long){
    val filtered = rootsMap.values.filter{ r => r.speechPart == speechPart && r.speechPartId == pairId }
    filtered.foreach{ r => rootsMap.remove(r.root) }
  }
  
  override def getRootByWord(root: String) = rootsMap.values.find{ _.root == root }
  override def getRootById(id: Long) = rootsMap.values.find{ _.id == id } 
  override def roots = rootsMap.values.toSeq
}