package logic

import scala.collection.mutable
import models.UninflectedPair
import models.AdverbPair
import models.AdjectivePair
import models.NounPair
import models.VerbPair
import models.SpeechPartPair

class Dictionary extends AbstractDictionary {
  private val pairs = new mutable.HashMap[Int, SpeechPartPair[_]]
  private val entries = new mutable.HashMap[Int, DictEntry]
  
  override def clear(){
    pairs.clear
    entries.clear
  }
  
  override def size = entries.size
  
  override def isEmpty = entries.isEmpty
  
  override def getTranslation(word: String) = {
    val entryOption = entries.find{ entry => entry._2.plWord == word }
    entryOption match {
      case Some(tuple) => Some(tuple._2.nsWord)
      case None => None
    }
  }
    
  override def addPair[T](pair: SpeechPartPair[T]): Long = 
    if(!pairs.contains(pair.hashCode)){
      val copy = pair.copyWithId(pairs.size)
      assert(pair.compareContents(copy) == 0)
      pairs.put(copy.hashCode, copy)
      copy.id
    } else {
      val original = pairs(pair.hashCode)
      original.id
    }
  
  override def updatePair[T](pair: SpeechPartPair[T]){
    val removedPair = removePair(pair)
    println("Dictionary.update, replacing pair " + removedPair + " with " + pair)
	pairs.put(pair.hashCode, pair)
  }
  
  override def removePair[T](pair : SpeechPartPair[T]): Option[SpeechPartPair[T]] = {
    val pairOption = getById(pair)
    pairOption match {
      case Some(p) => pairs.remove(p.hashCode); Some(p)
      case None => None
    }
  }
  
  override def getById[T](pair: SpeechPartPair[T]): Option[SpeechPartPair[T]] = 
    pairs.values.find { p => p.id == pair.id }.asInstanceOf[Option[SpeechPartPair[T]]]
  
  /** @todo again, it should be something like filterBySpeechPart[T <: SpeechPartPair[T]] but it doesn't work */
  private def filterBySpeechPart[T](speechPart: String) = 
    pairs.values.filter{ _.speechPart == "uninflected" }.asInstanceOf[Iterable[T]]
    
  private def getByContents(pair: UninflectedPair): Option[UninflectedPair] = {
    val filtered = filterBySpeechPart[UninflectedPair]("uninflected")
    filtered.find{ _.plWord == pair.plWord }
  }
  
  private def getByContents(pair: AdverbPair): Option[AdverbPair] = {
    val filtered = filterBySpeechPart[AdverbPair]("adverb")
    filtered.find{ p => p.plInd == pair.plInd && p.plCmp == pair.plCmp }
  }
  
  private def getByContents(pair: AdjectivePair): Option[AdjectivePair] = {
    val filtered = filterBySpeechPart[AdjectivePair]("adjective")
    filtered.find{ p => p.plInd == pair.plInd && p.plCmp == pair.plCmp }
  }
  
  private def getByContents(pair: NounPair): Option[NounPair] = {
    val filtered = filterBySpeechPart[NounPair]("noun")
    filtered.find{ p => p.plStem == pair.plStem && p.plPattern == pair.plPattern }
  }
  
  private def getByContents(pair: VerbPair): Option[VerbPair] = {
    val filtered = filterBySpeechPart[VerbPair]("verb")
    filtered.find{ p => p.plInfStem == pair.plInfStem && p.plImpStem == pair.plImpStem }
  }
  
  override def getByContents[T](pair: SpeechPartPair[T]): Option[SpeechPartPair[T]] = pair match {
    case un: UninflectedPair => getByContents(un)
    case adv: AdverbPair => getByContents(adv)
    case adj: AdjectivePair => getByContents(adj)
    case noun: NounPair => getByContents(noun)
    case verb: VerbPair => getByContents(verb)
    case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
  }
  
  override def listPairs = pairs.values.toSeq
  
  override def add(entry: DictEntry): Long = 
    if(!entries.contains(entry.hashCode)) {
	println("Dictionary.add, " + entry.plWord + " -> " + entry.nsWord)
	  val copy = entry.copyWithId(entries.size)
      assert(entry.compareContents(copy) == 0)
      entries.put(copy.hashCode, copy)
      copy.id
    } else {
      val original = entries(entry.hashCode)
      original.id
    }
	
  override def update(entry: DictEntry) {
    val removedEntry = remove(entry.id)
    println("Dictionary.update, replacing entry " + removedEntry + " with " + entry)
	entries.put(entry.hashCode, entry)
  }
  
  override def remove(id: Long): DictEntry = {
    val entryOption = entries.find{ tuple => tuple._2.id == id }
    entryOption match {
      case Some(tuple) => entries.remove(tuple._2.hashCode); tuple._2
      case None => throw new IllegalArgumentException("No entry with the given id: " + id)
    }
  }
  
  /** get by id */
  override def get(id: Long): Option[DictEntry] = {
    val entryOption = entries.find{ tuple => tuple._2.id == id }
    entryOption match {
      case Some(tuple) => Some(tuple._2)
      case None => None
    }
  }
	
  /** get by contents */
  override def get(entry: DictEntry): Option[DictEntry] = entries.get(entry.hashCode)

}