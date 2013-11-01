package logic

import scala.collection.mutable
import models.UninflectedPair
import models.AdverbPair
import models.AdjectivePair
import models.NounPair
import models.VerbPair
import models.SpeechPartPair

class Dictionary extends AbstractDictionary {
  private val pairs = new mutable.HashMap[Int, SpeechPartPair[_ <: SpeechPart[_]]]
  private val entries = new mutable.HashMap[Int, DictEntry]
  
  override def clear(){
    pairs.clear
    entries.clear
  }
  
  override def size:Long = entries.size
  
  override def isEmpty = entries.isEmpty
  
  override def getTranslation(word: String) = {
    val entryOption = entries.find{ entry => entry._2.plWord == word }
    entryOption match {
      case Some(tuple) => Some(tuple._2.nsWord)
      case None => None
    }
  }
  
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
        val copy = pair.copyWithId(pairs.size)
        pairs.put(copy.hashCode, copy)
        copy.id
      }
    }
  }
  
  override def updatePair[T <: SpeechPart[T]](pair: SpeechPartPair[T]){
    val removedPair = removePair(pair)
    println("Dictionary.update, replacing pair " + removedPair + " with " + pair)
	pairs.put(pair.hashCode, pair)
  }
  
  override def removePair[T <: SpeechPart[T]](pair : SpeechPartPair[T]) = {
    val pairOption = getPairById(pair)
    pairOption match {
      case Some(p) => pairs.remove(p.hashCode); Some(p)
      case None => None
    }
  }
  
  override def getPairById[T <: SpeechPart[T]](pair: SpeechPartPair[T]): Option[SpeechPartPair[T]] = 
    pairs.values.find { p => p.id == pair.id }.asInstanceOf[Option[SpeechPartPair[T]]]
  
  /** @todo again, it should be something like filterBySpeechPart[T <: SpeechPartPair[T]] but it doesn't work */
  private def filterBySpeechPart[T](speechPart: String) = 
    pairs.values.filter{ _.speechPart == speechPart }.asInstanceOf[Iterable[T]]
    
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
  
  override def listPairs:Seq[SpeechPartPair[_ <: SpeechPart[_]]] = pairs.values.toSeq
  
  override def addEntry(entry: DictEntry) = getEntryByContents(entry) match {
    case Some(e) => e.id
    case None => {
      val copy = entry.copyWithId(entries.size)
      entries.put(copy.hashCode, copy)
      copy.id
    }
  }	
  
  override def addEntries(entries: Seq[DictEntry]) = entries.foreach( addEntry(_) )
  
  override def updateEntry(entry: DictEntry) {
    val removedEntry = removeEntry(entry.id)
    println("Dictionary.update, replacing entry " + removedEntry + " with " + entry)
	entries.put(entry.hashCode, entry)
  }
  
  override def removeEntry(id: Long) = {
    val entryOption = entries.find{ tuple => tuple._2.id == id }
    entryOption match {
      case Some(tuple) => entries.remove(tuple._2.hashCode); Some(tuple._2);
      case None => None
    }
  }
  
  override def getEntryById(id: Long) = {
    val entryOption = entries.find{ tuple => tuple._2.id == id }
    entryOption match {
      case Some(tuple) => Some(tuple._2)
      case None => None
    }
  }
	
  override def getEntryByContents(entry: DictEntry) = getWord(entry.plWord, entry.plLang)
  
  override def getWord(word: String, lang: String) = 
    entries.values.find(e => e.plWord == word && e.plLang == lang)

}