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
    val removedPair = removePair(pair.id)
    println("Dictionary.update, replacing pair " + removedPair + " with " + pair)
	pairs.put(pair.hashCode, pair)
  }
  
  override def removePair[T](id: Long): SpeechPartPair[T] = {
    val pairOption = pairs.find{ tuple => tuple._2.id == id }
    pairOption match {
      case Some(tuple) => pairs.remove(tuple._2.hashCode); tuple._2.asInstanceOf[SpeechPartPair[T]]
      case None => throw new IllegalArgumentException("No entry with the given id: " + id)
    }
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