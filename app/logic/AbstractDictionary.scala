package logic

import scala.collection.mutable
import models.UninflectedPair
import models.AdverbPair
import models.AdjectivePair
import models.NounPair
import models.VerbPair
import models.SpeechPartPair

abstract class AbstractDictionary {
  def clear:Unit
  def size:Long
  def isEmpty:Boolean
  
  def getTranslation(word: String):Option[String]
  
  def addPair[T <: SpeechPart[T]](pair: SpeechPartPair[T]): Long
  protected def updatePair[T <: SpeechPart[T]](pair: SpeechPartPair[T]): Unit
  protected def removePair[T <: SpeechPart[T]](pair: SpeechPartPair[T]): Option[SpeechPartPair[T]]
  protected def removePair(speechPart: String, id: Long): Unit
  
  def getPairById[T <: SpeechPart[T]](pair: SpeechPartPair[T]): Option[SpeechPartPair[T]]
  def getPairById[T <: SpeechPart[T]](speechPart: String, pairId: Long): Option[SpeechPartPair[T]]
  /** Tries to retrieve the pair by its contents, different for every type.
   *  In order to use it, create a stub version of the pair of the given type
   *  and provide at least info for the 'from' part. */
  def getPairByContents[T <: SpeechPart[T]](pair: SpeechPartPair[T]): Option[SpeechPartPair[T]]
  def listPairs: Seq[SpeechPartPair[_ <: SpeechPart[_]]]
  
  protected def addEntry(entry: DictEntry): Long
  def addEntries(entries: Seq[DictEntry]): Unit
  protected def updateEntry(entry: DictEntry): Unit
  protected def removeEntry(id: Long): Option[DictEntry]
  protected def removeEntries(speechPart: String, pairId: Long): Unit
  def getEntryById(id: Long): Option[DictEntry]
  def getEntryByContents(entry: DictEntry): Option[DictEntry]
  def getWord(word: String, lang: String): Option[DictEntry]
  
  def addRoot(root: Root): Long
  protected def removeRoots(speechPart: String, pairId: Long): Unit
  def getRootByWord(root: String): Option[Root]
  def getRootById(id: Long): Option[Root]
  def roots: Seq[Root]
 
//-----------------------------------------------------
  
  def add[T <: SpeechPart[T]](pair: SpeechPartPair[T]) = getPairByContents(pair) match {
    case Some(p) => throw new IllegalArgumentException("There is a pair with matching contents in the dictionary: " + pair)
    case None => pair.add(this)
  }
  
  def remove(speechPart: String, pairId: Long) = getPairById(speechPart, pairId) match {
    case None => throw new IllegalArgumentException("Unable to find a " + speechPart + " with pairId: " + pairId)
    case Some(p) => {
      removeEntries(speechPart, pairId)
      removeRoots(speechPart, pairId)
      removePair(speechPart, pairId)
    }
  }
  
  def update[T <: SpeechPart[T]](pair: SpeechPartPair[T]) = getPairById(pair) match {
    case None => throw new IllegalArgumentException("Unable to find a " + pair.speechPart + " with pairId: " + pair.id)
    case Some(p) => {
      removeEntries(pair.speechPart, pair.id)
      removeRoots(pair.speechPart, pair.id)
      updatePair(pair)
      val (plRoot, nsRoot) = pair.roots
      addRoot(plRoot)
      addRoot(nsRoot)
      addEntries(pair.generate)
    }
  }
  
  def translate(sentence: String):(String,Seq[String]) = {
	val words = split(sentence)
	val translatedPairs = words.map(word => word match {
	  case wordPattern() => translateWord(word)
	  case _ => (word,true)
	})
	
	val translatedWords = translatedPairs.map(pair => pair._1)
	val untranslated = translatedPairs.flatMap(pair => if(pair._2) None else Some(pair._1)).toSeq
	(translatedWords.mkString,untranslated)
  }
  
  private def split(sentence: String) = {
    val words = new mutable.ArrayBuffer[String]()
	var flag = 0 // 0 - start, 1 - a letter, 2 - other
	val sb = StringBuilder.newBuilder
	
	val flush = (newFlag: Int) => { 
	  if(!sb.isEmpty){ 
	    words += sb.toString 
	    sb.clear()
	  }
	  flag = newFlag 
	}
	    
	sentence.toCharArray.foreach(c => { 
	  c.toString match { 
	    case letterPattern() => if(flag != 1) flush(1)
	    case _ => if(flag == 1) flush(2)
	  }
	  sb.append(c)
	})
	    
	flush(0)
	    
	words.toSeq
  }
    
  private def translateWord(word: String): (String,Boolean) = {
	if(word.isEmpty()) return ("",true)
	if(wordPattern.findFirstIn(word).isEmpty) return (word,true)
	println("word to translate: " + word)
	getTranslation(word.toLowerCase()) match {
	  case Some(t) => {
	    val translated = word match {
	      case allLowercasePattern() => t
	      case startsUppercasePattern() => t.charAt(0).toString.toUpperCase() + t.substring(1)
	      case allUppercasePattern() => t.toUpperCase()
	      case _ => t
	    };
	    (translated,true)    
	  }
	  case None => (word,false)
	}
  }
	
  private val allLowercasePattern = """^[a-ząćęłńóśźż].+$""".r;
  private val allUppercasePattern = """^[A-ZĄĆĘŁŃÓŚŹŻ].+$""".r;
  private val startsUppercasePattern = """^[A-ZĄĆĘŁŃÓŚŹŻ]{1}[a-ząćęłńóśźż].*$""".r;
  private val letterPattern = """^[a-zA-ZĄĆĘŁŃÓŚŹŻąćęłńóśźż\-]{1}$""".r;
  private val wordPattern = """^[a-zA-ZĄĆĘŁŃÓŚŹŻąćęłńóśźż\-].*$""".r;
}