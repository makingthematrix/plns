package logic

import scala.collection.mutable
import models.UninflectedPair
import models.AdverbPair
import models.AdjectivePair
import models.NounPair
import models.VerbPair
import models.SpeechPartPair

import scala.reflect.runtime.{universe => ru}

case class Root(id: Long, root: String, speechPart: SpeechPart.Value, lang: String){
  def this(root: String, speechPart: SpeechPart.Value, lang: String) = this(-1L, root, speechPart, lang)
}

abstract class AbstractDictionary {
  def clear:Unit
  def size:Int
  def isEmpty:Boolean
  def getTranslation(word: String):Option[String]
  
  def addPair[T](pair: SpeechPartPair[T]): Long
  def updatePair[T](pair: SpeechPartPair[T]): Unit
  def removePair[T](id: Long): SpeechPartPair[T]
  def listPairs: Seq[SpeechPartPair[_]]
  
  def add(entry: DictEntry): Long
  def update(entry: DictEntry): Unit
  def remove(id: Long): DictEntry
  /** get by id */
  def get(id: Long): Option[DictEntry]
  /** get by contents */
  def get(entry: DictEntry): Option[DictEntry]
  
  
//-----------------------------------------------------
  
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