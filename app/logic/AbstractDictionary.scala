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
  def getTranslation(word: String):Option[String]
  def add(entry: DictEntry): Boolean
  def update(entry: DictEntry): Boolean
  def remove(entry: DictEntry): Boolean
  def getEntry(word: String): DictEntry
  def seq: Seq[DictEntry]

  def isEmpty:Boolean
  
  /** @todo can these be refactorized into one generic method?
   *  they all do similar stuff, although the way it's done in DB
   *  is totally different from the debug mode
   */
  def add(pair: UninflectedPair): Long
  def add(pair: AdverbPair): Long
  def add(pair: AdjectivePair): Long
  def add(pair: NounPair): Long
  def add(pair: VerbPair): Long
  
  def add[T](pair: SpeechPartPair[T]): Long = pair match {
    case un: UninflectedPair => add(un)
    case adv: AdverbPair => add(adv)
    case adj: AdjectivePair => add(adj)
    case noun: NounPair => add(noun)
    case verb: VerbPair => add(verb)
    case _ => throw new IllegalArgumentException("Unrecogrnized speech part: " + pair.toString())
  }
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