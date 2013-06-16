package logic

import scala.collection.mutable;

case class RootWord(word: String, speechPart: String, lang: String);

abstract class AbstractDictionary {
  def get(word: String):Option[String];
	
  def add(from: String, to: String): Unit;
	
  def update(from: String, to: String): Unit;
	
  def tuples:Seq[(String,String)];
  
  def words:Seq[String];
  
  def hasWord(word: String,lang: String):Boolean;
  
  def isEmpty:Boolean;
  
  def addRoot(word: String, speechPart: String, lang: String):Option[Long]
  
  def roots:Seq[RootWord]
//-----------------------------------------------------
  def add(tuple: (String,String)): Unit = add(tuple._1,tuple._2);
	
  def update(tuple: (String,String)): Unit = update(tuple._1,tuple._2);
  
  def translate(sentence: String): (String, Array[String]) = {
	val words = split(sentence);
	val translatedPairs = words.map(word => word match {
	  case wordPattern() => translateWord(word);
	  case _ => (word,true);
	});
	
	val translatedWords = translatedPairs.map(pair => pair._1);
	val untranslated = translatedPairs.flatMap(pair => if(pair._2) None else Some(pair._1));
	return (translatedWords.mkString,untranslated);
  }
  
  private def split(sentence: String): Array[String] = {
    val words = new mutable.ArrayBuffer[String]();
	var flag = 0; // 0 - start, 1 - a letter, 2 - other
	val sb = new StringBuilder();
	
	val flush = (newFlag: Int) => { 
	  if(!sb.isEmpty){ 
	    words += sb.toString; 
	    sb.clear(); 
	  }; 
	  flag = newFlag; 
	}
	    
	sentence.toCharArray.foreach(c => { 
	  c.toString match { 
	    case letterPattern() => if(flag != 1) flush(1);
	    case _ => if(flag == 1) flush(2);
	  }
	  sb.append(c);
	});
	    
	flush(0);
	    
	words.toArray;
  }
    
  private def translateWord(word: String): (String,Boolean) = {
	if(word.isEmpty()) return ("",true);
	if(wordPattern.findFirstIn(word).isEmpty) return (word,true);
	
	get(word.toLowerCase()) match {
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