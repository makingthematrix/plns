package logic

import scala.collection.mutable;

class Dictionary {
  private val map = new mutable.HashMap[String, String];
	
  private val allLowercasePattern = """^[a-ząćęłńóśźż].+$""".r;
  private val allUppercasePattern = """^[A-ZĄĆĘŁŃÓŚŹŻ].+$""".r;
  private val startsUppercasePattern = """^[A-ZĄĆĘŁŃÓŚŹŻ]{1}[a-ząćęłńóśźż].*$""".r;
  private val letterPattern = """^[a-zA-ZĄĆĘŁŃÓŚŹŻąćęłńóśźż\-]{1}$""".r;
  private val wordPattern = """^[a-zA-ZĄĆĘŁŃÓŚŹŻąćęłńóśźż\-].*$""".r;
  
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
	  
	val w = word.toLowerCase();
	val t = map.getOrElse(w,null);
	if(t == null){
	  return (word,false);
	}
	  
	val translated = word match {
	  case allLowercasePattern() => t;
	  case startsUppercasePattern() => t.charAt(0).toString.toUpperCase() + t.substring(1);
	  case allUppercasePattern() => t.toUpperCase();
	  case _ => t;
	};
	return (translated,true);
  }
	
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
	
  def add(tuple: (String,String)): Unit = add(tuple._1,tuple._2);
	
  def add(from: String, to: String): Unit = {
	println(from + " -> " + to);
	val w = from.toLowerCase();
	if(map.contains(w)) return;
	map += (w -> to.toLowerCase());
  }
	
  def update(from: String, to: String): Unit = {
	val w = from.toLowerCase();
	if(map.contains(w)) map -= w;
	map += (w -> to.toLowerCase());
  }
	
  def update(tuple: (String,String)): Unit = update(tuple._1,tuple._2);
	
  def keys = map.keys;
  
  def values = map.values;
	
  def tuples = map.keys.map(w =>(w,map.getOrElse(w,w)))
  
  def isEmpty = map.isEmpty;
  
}