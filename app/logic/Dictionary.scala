package logic

import scala.collection.mutable;

class Dictionary extends AbstractDictionary {
  private val map = new mutable.HashMap[String, String];

  override def get(word: String) = map.get(word)
  
  override def add(from: String, to: String){
	println(from + " -> " + to);
	val w = from.toLowerCase();
	if(!map.contains(w)) map += (w -> to.toLowerCase());
  }
	
  override def update(from: String, to: String){
	val w = from.toLowerCase();
	if(map.contains(w)) map -= w;
	map += (w -> to.toLowerCase());
  }
	
  override def words:Seq[String] = map.keys.toSeq ++ map.values
  
  override def hasWord(word: String,lang: String):Boolean = lang match {
    case "pl" => map.keySet.contains(word)
    case "ns" => map.values.toSet.contains(word)
    case _ => false
  }
  
  override def tuples = map.keys.toSeq.map(w =>(w,map.getOrElse(w,w)))
  
  override def isEmpty = map.isEmpty;
  
}