package logic

import scala.collection.mutable;

class Dictionary extends AbstractDictionary {
  private val roots2words = new mutable.HashMap[String,mutable.ListBuffer[String]]
  private val map = new mutable.HashMap[String,String];
  private val rootsMap = new mutable.HashMap[String,Root];
  private val rootsIds = new mutable.HashMap[Long,String];
  private val rootTrans = new mutable.HashMap[Long,Long];

  override def get(word: String) = map.get(word)
  
  private def addRoots2Words(word: Word) = rootsIds.get(word.rootid) match {
	case Some(root) => {
	  val list = roots2words.get(root) match {
	    case Some(list) => list
	    case None => { val list = new mutable.ListBuffer[String](); roots2words += (root -> list); list; }
	  }
	  list += word.word.toLowerCase()
	}
	case None => 
  }
  
  override def add(from: Word, to: Word){
	println("Dictionary.add, " + from + " -> " + to);
	val w = from.word.toLowerCase();
	if(!map.contains(w)){
	  println("no such word yet in the map - adding")
	  map += (w -> to.word.toLowerCase());
	  addRoots2Words(from)
	  addRoots2Words(to)
	}
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
  
  override def addRoot(root: Root):Option[Long] = rootsMap.get(root.root) match {
    case Some(root) => Some(root.id)
    case None => {
      val id:Long = roots.size + 1
      rootsMap += (root.root -> root)
      rootsIds += (id -> root.root)
      Some(id)
    }
  }
  
  override def addRoots(from: Root,to: Root): (Long,Long) = {
    val rootId1 = addRoot(from) match {
      case Some(id) => id
      case None => throw new IllegalArgumentException("Unable to store root " + from)
    }
    val rootId2 = addRoot(to) match {
      case Some(id) => id
      case None => throw new IllegalArgumentException("Unable to store root " + to)
    }
    rootTrans += (rootId1 -> rootId2)
    return (rootId1, rootId2)
  }

  override def roots:Seq[Root] = rootsMap.values.toSeq
  
  override def rootPairs:Seq[(Root,Root)] = {
    rootTrans.keys.map(key => {
        val value = rootTrans(key)
        val root1 = rootsIds(key)
        val root2 = rootsIds(value)
        (rootsMap(root1),rootsMap(root2))
    }).toSeq
  }
}