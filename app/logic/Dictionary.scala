package logic

import scala.collection.mutable
import models.UninflectedPair
import models.AdverbPair
import models.AdjectivePair
import models.NounPair
import models.VerbPair
import models.SpeechPartPair

class Dictionary extends AbstractDictionary {
  private val uninflecteds = new mutable.HashMap[Int, UninflectedPair]
  private val adverbs = new mutable.HashMap[Int, AdverbPair]
  private val adjectives = new mutable.HashMap[Int, AdjectivePair]
  private val nouns = new mutable.HashMap[Int, NounPair]
  private val verbs = new mutable.HashMap[Int, VerbPair]
  private val map = new mutable.HashMap[String,String]
  private val entries = new mutable.HashMap[Int, DictEntry]
  
  override def getTranslation(word: String) = map.get(word)
  
  /** @todo ok, now, there has to be some way to generalize the following methods
   *  somehow along the lines of this commented out method (it was commented out because it doesn't work)
   */
  /**
  private def add[T <: SpeechPartPair[T]](map: mutable.HashMap[Int, SpeechPartPair[T]], pair: SpeechPartPair[T]): Long = 
    if(!map.contains(pair.hashCode)){
      val copy = pair.copyWithId(map.size)
      assert(pair.compareContents(copy) == 0)
      map.put(copy.hashCode, copy)
      copy.id
    } else {
      val original = map(pair.hashCode)
      original.id
    }
  */
  override def add(un: UninflectedPair): Long = if(!uninflecteds.contains(un.hashCode)){
      val copy = un.copyWithId(uninflecteds.size)
      assert(un.compareContents(copy) == 0)
      uninflecteds.put(copy.hashCode, copy)
      copy.id
    } else {
      val original = uninflecteds(un.hashCode)
      original.id
    }
  
  override def add(adv: AdverbPair) = if(!adverbs.contains(adv.hashCode)){
      val copy = adv.copyWithId(adverbs.size)
      assert(adv.compareContents(copy) == 0)
      adverbs.put(copy.hashCode, copy)
      copy.id
    } else {
      val original = adverbs(adv.hashCode)
      original.id
    }
  
  override def add(adj: AdjectivePair) = if(!adjectives.contains(adj.hashCode)){
      val copy = adj.copyWithId(adjectives.size)
      assert(adj.compareContents(copy) == 0)
      adjectives.put(copy.hashCode, copy)
      copy.id
    } else {
      val original = adjectives(adj.hashCode)
      original.id
    }
  
  override def add(noun: NounPair) = if(!nouns.contains(noun.hashCode)){
      val copy = noun.copyWithId(nouns.size)
      assert(noun.compareContents(copy) == 0)
      nouns.put(copy.hashCode, copy)
      copy.id
    } else {
      val original = nouns(noun.hashCode)
      original.id
    }
  
  override def add(verb: VerbPair) = if(!verbs.contains(verb.hashCode)){
      val copy = verb.copyWithId(verbs.size)
      assert(verb.compareContents(copy) == 0)
      verbs.put(copy.hashCode, copy)
      copy.id
    } else {
      val original = verbs(verb.hashCode)
      original.id
    }
  
  override def add(entry: DictEntry): Boolean = {
	println("Dictionary.add, " + entry.plWord + " -> " + entry.nsWord)
	val w = entry.plWord.toLowerCase()
	if(!map.contains(w)){
	  println("no such word yet in the map - adding")
	  map += (w -> entry.nsWord.toLowerCase())
	  entries += entry
	  true
	} else false
  }
	
  override def update(entry: DictEntry): Boolean = {
	remove(entry)
	add(entry)
  }
  
  override def remove(entry: DictEntry): Boolean = {
    val w = entry.plWord.toLowerCase();
	if(map.contains(w)){
	  val filteredEntrySet = entries.filter(_.plWord == w)
	  if(filteredEntrySet.isEmpty) 
	    throw new IllegalArgumentException("Dictionary.update: Found the word " + w + " in the map, but no associated DictEntry")
	  filteredEntrySet.foreach(entries.remove(_))
	  map -= w
	  true
	} else false
  }
	
  override def words:Seq[String] = map.keys.toSeq ++ map.values
  
  override def hasWord(word: String,lang: String):Boolean = lang match {
    case "pl" => map.keySet.contains(word)
    case "ns" => map.values.toSet.contains(word)
    case _ => false
  }
  
  override def tuples = map.keys.toSeq.map(w =>(w,map.getOrElse(w,w)))
  
  override def isEmpty = map.isEmpty;

  override def size:Int = map.size
  override def clear:Unit = map.clear
}