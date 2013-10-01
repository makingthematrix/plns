package models

import logic.Verb
import logic.PLVerb
import logic.NSVerb
import logic.VerbException
import logic.NSTranslator
import scala.collection.mutable;
            
case class VerbPair(val plInfRoot: String,val plImpRoot: String,val plPattern: String,val plExceptions: Option[String],
                    val nsInfRoot: String,val nsImpRoot: String,val nsPattern: String,val nsExceptions: Option[String],
                    val prefixes: Option[String]) extends SpeechPartPair[Verb] {
  
  def pl:Verb = {
    val perfective = prefixes.getOrElse("").startsWith(VerbPair.perfectiveMarker)
    val word = PLVerb.word(plInfRoot, plImpRoot, plPattern, perfective)
	plExceptions match {
      case Some(str) => VerbPair.parse(str).foreach(word.except)
      case None => 
	}
	word
  }
  
  def ns:Verb = {
    val perfective = prefixes.getOrElse("").startsWith(VerbPair.perfectiveMarker)
    val word = NSVerb.word(nsInfRoot, nsImpRoot, nsPattern, perfective)
	nsExceptions match {
      case Some(str) => VerbPair.parse(str).foreach(word.except)
      case None => 
	}
	word    
  }
  
  private def prefixPair(plPrefix:String,nsPrefix:String,perfective:Boolean):(Verb,Verb) = {
    val plInf = plPrefix + plInfRoot;
    val plImp = plPrefix + plImpRoot;
    val plEx = VerbPair.prefixExceptions(plExceptions,plPrefix)
    val nsInf = nsPrefix + nsInfRoot;
    val nsImp = nsPrefix + nsImpRoot;
    val nsEx = VerbPair.prefixExceptions(nsExceptions,nsPrefix)
    val prefixes = (if(perfective) VerbPair.perfectiveMarker else "") + VerbPair.prefixSplitMarker
    val pair = VerbPair(plInf,plImp,plPattern,plEx,nsInf,nsImp,nsPattern,nsEx,Some(prefixes))
    (pair.pl,pair.ns)
  }  
  
  private def add(plPrefix: String,nsPrefix: String):(String,String) = {
    val (realPlPrefix,perfective) = if(!plPrefix.startsWith(VerbPair.perfectiveMarker)) (plPrefix,false)
    							    else (plPrefix.substring(1),true)
    val (plWord,nsWord) = if(realPlPrefix.isEmpty()) (this.pl,this.ns) 
    					  else prefixPair(realPlPrefix,nsPrefix,perfective)
    NSTranslator.add(plWord,nsWord);
    (plWord.mainRoot,nsWord.mainRoot)
  }
  
  override def add():Seq[(String,String)] = prefixes match {
    case Some(pre) => VerbPair.prefixesAsSeq(pre).map( tuple => add(tuple._1,tuple._2) )
    case None => Seq(add("",""))
  }
}

object VerbPair {
  // exceptions should be in the format "case1:word1,case2:word2,..."
  private def parse(exceptions: String,prefix:String) = {
    exceptions.split(",").map(str => {
      val t = str.split(":");
      val key = t(0)
      val word = t(1)
      VerbException(key,prefix+word);
    });
  }
  
  private def prefixExceptions(exceptions:Option[String],prefix:String):Option[String] = exceptions match {
    case Some(str) =>
      val parsed = VerbPair.parse(str,prefix)
      val serialized = parsed.map(ve => "" + ve.conjCase + prefixSplitMarker + ve.word)
      val reduced = serialized.reduce( (t1,t2) => t1 + "," + t2)
      Some(reduced)
    case None => None
  }
  
  private def prefixesAsSeq(prefixes: String):Seq[(String,String)] = prefixes match {
    case "" => Seq(("",""))
    case `prefixSplitMarker` => Seq(("",""))
    case str if str.contains(",") =>  {
      val tab = str.split(",")
      val chunks = tab.map(prefixesAsSeq(_))
      chunks.toSeq.flatten
    }
    case prefix if prefix.contains(prefixSplitMarker) => {
      val arr = prefix.split(prefixSplitMarker) 
      Seq((arr(0),arr(1)))
    }
    case other => throw new IllegalArgumentException("VerbPair.prefixesAsSeq, unable to parse: " + other)
  }
  
  private def parse(exceptions: String):Seq[VerbException] = parse(exceptions,"")
  
  val allPrefixes = Seq(("",""),("po","po"),("za","s"),("w","v"),("s","iz"),
      ("za","za"),("do","do"),("prze","pre"),("przy","pri"),("u","u"),("od","ot"),("wy","vy") )
  
  val perfectiveMarker = "*"
  val prefixSplitMarker = "_"
    
  def pp(from: String,to: String):String = pp(from,to,false)
  def pp(from: String,to: String,perfective: Boolean):String = 
    (if(perfective) perfectiveMarker else "") + from + prefixSplitMarker + to
}