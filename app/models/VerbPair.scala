package models

import logic.Verb
import logic.PLVerb
import logic.NSVerb
import logic.VerbException
import logic.NSTranslator
import scala.collection.mutable;
            
case class VerbPair(plInfRoot: String, plImpRoot: String, plPattern: String, plExceptions: Option[String],
                    nsInfRoot: String, nsImpRoot: String, nsPattern: String, nsExceptions: Option[String],
                    prefixes: String
				   ) extends SpeechPartPair[Verb] {
  private var perfective = false;
  
  def pl:Verb = {
    val word = PLVerb.word(plInfRoot, plImpRoot, plPattern, perfective)
	plExceptions match {
      case Some(str) => VerbPair.parse(str).foreach(word.except)
      case None => 
	}
	word
  }
  
  def ns:Verb = {
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
    val prefixes = if(perfective) "P:" else ":"
    val pair = VerbPair(plInf,plImp,plPattern,plEx,nsInf,nsImp,nsPattern,nsEx,prefixes)
    pair.perfective = perfective
    (pair.pl,pair.ns)
  }  
  
  private def add(plPrefix: String,nsPrefix: String):(String,String) = {
    val (realPlPrefix,perfective) = if(plPrefix.isEmpty() || !plPrefix.startsWith("P")) (plPrefix,false)
    							    else (plPrefix.substring(1),true) 
    val (plWord,nsWord) = if(realPlPrefix.isEmpty()) {
      this.perfective = perfective
      (this.pl,this.ns) 
    } else prefixPair(realPlPrefix,nsPrefix,perfective)
    NSTranslator.add(plWord,nsWord);
    (plWord.mainRoot,nsWord.mainRoot)
  }
  
  override def add():Seq[(String,String)] = VerbPair.prefixesAsSeq(prefixes).map( tuple => add(tuple._1,tuple._2) )
}

object VerbPair {
  // exceptions should be in the format "case1:word1,case2:word2,..."
  private def parse(exceptions: String,prefix:String) = {
    val log = play.Logger.of("application")
    log.info("parsing exceptions!: " + exceptions)
    exceptions.split(",").map(str => {
      log.debug("trying to parse exception: " + str)
      println(str)
      val t = str.split(":");
      val key = t(0)
      val word = t(1)
      new VerbException(key,prefix+word);
    });
  }
  
  private def prefixExceptions(exceptions:Option[String],prefix:String):Option[String] = exceptions match {
    case Some(str) =>
      val parsed = VerbPair.parse(str,prefix)
      val serialized = parsed.map(ve => "" + ve.conjCase + ":" + ve.word)
      val reduced = serialized.reduce( (t1,t2) => t1 + "," + t2)
      Some(reduced)
    case None => None
  }
  
  private def prefixesAsSeq(prefixes: String):Seq[(String,String)] = prefixes match {
    case "" => Seq(("",""))
    case ":" => Seq(("",""))
    case str if str.contains(",") =>  {
      val tab = str.split(',')
      val chunks = tab.map(prefixesAsSeq(_))
      chunks.toSeq.flatten
    }
    case prefix if prefix.contains(":") => {
      val arr = prefix.split(':') 
      Seq((arr(0),arr(1)))
    }
    case other => throw new IllegalArgumentException("VerbPair.prefixesAsSeq, unable to parse: " + other)
  }
  
  private def parse(exceptions: String):Seq[VerbException] = parse(exceptions,"")
  
  val allPrefixes = Seq(("",""),("po","po"),("za","s"),("w","v"),("s","iz"),
      ("za","za"),("do","do"),("prze","pre"),("przy","pri"),("u","u"),("od","ot"),("wy","vy") )
}