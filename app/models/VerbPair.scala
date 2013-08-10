package models

import logic.Verb
import logic.PLVerb
import logic.NSVerb
import logic.VerbException
import logic.NSTranslator
            
case class VerbPair(plInfRoot: String, plImpRoot: String, plPattern: String, plExceptions: Option[String],
                    nsInfRoot: String, nsImpRoot: String, nsPattern: String, nsExceptions: Option[String],
                    prefixes: String
				   )
  extends SpeechPartPair[Verb] {
  
  def pl:Verb = {
    val word = PLVerb.word(plInfRoot, plImpRoot, plPattern)
	plExceptions match {
      case Some(str) => VerbPair.parse(str).foreach(word.except)
      case None => 
	}
	return word
  }
  
  def ns:Verb = {
    val word = NSVerb.word(nsInfRoot, nsImpRoot, nsPattern)
	nsExceptions match {
      case Some(str) => VerbPair.parse(str).foreach(word.except)
      case None => 
	}
	return word    
  }
  
  private def prefixPair(plPrefix:String,nsPrefix:String):(Verb,Verb) = {
    val plInf = plPrefix + plInfRoot;
    val plImp = plPrefix + plImpRoot;
    val plEx = VerbPair.prefixExceptions(plExceptions,plPrefix)
    val nsInf = nsPrefix + nsInfRoot;
    val nsImp = nsPrefix + nsImpRoot;
    val nsEx = VerbPair.prefixExceptions(nsExceptions,nsPrefix)
    val pair = VerbPair(plInf,plImp,plPattern,plEx,nsInf,nsImp,nsPattern,nsEx,"")
    (pair.pl,pair.ns)
  }  
  
  private def prefixesAsSeq:Seq[(String,String)] = prefixes match {
    case "" => Nil
    case str => str.split(",").map( t => {
      val tab = t.split(":")
      (tab(0),tab(1))
    })
  }
  
  override def add():Seq[(String,String)] = {
    prefixesAsSeq.map( tuple => {
      val (plPrefix,nsPrefix) = tuple
      val (plWord,nsWord) = if(plPrefix.isEmpty()) (this.pl,this.ns) else prefixPair(plPrefix,nsPrefix)
      NSTranslator.add(plWord,nsWord);
      (plWord.mainRoot,nsWord.mainRoot)
    })
  }
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
  
  private def parse(exceptions: String):Seq[VerbException] = parse(exceptions,"")
  
  val allPrefixes = Seq(("",""),("po","po"),("za","s") )
}