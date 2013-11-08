package dbhelpers

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import play.api.Logger
import logic.AbstractDictionary
import logic.Root
import models.SpeechPartPair
import models.UninflectedPair
import models.AdverbPair
import models.AdjectivePair
import models.NounPair
import models.VerbPair
import logic.DictEntry
import logic.SpeechPart
import logic.Adverb

object DBParsers {
  val rootParser = {
    get[Long]("id") ~ 
    get[String]("root") ~
    get[String]("speechpart") ~ 
    get[String]("lang") ~
    get[Long]("speechpartid") map {
      case id~root~speechpart~lang~speechpartid =>
        Root(id, root, speechpart, lang, speechpartid)
    }
  }
  
  val dictEntryParser = {
    get[Long]("id") ~ 
    get[String]("fromword") ~
    get[String]("fromlang") ~
    get[String]("toword") ~
    get[String]("tolang") ~
    get[String]("caseid") ~
    get[String]("speechpart") ~
    get[Long]("speechpartid") map {
      case id~fromword~fromlang~toword~tolang~caseid~speechpart~speechpartid => 
        DictEntry(id, fromword, fromlang, toword, tolang, caseid, speechpart, speechpartid)
    }
  }
  
  val uninflectedPairParser = {
    get[Long]("id") ~ 
    get[String]("fromword") ~
    get[String]("toword") map {
      case id~fromword~toword => 
        UninflectedPair(id, fromword, toword) 
    }
  }
  
  val adverbPairParser = {
    get[Long]("id") ~
    get[String]("fromind") ~
    get[String]("fromcmp") ~
    get[String]("frommode") ~
    get[String]("toind") ~
    get[String]("tocmp") ~
    get[String]("cmpignored") map {
      case id~fromind~fromcmp~frommode~toind~tocmp~cmpignored => 
        AdverbPair(id, fromind, fromcmp, frommode, toind, tocmp, cmpignored) 
    }
  }
  
  val adjectivePairParser = {
    get[Long]("id") ~
    get[String]("fromind") ~
    get[String]("fromadvind") ~
    get[String]("fromcmp") ~
    get[String]("fromadvcmp") ~
    get[String]("frommode") ~
    get[String]("fromadvmode") ~
    get[Option[String]]("fromexceptions") ~
    get[String]("toind") ~
    get[String]("toadvind") ~
    get[String]("tocmp") ~
    get[String]("toadvcmp") ~
    get[Option[String]]("toexceptions") ~
    get[String]("cmpignored") map {
      case id~fromind~fromadvind~fromcmp~fromadvcmp~frommode~fromadvmode~fromexceptions~
           toind~toadvind~tocmp~toadvcmp~toexceptions~cmpignored => 
       AdjectivePair(id, fromind, fromadvind, fromcmp, fromadvcmp, frommode, fromadvmode, fromexceptions,
                          toind, toadvind, tocmp, toadvcmp, toexceptions, cmpignored) 
    }
  }
  
  val nounPairParser = {
    get[Long]("id") ~
    get[String]("fromstem") ~
    get[String]("frompattern") ~
    get[Option[String]]("fromexceptions") ~
    get[String]("tostem") ~
    get[String]("topattern") ~
    get[Option[String]]("toexceptions") ~
    get[String]("ignored") map {
      case id~fromstem~frompattern~fromexceptions~
           tostem~topattern~toexceptions~ignored => 
       NounPair(id, fromstem, frompattern, fromexceptions, tostem, topattern, toexceptions, ignored) 
    }
  }
  
  val verbPairParser = {
    get[Long]("id") ~
    get[String]("frominfstem") ~
    get[String]("fromimpstem") ~
    get[String]("frompattern") ~
    get[Option[String]]("fromexceptions") ~
    get[String]("toinfstem") ~
    get[String]("toimpstem") ~
    get[String]("topattern") ~
    get[Option[String]]("toexceptions") ~
    get[Option[String]]("prefixes") map {
      case id~frominfstem~fromimpstem~frompattern~fromexceptions~
           toinfstem~toimpstem~topattern~toexceptions~prefixes => 
        VerbPair(id, frominfstem, fromimpstem, frompattern, fromexceptions, toinfstem, toimpstem, topattern, toexceptions, prefixes) 
    }
  }
}

class DBDictionary extends AbstractDictionary {
  override def size = DB.withConnection {
    implicit c => {
      val cRow = SQL("select count(*) as c from dictentries").apply().head
      cRow[Long]("c")
    }  
  }
  
  override def clear:Unit = DB.withConnection {
    implicit c => {
      SQL("delete * from dictentries").execute()
      SQL("delete * from uninflectedpairs").execute()
      SQL("delete * from adverbpairs").execute()
      SQL("delete * from adjectivepairs").execute()
      SQL("delete * from nounpairs").execute()
      SQL("delete * from verbpairs").execute()
    }
  }
  
  override def isEmpty:Boolean = this.size == 0
    
  override def getTranslation(word: String) = DB.withConnection { 
    implicit c => {
      println("DBDictionary.getTranslation, word: " + word)
      val dictEntryRowOption = SQL("""
          select toword from dictentries 
          where fromword={word} and fromlang='pl'
      """).on('word -> word).apply().headOption
      dictEntryRowOption match {
        case Some(wordRow) => Some(wordRow[String]("toword"))
        case None => None
      }
    }
  }

  /** @todo This 'synchronized' is sooo going to be replaced by an Akka actor 
   *        And try to find a way not to use asInstanceOf
   */
  override def addPair[T <: SpeechPart[T]](pair: SpeechPartPair[T]) = DB.withConnection {
    implicit c => pair match {
      case un: UninflectedPair => this.synchronized { add(un) }
      case adv: AdverbPair => this.synchronized { add(adv) }
      case adj: AdjectivePair => this.synchronized { add(adj) }
      case noun: NounPair => this.synchronized { add(noun) }
      case verb: VerbPair => this.synchronized { add(verb) }
      case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
    }
  }
  
  override protected def removePair[T <: SpeechPart[T]](pair: SpeechPartPair[T]) = DB.withConnection {
    implicit c => pair match {
      case un: UninflectedPair => this.synchronized { remove(un).asInstanceOf[Option[SpeechPartPair[T]]] }
      case adv: AdverbPair => this.synchronized { remove(adv).asInstanceOf[Option[SpeechPartPair[T]]] }
      case adj: AdjectivePair => this.synchronized { remove(adj).asInstanceOf[Option[SpeechPartPair[T]]] }
      case noun: NounPair => this.synchronized { remove(noun).asInstanceOf[Option[SpeechPartPair[T]]] }
      case verb: VerbPair => this.synchronized { remove(verb).asInstanceOf[Option[SpeechPartPair[T]]] }
      case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
    }
  }
  
  override protected def removePair(speechPart: String, id: Long){
    val t = speechPart match {
      case "uninflected" => "uninflectedpairs"
      case "adverb" => "adverbpairs"
      case "adjective" => "adjectivepairs"
      case "noun" => "nounpairs"
      case "verb" => "verbpairs"
      case _ => throw new IllegalArgumentException("Unknown speech part: " + speechPart)
    } 
    
    DB.withConnection { 
      implicit c => SQL("delete from {table} where id={id}").on('table -> t, 'id -> id).executeUpdate()
    }
  }
  
  override protected def updatePair[T <: SpeechPart[T]](pair: SpeechPartPair[T]) = DB.withConnection {
    implicit c => pair match {
      case un: UninflectedPair => this.synchronized { update(un) }
      case adv: AdverbPair => this.synchronized { update(adv) }
      case adj: AdjectivePair => this.synchronized { update(adj) }
      case noun: NounPair => this.synchronized { update(noun) }
      case verb: VerbPair => this.synchronized { update(verb) }
      case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
    }
  }
  
  override def getPairById[T <: SpeechPart[T]](pair: SpeechPartPair[T]) = DB.withConnection {
    implicit c => pair match {
      case un: UninflectedPair => getById(un).asInstanceOf[Option[SpeechPartPair[T]]] 
      case adv: AdverbPair => getById(adv).asInstanceOf[Option[SpeechPartPair[T]]]
      case adj: AdjectivePair => getById(adj).asInstanceOf[Option[SpeechPartPair[T]]] 
      case noun: NounPair => getById(noun).asInstanceOf[Option[SpeechPartPair[T]]] 
      case verb: VerbPair => getById(verb).asInstanceOf[Option[SpeechPartPair[T]]] 
      case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
    }
  }
  
  override def getPairById[T <: SpeechPart[T]](speechPart: String, pairId: Long): Option[SpeechPartPair[T]] = DB.withConnection {
    implicit c => speechPart match {
      case "uninflected" => getById(new UninflectedPair(pairId,"","")).asInstanceOf[Option[SpeechPartPair[T]]]
      case "adverb" => getById(new AdverbPair(pairId, "", "", "", "", "", "")).asInstanceOf[Option[SpeechPartPair[T]]]
      case "adjective" => getById(new AdjectivePair(pairId, "", "", "", "", "", "", None, "", "", "", "", None, "")).asInstanceOf[Option[SpeechPartPair[T]]]
      case "noun" => getById(new NounPair(pairId, "", "", None, "", "", None, "")).asInstanceOf[Option[SpeechPartPair[T]]]
      case "verb" => getById(new VerbPair(pairId, "", "", "", None, "", "", "", None, None)).asInstanceOf[Option[SpeechPartPair[T]]]
      case _ => throw new IllegalArgumentException("Unknown speech part: " + speechPart)      
    }
  }

  override def getPairByContents[T <: SpeechPart[T]](pair: SpeechPartPair[T]) = DB.withConnection {
    implicit c => pair match {
      case un: UninflectedPair => getByContents(un).asInstanceOf[Option[SpeechPartPair[T]]]
      case adv: AdverbPair => getByContents(adv).asInstanceOf[Option[SpeechPartPair[T]]]
      case adj: AdjectivePair => getByContents(adj).asInstanceOf[Option[SpeechPartPair[T]]]
      case noun: NounPair => getByContents(noun).asInstanceOf[Option[SpeechPartPair[T]]]
      case verb: VerbPair => getByContents(verb).asInstanceOf[Option[SpeechPartPair[T]]]
      case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
    }
  }
  
  private def add(pair: UninflectedPair)(implicit c: java.sql.Connection) = {
	assert(pair.id == SpeechPartPair.noId)
	// returns the new id, generated for this pair
	val t = getByContents(pair)
    getByContents(pair) match { 
      case None => SQL("insert into uninflectedpairs (fromword, toword) values ({fromword}, {toword})").on(
                        'fromword -> pair.plWord,
                        'toword -> pair.nsWord
                   ).executeInsert(scalar[Long].single)
      case Some(p) => p.id
    }
  }
  
  private def remove(pair: UninflectedPair)(implicit c: java.sql.Connection) = {
    assert(pair.id != SpeechPartPair.noId)
    getById(pair) match{
      case Some(p) => SQL("delete from uninflectedpairs where id={id}").on('id -> pair.id).executeUpdate(); Some(p);
      case None => None
    }
  }
  
  private def update(pair: UninflectedPair)(implicit c: java.sql.Connection) {
    assert(pair.id != SpeechPartPair.noId)
    SQL("update uninflectedpairs set fromword={fromword}, toword={toword} where id={id}")
    .on('id -> pair.id, 'fromword -> pair.plWord, 'toword -> pair.nsWord)
    .executeUpdate()  
  }
  
  private def getById(pair: UninflectedPair)(implicit c: java.sql.Connection) = {
    val t = SQL("select id, fromword, toword from uninflectedpairs where id={id}")
            .on('id -> pair.id)
            .as(DBParsers.uninflectedPairParser *)
    if(t.isEmpty) None else Some(t(0))
  }
  
  private def getByContents(pair: UninflectedPair)(implicit c: java.sql.Connection) = {
    val t = SQL("select id, fromword, toword from uninflectedpairs where fromword={fromword}")
            .on('fromword -> pair.plWord)
            .as(DBParsers.uninflectedPairParser *)
    if(t.isEmpty) None else Some(t(0))
  } 
  
  private def add(pair: AdverbPair)(implicit c: java.sql.Connection) = {
	assert(pair.id == SpeechPartPair.noId)
	// returns the new id, generated for this pair
    getByContents(pair) match { 
      case None => SQL("""insert into adverbpairs (fromind, fromcmp, frommode, toind, tocmp, cmpignored) 
                        values ({fromind}, {fromcmp}, {frommode}, {toind}, {tocmp}, {cmpignored})""").on(
                        'fromind -> pair.plInd,
                        'fromcmp -> pair.plCmp,
                        'frommode -> pair.plMode,
                        'toind -> pair.nsInd,
                        'tocmp -> pair.nsCmp,
                        'cmpignored -> pair.cmpIgnored
                   ).executeInsert(scalar[Long].single)
      case Some(p) => p.id
    }
  }

  private def remove(pair: AdverbPair)(implicit c: java.sql.Connection) = {
    assert(pair.id != SpeechPartPair.noId)
    getById(pair) match{
      case Some(p) => SQL("delete from adverbpairs where id={id}").on('id -> pair.id).executeUpdate(); Some(p);
      case None => None
    }
  }

  private def update(pair: AdverbPair)(implicit c: java.sql.Connection) {
	assert(pair.id != SpeechPartPair.noId)
    SQL("""update adverbpairs 
           set fromind={fromind}, fromcmp={fromcmp}, frommode={frommode},
               toind={toind}, tocmp={tocmp}, cmpignored={cmpignored}
           where id={id}""")
    .on('id -> pair.id, 'fromind -> pair.plInd, 'fromcmp -> pair.plCmp, 'frommode -> pair.plMode,
        'toind -> pair.nsInd, 'tocmp -> pair.nsCmp, 'cmpignored -> pair.cmpIgnored)
    .executeUpdate()
  }
  
  private def getById(pair: AdverbPair)(implicit c: java.sql.Connection) = {
    val t = SQL("""select id, fromind, fromcmp, frommode, toind, tocmp, cmpignored 
                   from adverbpairs where id={id}""")
            .on('id -> pair.id)
            .as(DBParsers.adverbPairParser *)
    if(t.isEmpty) None else Some(t(0))
  }
  
  private def getByContents(pair: AdverbPair)(implicit c: java.sql.Connection) = {
    val t = SQL("""select id, fromind, fromcmp, frommode, toind, tocmp, cmpignored 
                   from adverbpairs where fromind={fromind} and fromcmp={fromcmp}""")
            .on('fromind -> pair.plInd, 'fromcmp -> pair.plCmp)
            .as(DBParsers.adverbPairParser *)
    if(t.isEmpty) None else Some(t(0))
  } 
  
  private def add(pair: AdjectivePair)(implicit c: java.sql.Connection) = {
	assert(pair.id == SpeechPartPair.noId)
	// returns the new id, generated for this pair
    getByContents(pair) match { 
      case None => SQL("""insert into adjectivepairs (fromind, fromadvind, fromcmp, fromadvcmp, frommode, fromadvmode, 
                                                      fromexceptions,
                                                      toind, toadvind, tocmp, toadvcmp, toexceptions, cmpignored) 
                        values ({fromind}, {fromadvind}, {fromcmp}, {fromadvcmp}, {frommode}, {fromadvmode}, {fromexceptions}, 
                                {toind}, {toadvind}, {tocmp}, {toadvcmp}, {toexceptions}, {cmpignored})""").on(
                        'fromind -> pair.plInd,
                        'fromadvind -> pair.plAdvInd,
                        'fromcmp -> pair.plCmp,
                        'fromadvcmp -> pair.plAdvCmp,
                        'frommode -> pair.plMode,
                        'fromadvmode -> pair.plAdvMode,
                        'fromexceptions -> pair.plExceptions,
                        'toind -> pair.nsInd,
                        'toadvind -> pair.nsAdvInd,
                        'tocmp -> pair.nsCmp,
                        'toadvcmp -> pair.nsAdvCmp,
                        'toexceptions -> pair.nsExceptions,
                        'cmpignored -> pair.cmpIgnored
                   ).executeInsert(scalar[Long].single)
      case Some(p) => p.id
    }
  }
  
  private def remove(pair: AdjectivePair)(implicit c: java.sql.Connection) = {
    assert(pair.id != SpeechPartPair.noId)
    getById(pair) match{
      case Some(p) => SQL("delete from adjectivepairs where id={id}").on('id -> pair.id).executeUpdate(); Some(p);
      case None => None
    }
  }

  private def update(pair: AdjectivePair)(implicit c: java.sql.Connection) {
	assert(pair.id != SpeechPartPair.noId)
    SQL("""update adjectivepairs 
           set fromind={fromind}, fromadvind={fromadvind}, fromcmp={fromcmp}, fromadvcmp={fromadvcmp},
               frommode={frommode}, fromadvmode={fromadvmode}, fromexceptions={fromexceptions},
               toind={toind}, toadvind={toadvind}, tocmp={tocmp}, toadvcmp={toadvcmp}, 
               toexceptions={toexceptions}, cmpignored={cmpignored}
           where id={id}""")
    .on('id -> pair.id,
        'fromind -> pair.plInd,
        'fromadvind -> pair.plAdvInd,
        'fromcmp -> pair.plCmp,
        'fromadvcmp -> pair.plAdvCmp,
        'frommode -> pair.plMode,
        'fromadvmode -> pair.plAdvMode,
        'fromexceptions -> pair.plExceptions,
        'toind -> pair.nsInd,
        'toadvind -> pair.nsAdvInd,
        'tocmp -> pair.nsCmp,
        'toadvcmp -> pair.nsAdvCmp,
        'toexceptions -> pair.nsExceptions,
        'cmpignored -> pair.cmpIgnored)
    .executeUpdate()  
  }  
  
  private def getById(pair: AdjectivePair)(implicit c: java.sql.Connection) = {
    val t = SQL("""select id, fromind, fromadvind, fromcmp, fromadvcmp, frommode, fromadvmode, fromexceptions,
                          toind, toadvind, tocmp, toadvcmp, toexceptions, cmpignored
                   from adjectivepairs where id={id}""")
            .on('id -> pair.id)
            .as(DBParsers.adjectivePairParser *)
    if(t.isEmpty) None else Some(t(0))
  }  
  
  private def getByContents(pair: AdjectivePair)(implicit c: java.sql.Connection) = {
    val t = SQL("""select id, fromind, fromadvind, fromcmp, fromadvcmp, frommode, fromadvmode, fromexceptions,
                          toind, toadvind, tocmp, toadvcmp, toexceptions, cmpignored 
                   from adjectivepairs 
                   where fromind={fromind} and fromcmp={fromcmp}""")
            .on('fromind -> pair.plInd, 'fromcmp -> pair.plCmp)
            .as(DBParsers.adjectivePairParser *)
    if(t.isEmpty) None else Some(t(0))
  } 
  
  private def add(pair: NounPair)(implicit c: java.sql.Connection) = {
	assert(pair.id == SpeechPartPair.noId)
	// returns the new id, generated for this pair
    getByContents(pair) match { 
      case None => SQL("""insert into nounpairs (fromstem, frompattern, fromexceptions, tostem, topattern, toexceptions, ignored) 
                        values ({fromstem}, {frompattern}, {fromexceptions}, {tostem}, {topattern}, {toexceptions}, {ignored})""")
                   .on('fromstem -> pair.plStem,
                       'frompattern -> pair.plPattern,
                       'fromexceptions -> pair.plExceptions,
                       'tostem -> pair.nsStem,
                       'topattern -> pair.nsPattern,
                       'toexceptions -> pair.nsExceptions,
                       'ignored -> pair.ignored)
                   .executeInsert(scalar[Long].single)
      case Some(p) => p.id
    }
  }
  
  private def remove(pair: NounPair)(implicit c: java.sql.Connection) = {
    assert(pair.id != SpeechPartPair.noId)
    getById(pair) match{
      case Some(p) => SQL("delete from nounpairs where id={id}").on('id -> pair.id).executeUpdate(); Some(p);
      case None => None
    }
  }

  private def update(pair: NounPair)(implicit c: java.sql.Connection) {
	assert(pair.id != SpeechPartPair.noId)
    SQL("""update nounpairs 
           set fromstem={fromstem}, frompattern={frompattern}, fromexceptions={fromexceptions},
               tostem={tostem}, topattern={topattern}, toexceptions={toexceptions}, ignored={ignored}
           where id={id}""")
    .on('id -> pair.id,
        'fromstem -> pair.plStem,
        'frompattern -> pair.plPattern,
        'fromexceptions -> pair.plExceptions,
        'tostem -> pair.nsStem,
        'topattern -> pair.nsPattern,
        'toexceptions -> pair.nsExceptions,
        'ignored -> pair.ignored)
    .executeUpdate() 
  }  
  
  private def getById(pair: NounPair)(implicit c: java.sql.Connection) = {
    val t = SQL("""select id, fromstem, frompattern, fromexceptions, 
                          tostem, topattern, toexceptions, ignored
                   from nounpairs where id={id}""")
            .on('id -> pair.id)
            .as(DBParsers.nounPairParser *)
    if(t.isEmpty) None else Some(t(0))
  }
  
  private def getByContents(pair: NounPair)(implicit c: java.sql.Connection) = {
    val t = SQL("""select id, fromstem, frompattern, fromexceptions, 
                          tostem, topattern, toexceptions, ignored 
                   from nounpairs 
                   where fromstem={fromstem} and frompattern={frompattern}""").on(
        'fromstem -> pair.plStem, 
        'frompattern -> pair.plPattern
    ).as(DBParsers.nounPairParser *)
    
    if(t.isEmpty) None else Some(t(0))
  }
  
  private def add(pair: VerbPair)(implicit c: java.sql.Connection) = {
	assert(pair.id == SpeechPartPair.noId)
	// returns the new id, generated for this pair
    getByContents(pair) match { 
      case None => SQL("""insert into verbpairs 
                          (frominfstem, fromimpstem, frompattern, fromexceptions, 
                           toinfstem, toimpstem, topattern, toexceptions, prefixes) 
                          values 
                          ({frominfstem}, {fromimpstem}, {frompattern}, {fromexceptions}, 
                           {toinfstem}, {toimpstem}, {topattern}, {toexceptions}, {prefixes})""")
                   .on('frominfstem -> pair.plInfStem,
                       'fromimpstem -> pair.plImpStem,
                       'frompattern -> pair.plPattern,
                       'fromexceptions -> pair.plExceptions,
                       'toinfstem -> pair.nsInfStem,
                       'toimpstem -> pair.nsImpStem,
                       'topattern -> pair.nsPattern,
                       'toexceptions -> pair.nsExceptions,
                       'prefixes -> pair.prefixes)
                   .executeInsert(scalar[Long].single)
      case Some(p) => p.id
    }
  }

  private def remove(pair: VerbPair)(implicit c: java.sql.Connection) = {
    assert(pair.id != SpeechPartPair.noId)
    getById(pair) match{
      case Some(p) => SQL("delete from verbpairs where id={id}").on('id -> pair.id).executeUpdate(); Some(p);
      case None => None
    }
  }
  
  private def update(pair: VerbPair)(implicit c: java.sql.Connection) {
	assert(pair.id != SpeechPartPair.noId)
    SQL("""update verbpairs 
           set frominfstem={frominfstem}, fromimpstem={fromimpstem}, frompattern={frompattern}, 
               fromexceptions={fromexceptions},
               toinfstem={toinfstem}, toimpstem={toimpstem}, topattern={topattern}, 
               toexceptions={toexceptions}, prefixes={prefixes}
           where id={id}""")
    .on('id -> pair.id, 
        'frominfstem -> pair.plInfStem,
        'fromimpstem -> pair.plImpStem,
        'frompattern -> pair.plPattern,
        'fromexceptions -> pair.plExceptions,
        'toinfstem -> pair.nsInfStem,
        'toimpstem -> pair.nsImpStem,
        'topattern -> pair.nsPattern,
        'toexceptions -> pair.nsExceptions,
        'prefixes -> pair.prefixes)
    .executeUpdate()
  }  
  
  private def getById(pair: VerbPair)(implicit c: java.sql.Connection) = {
    val t = SQL("""select id, frominfstem, fromimpstem, frompattern, fromexceptions, 
                          toinfstem, toimpstem, topattern, toexceptions, prefixes
                   from verbpairs where id={id}""")
            .on('id -> pair.id)
            .as(DBParsers.verbPairParser *)
    if(t.isEmpty) None else Some(t(0))
  }
  
  private def getByContents(pair: VerbPair)(implicit c: java.sql.Connection) = {
    val t = SQL("""select id, frominfstem, fromimpstem, frompattern, fromexceptions, 
                          toinfstem, toimpstem, topattern, toexceptions, prefixes
                   from verbpairs 
                   where frominfstem={frominfstem} and fromimpstem={fromimpstem}""").on(
        'frominfstem -> pair.plInfStem, 
        'fromimpstem -> pair.plImpStem
    ).as(DBParsers.verbPairParser *)
    
    if(t.isEmpty) None else Some(t(0))
  }  
  
  def listPairs: Seq[SpeechPartPair[_ <: SpeechPart[_]]] = DB.withConnection {
    implicit c => {
      val uninflecteds = SQL("select id, fromword, toword from uninflectedpairs")
                         .as(DBParsers.uninflectedPairParser *)
      val adverbs = SQL("select id, fromind, fromcmp, frommode, toind, tocmp, cmpignored from adverbpairs")
                    .as(DBParsers.adverbPairParser *)
      val adjectives = SQL("""select id, fromind, fromadvind, fromcmp, fromadvcmp, frommode, fromadvmode, fromexceptions,
                              toind, toadvind, tocmp, toadvcmp, toexceptions, cmpignored from adjectivepairs """)
                       .as(DBParsers.adjectivePairParser *)
      val nouns = SQL("""select id, fromstem, frompattern, fromexceptions, 
                          tostem, topattern, toexceptions, ignored from nounpairs""")
                  .as(DBParsers.nounPairParser *)
      val verbs = SQL("""select id, frominfstem, fromimpstem, frompattern, fromexceptions, 
                          toinfstem, toimpstem, topattern, toexceptions, prefixes from verbpairs""")
                  .as(DBParsers.verbPairParser *)
      uninflecteds ++ adverbs ++ adjectives ++ nouns ++ verbs
    }
  }
  
  override protected def addEntry(entry: DictEntry) = DB.withConnection {
    assert(entry.id == DictEntry.noId)
    implicit c => add(entry) 
  }
  
  override def addEntries(entries: Seq[DictEntry]) = DB.withConnection {
    implicit c => entries.foreach( e => {  
      assert(e.id == DictEntry.noId) 
      add(e) 
    })
  }
  
  private def add(entry: DictEntry)(implicit c: java.sql.Connection) = getByContents(entry) match { 
    case None => SQL("""insert into dictentries (fromword, fromlang, toword, tolang, caseid, speechpart, speechpartid) 
                        values ({fromword}, {fromlang}, {toword}, {tolang}, {caseid}, {speechpart}, {speechpartid})""")
                  .on('fromword -> entry.plWord,
                      'fromlang -> entry.plLang,
                      'toword -> entry.nsWord,
                      'tolang -> entry.nsLang,
                      'caseid -> entry.caseId,
                      'speechpart -> entry.speechPart.toString(),
                      'speechpartid -> entry.speechPartId)
                  .executeInsert(scalar[Long].single)
    case Some(e) => e.id
  }
  
  override def getEntryByContents(entry: DictEntry) = DB.withConnection {
    implicit c => getByContents(entry)
  }
  
  override def getWord(word: String, lang: String) = DB.withConnection {
    implicit c => getByContents(new DictEntry(word,lang,"",""))
  }
  
  private def getByContents(entry: DictEntry)(implicit c: java.sql.Connection): Option[DictEntry] = {
    val t = SQL("""select id, fromword, fromlang, toword, tolang, caseid, speechpart, speechpartid from dictentries
                   where fromword={fromword} and fromlang={fromlang}""")
            .on('fromword -> entry.plWord, 'fromlang -> entry.plLang, 'speechpart -> entry.speechPart.toString)
            .as(DBParsers.dictEntryParser *)
    if(t.isEmpty) None else Some(t(0))
  }
  
  override def getEntryById(id: Long) = DB.withConnection {
    implicit c => getEntryByIdPriv(id)
  }
  
  private def getEntryByIdPriv(id: Long)(implicit c: java.sql.Connection) = {
    val t = SQL("""select id, fromword, fromlang, toword, tolang, caseid, speechpart, speechpartid from dictentries
                   where id={id}""")
            .on('id -> id)
            .as(DBParsers.dictEntryParser *)
    if(t.isEmpty) None else Some(t(0))
  }
  
  override protected def removeEntry(id: Long) = DB.withConnection {
    implicit c => remove(id)
  }
  
  private def remove(id: Long)(implicit c: java.sql.Connection) = {
    assert(id != DictEntry.noId)
    getEntryByIdPriv(id) match{
      case Some(e) => SQL("delete from dictentries where id={id}").on('id -> id).executeUpdate(); Some(e);
      case None => None
    }
  }
  
  override protected def removeEntries(speechPart: String, pairId: Long) = DB.withConnection {
    implicit c => SQL("delete from dictentries where speechpart={speechpart} and speechpartid={pairid}")
                  .on('speechpart -> speechPart, 'speechpartid -> pairId)
                  .executeUpdate()
  }
  
  override protected def updateEntry(entry: DictEntry) = DB.withConnection {
    implicit c => update(entry)
  }
  
  
  private def update(entry: DictEntry)(implicit c: java.sql.Connection){
   	assert(entry.id != DictEntry.noId)
    SQL("""update dicentries
           set id={id}, fromword={fromword}, fromlang={fromlang},
               toword={toword}, tolang={tolang}, caseid={caseid},
               speechpard={speechpart}, speechpartid={speechpartid}
           where id={id}""")
    .on('id -> entry.id,
        'fromword -> entry.plWord,
        'fromlang -> entry.plLang,
        'toword -> entry.nsWord,
        'tolang -> entry.nsLang,
        'caseid -> entry.caseId,
        'speechpart -> entry.speechPart.toString(),
        'speechpartid -> entry.speechPartId)
    .executeUpdate()
  }
  
  override def addRoot(root: Root): Long = DB.withConnection {
    implicit c => getRootByIdPriv(root.id) match {
      case Some(r) => r.id
      case None => SQL("""insert into roots (root, speechpart, lang, speechpartid) 
                          values ({root}, {speechpart}, {lang}, {speechpartid})""")
                   .on('root -> root.root,'speechpart -> root.speechPart.toString(), 
                       'lang -> root.lang, 'speechpartid -> root.speechPartId)
                   .executeInsert(scalar[Long].single)
    }
  }
  
  override protected def removeRoots(speechPart: String, pairId: Long) = DB.withConnection {
    implicit c => SQL("delete from roots where speechpart={speechpart} and speechpartid={speechpartid}")
                  .on('speechpart -> speechPart, 'speechpartid -> pairId)
                  .executeUpdate()
  }

  def getRootByWord(root: String) = DB.withConnection {
    implicit c => {
      val t = SQL("select id, root, speechpart, lang, speechpartid from roots where root={root}")
              .on('root -> root)
              .as(DBParsers.rootParser *)
       if(t.isEmpty) None else Some(t(0))
    }
  }
  
  def getRootById(id: Long) = DB.withConnection {
    implicit c => getRootByIdPriv(id)
  }
    
  private def getRootByIdPriv(id: Long)(implicit c: java.sql.Connection) = {
    val t = SQL("select id, root, speechpart, lang, speechpartid from roots where id={id}")
            .on('id -> id)
            .as(DBParsers.rootParser *)
    if(t.isEmpty) None else Some(t(0))
  }
  
  override def roots:Seq[Root] = DB.withConnection {
    implicit c => SQL("select id, root, speechpart, lang, speechpartid from roots").as(DBParsers.rootParser *)
  }

}