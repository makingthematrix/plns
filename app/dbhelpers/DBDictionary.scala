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

object DBParsers {
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
    get[String]("fromexceptions") ~
    get[String]("toind") ~
    get[String]("toadvind") ~
    get[String]("tocmp") ~
    get[String]("toadvcmp") ~
    get[String]("toexceptions") ~
    get[String]("cmpignored") map {
      case id~fromind~fromadvind~fromcmp~fromadvcmp~frommode~fromadvmode~fromexceptions~
           toind~toadvind~tocmp~toadvcmp~toexceptions~cmpignored => {
        val fromExceptions = if(fromexceptions.isEmpty()) None else Some(fromexceptions)
        val toExceptions = if(toexceptions.isEmpty()) None else Some(toexceptions)
        AdjectivePair(id, fromind, fromadvind, fromcmp, fromadvcmp, frommode, fromadvmode, fromExceptions,
                          toind, toadvind, tocmp, toadvcmp, toExceptions, cmpignored) 
      }
    }
  }
  
  val nounPairParser = {
    get[Long]("id") ~
    get[String]("fromstem") ~
    get[String]("frompattern") ~
    get[String]("fromexceptions") ~
    get[String]("tostem") ~
    get[String]("topattern") ~
    get[String]("toexceptions") ~
    get[String]("ignored") map {
      case id~fromstem~frompattern~fromexceptions~
           tostem~topattern~toexceptions~ignored => {
        val fromExceptions = if(fromexceptions.isEmpty()) None else Some(fromexceptions)
        val toExceptions = if(toexceptions.isEmpty()) None else Some(toexceptions)
        NounPair(id, fromstem, frompattern, fromExceptions, tostem, topattern, toExceptions, ignored) 
      }
    }
  }
  
  val verbPairParser = {
    get[Long]("id") ~
    get[String]("frominfstem") ~
    get[String]("fromimpstem") ~
    get[String]("frompattern") ~
    get[String]("fromexceptions") ~
    get[String]("toinfstem") ~
    get[String]("toimpstem") ~
    get[String]("topattern") ~
    get[String]("toexceptions") ~
    get[String]("prefixes") map {
      case id~frominfstem~fromimpstem~frompattern~fromexceptions~
           toinfstem~toimpstem~topattern~toexceptions~prefixes => {
        val fromExceptions = if(fromexceptions.isEmpty()) None else Some(fromexceptions)
        val toExceptions = if(toexceptions.isEmpty()) None else Some(toexceptions)
        val pref = if(prefixes.isEmpty()) None else Some(prefixes)
        VerbPair(id, frominfstem, fromimpstem, frompattern, fromExceptions, toinfstem, toimpstem, topattern, toExceptions, pref) 
      }
    }
  }
}

class DBDictionary extends AbstractDictionary {
  override def size:Int = DB.withConnection {
    implicit c => {
      val cRow = SQL("select count(*) as c from dictentries").apply().head
      cRow[Int]("c")
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
    
  override def getTranslation(word: String):Option[String] = DB.withConnection { 
    implicit c => {
      println("DBDictionary.getTranslation, word: " + word)
      val dictEntryRowOption = SQL("""
          select toword from dictentries 
          where fromword={word} lang='pl');
      """).on('word -> word).apply().headOption
      dictEntryRowOption match {
        case Some(wordRow) => Some(wordRow[String]("word"))
        case None => None
      }
    }
  }

  override def addPair[T](pair: SpeechPartPair[T]): Long = DB.withConnection {
    implicit c => pair match {
      case un: UninflectedPair => this.synchronized { add(un) }
      case adv: AdverbPair => this.synchronized { add(adv) }
      case adj: AdjectivePair => this.synchronized { add(adj) }
      case noun: NounPair => this.synchronized { add(noun) }
      case verb: VerbPair => this.synchronized { add(verb) }
      case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
    }
  }
  
  override def removePair[T](pair: SpeechPartPair[T]): Option[SpeechPartPair[T]] = DB.withConnection {
    implicit c => pair match {
      case un: UninflectedPair => this.synchronized { remove(un) }
      case adv: AdverbPair => this.synchronized { remove(adv) }
      case adj: AdjectivePair => this.synchronized { remove(adj) }
      case noun: NounPair => this.synchronized { remove(noun) }
      case verb: VerbPair => this.synchronized { remove(verb) }
      case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
    }
  }
  
  override def updatePair[T](pair: SpeechPartPair[T]): Unit = DB.withConnection {
    implicit c => pair match {
      case un: UninflectedPair => this.synchronized { update(un) }
      case adv: AdverbPair => this.synchronized { update(adv) }
      case adj: AdjectivePair => this.synchronized { update(adj) }
      case noun: NounPair => this.synchronized { update(noun) }
      case verb: VerbPair => this.synchronized { update(verb) }
      case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
    }
  }
  
  override def getById[T](pair: SpeechPartPair[T]): SpeechPartPair[T] = DB.withConnection {
    implicit c => pair match {
      case un: UninflectedPair => getById(un) 
      case adv: AdverbPair => getById(adv) 
      case adj: AdjectivePair => getById(adj) 
      case noun: NounPair => getById(noun) 
      case verb: VerbPair => getById(verb) 
      case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
    }
  }

  override def getByContents[T](pair: SpeechPartPair[T]): SpeechPartPair[T] = DB.withConnection {
    implicit c => pair match {
      case un: UninflectedPair => getByContents(un) 
      case adv: AdverbPair => getByContents(adv) 
      case adj: AdjectivePair => getByContents(adj) 
      case noun: NounPair => getByContents(noun) 
      case verb: VerbPair => getByContents(verb) 
      case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
    }
  }
  
  private def add(pair: UninflectedPair)(implicit c: java.sql.Connection): Long = {
	assert(pair.id == SpeechPartPair.noId)
	// returns the new id, generated for this pair
    getByContents(pair) match { 
      case None => SQL("insert into uninflectedpairs (fromword, toword) values ({fromword}, {toword})").on(
                        'fromword -> pair.plWord,
                        'toword -> pair.nsWord
                   ).executeInsert(scalar[Long].single)
      case Some(p) => p.id
    }
  }
  
  private def remove(pair: UninflectedPair)(implicit c: java.sql.Connection): Option[UninflectedPair] =  {
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
  
  private def getById(pair: UninflectedPair)(implicit c: java.sql.Connection): Option[UninflectedPair] = {
    val t = SQL("select id, fromword, toword from uninflectedpairs where id={id}")
            .on('id -> pair.id)
            .as(DBParsers.uninflectedPairParser *)
    if(t.isEmpty) None else Some(t(0))
  }
  
  private def getByContents(pair: UninflectedPair)(implicit c: java.sql.Connection): Option[UninflectedPair] = {
    val t = SQL("select id, fromword, toword from uninflectedpairs where fromword={fromword}")
            .on('fromword -> pair.plWord)
            .as(DBParsers.uninflectedPairParser *)
    if(t.isEmpty) None else Some(t(0))
  } 
  
  private def add(pair: AdverbPair)(implicit c: java.sql.Connection): Long = {
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

  private def remove(pair: AdverbPair)(implicit c: java.sql.Connection): Option[AdverbPair] = {
    assert(pair.id != SpeechPartPair.noId)
    getById(pair) match{
      case Some(p) => SQL("delete from adverbpairs where id={id}").on('id -> pair.id).executeUpdate(); Some(p);
      case None => None
    }
  }

  private def update(pair: AdverbPair)(implicit c: java.sql.Connection) {
	assert(pair.id != SpeechPartPair.noId)
    SQL("""update adverbpairs 
           set fromind={fromind}, fromcmp={fomcmp}, frommode={frommode},
               toind={toind}, tocmp={tocmp}, cmpignored={cmpignored}
           where id={id}""")
    .on('id -> pair.id, 'fromind -> pair.plInd, 'fromcmp -> pair.plCmp, 'frommode -> pair.plMode,
        'toind -> pair.nsInd, 'tocmp -> pair.nsCmp, 'cmpignored -> pair.cmpIgnored)
    .executeUpdate()
  }
  
  private def getById(pair: AdverbPair)(implicit c: java.sql.Connection): Option[AdverbPair] = {
    val t = SQL("""select id, fromind, fromcmp, frommode, toind, tocmp, cmpignored 
                   from adverbpairs where id={id}""")
            .on('id -> pair.id)
            .as(DBParsers.adverbPairParser *)
    if(t.isEmpty) None else Some(t(0))
  }
  
  private def getByContents(pair: AdverbPair)(implicit c: java.sql.Connection): Option[AdverbPair] = {
    val t = SQL("""select id, fromind, fromcmp, frommode, toind, tocmp, cmpignored 
                   from adverbpairs where fromind={fromind} and fromcmp={fromcmp}""")
            .on('fromind -> pair.plInd, 'fromcmp -> pair.plCmp)
            .as(DBParsers.adverbPairParser *)
    if(t.isEmpty) None else Some(t(0))
  } 
  
  private def add(pair: AdjectivePair)(implicit c: java.sql.Connection): Long = {
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
  
  private def remove(pair: AdjectivePair)(implicit c: java.sql.Connection): Option[AdjectivePair] = {
    assert(pair.id != SpeechPartPair.noId)
    getById(pair) match{
      case Some(p) => SQL("delete from adjectivepairs where id={id}").on('id -> pair.id).executeUpdate(); Some(p);
      case None => None
    }
  }

  private def update(pair: AdjectivePair)(implicit c: java.sql.Connection) {
	assert(pair.id != SpeechPartPair.noId)
    SQL("""update adjectivepairs 
           set fromind={fromind}, fromadvind={fromadvind}, fromcmp={fomcmp}, fromadvcmp={fromadvcmp},
               frommode={frommode}, fromadvmode={fromadvmode}, fromexceptions={fromexceptions},
               toind={toind}, toadvind={toadvind}, tocmp={tocmp}, toadvcmp={toadvcmp}, 
               toexceptions={toexceptions}, cmpignored={cmpignored}
           where id={id}""")
    .on('fromind -> pair.plInd,
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
  
  private def getById(pair: AdjectivePair)(implicit c: java.sql.Connection): Option[AdjectivePair] = {
    val t = SQL("""select id, fromind, fromadvind, fromcmp, fromadvcmp, frommode, fromadvmode, fromexceptions,
                          toind, toadvind, tocmp, toadvcmp, toexceptions, cmpignored
                   from adjectivepairs where id={id}""")
            .on('id -> pair.id)
            .as(DBParsers.adjectivePairParser *)
    if(t.isEmpty) None else Some(t(0))
  }  
  
  private def getByContents(pair: AdjectivePair)(implicit c: java.sql.Connection): Option[AdjectivePair] = {
    val t = SQL("""select id, fromind, fromadvind, fromcmp, fromadvcmp, frommode, fromadvmode, fromexceptions,
                          toind, toadvind, tocmp, toadvcmp, toexceptions, cmpignored 
                   from adjectivepairs 
                   where fromind={fromind} and fromcmp={fromcmp}""")
            .on('fromind -> pair.plInd, 'fromcmp -> pair.plCmp)
            .as(DBParsers.adjectivePairParser *)
    if(t.isEmpty) None else Some(t(0))
  } 
  
  private def add(pair: NounPair)(implicit c: java.sql.Connection): Long = {
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
  
  private def remove(pair: NounPair)(implicit c: java.sql.Connection): Option[NounPair] = {
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
    .on('fromstem -> pair.plStem,
        'frompattern -> pair.plPattern,
        'fromexceptions -> pair.plExceptions,
        'tostem -> pair.nsStem,
        'topattern -> pair.nsPattern,
        'toexceptions -> pair.nsExceptions,
        'ignored -> pair.ignored)
    .executeUpdate() 
  }  
  
  private def getById(pair: NounPair)(implicit c: java.sql.Connection): Option[NounPair] = {
    val t = SQL("""select id, fromstem, frompattern, fromexceptions, 
                          tostem, topattern, toexceptions, ignored
                   from nounpairs where id={id}""")
            .on('id -> pair.id)
            .as(DBParsers.nounPairParser *)
    if(t.isEmpty) None else Some(t(0))
  }
  
  private def getByContents(pair: NounPair)(implicit c: java.sql.Connection): Option[NounPair] = {
    val t = SQL("""select id, fromstem, frompattern, fromexceptions, 
                          tostem, topattern, toexceptions, ignored 
                   from nounpairs 
                   where fromstem={fromstem} and frompattern={frompattern}""").on(
        'fromstem -> pair.plStem, 
        'frompattern -> pair.plPattern
    ).as(DBParsers.nounPairParser *)
    
    if(t.isEmpty) None else Some(t(0))
  }
  
  private def add(pair: VerbPair)(implicit c: java.sql.Connection): Long = {
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

  private def remove(pair: VerbPair)(implicit c: java.sql.Connection): Option[VerbPair] = {
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
    .on('frominfstem -> pair.plInfStem,
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
  
  private def getById(pair: VerbPair)(implicit c: java.sql.Connection): Option[VerbPair] = {
    val t = SQL("""select id, frominfstem, fromimpstem, frompattern, fromexceptions, 
                          toinfstem, toimpstem, topattern, toexceptions, prefixes
                   from verbpairs where id={id}""")
            .on('id -> pair.id)
            .as(DBParsers.verbPairParser *)
    if(t.isEmpty) None else Some(t(0))
  }
  
  private def getByContents(pair: VerbPair)(implicit c: java.sql.Connection): Option[VerbPair] = {
    val t = SQL("""select id, frominfstem, fromimpstem, frompattern, fromexceptions, 
                          toinfstem, toimpstem, topattern, toexceptions, prefixes
                   from verbpairs 
                   where frominfstem={frominfstem} and fromimpstem={fromimpstem}""").on(
        'frominfstem -> pair.plInfStem, 
        'fromimpstem -> pair.plImpStem
    ).as(DBParsers.verbPairParser *)
    
    if(t.isEmpty) None else Some(t(0))
  }  
  
  def listPairs: Seq[SpeechPartPair[_]]
  
  def add(entry: DictEntry): Long
  def update(entry: DictEntry): Unit
  def remove(id: Long): DictEntry
  /** get by id */
  def get(id: Long): Option[DictEntry]
  /** get by contents */
  def get(entry: DictEntry): Option[DictEntry]
     
  override def addRoot(root: Root):Option[Long] = DB.withConnection {
    implicit c => rootId(root) match {
      case Some(id) => Some(id)
      case None => {
        SQL("insert into roots (root, speechpart, lang) values ({root},{speechpart},{lang})")
        .on('root -> root.root,'speechpart -> root.speechpart, 'lang -> root.lang)
        .executeInsert()
        rootId(root)
      }
    }
  }
  
  override def roots:Seq[Root] = DB.withConnection {
    implicit c => SQL("select * from roots").as(DBParsers.rootParser *)
  }
  
  private def rootId(root: Root)(implicit c: java.sql.Connection):Option[Long] = {
    val idRowOption = SQL("select id from roots where root={root} and speechpart={speechPart} and lang={lang}")
    		  			.on('root -> root.root, 'speechPart -> root.speechpart, 'lang -> root.lang).apply().headOption
    idRowOption match {
      case Some(idRow) => Some(idRow[Long]("id"))
      case None => None
    }
  }
  
  

}