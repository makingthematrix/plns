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
          select dictentries.toword as word from dictentries 
          where fromword={word} lang='pl');
      """).on('word -> word).apply().headOption
      dictEntryRowOption match {
        case Some(wordRow) => Some(wordRow[String]("word"))
        case None => None
      }
    }
  }

  override def add[T](pair: SpeechPartPair[T]): Long = pair match {
    case un: UninflectedPair => add(un)
    case adv: AdverbPair => add(adv)
    case adj: AdjectivePair => add(adj)
    case noun: NounPair => add(noun)
    case verb: VerbPair => add(verb)
    case _ => throw new IllegalArgumentException("Unrecognized speech part: " + pair.toString())
  }
  
  private def add(pair: UninflectedPair) = {}
  private def add(pair: AdverbPair) = {}
  private def add(pair: AdjectivePair) = {}
  private def add(pair: NounPair) = {}
  private def add(pair: VerbPair) = {}
  
  def updatePair[T](pair: SpeechPartPair[T]): Unit
  def removePair[T](id: Long): SpeechPartPair[T]
  def listPairs: Seq[SpeechPartPair[_]]
  
  def add(entry: DictEntry): Long
  def update(entry: DictEntry): Unit
  def remove(id: Long): DictEntry
  /** get by id */
  def get(id: Long): Option[DictEntry]
  /** get by contents */
  def get(entry: DictEntry): Option[DictEntry]
    

  

  
  override def add(word1:Word, word2:Word):Unit = DB.withConnection {
    implicit c => {
      println("addTranslation: " + word1 + " -> " + word2)
      
      val w1Id = insertOrGetWord(word1.word,word1.lang,word1.rootid,word1.caseid)
      println("word1 added with id: " + w1Id)
      
      val w2Id = insertOrGetWord(word2.word,word2.lang,word2.rootid,word2.caseid)
      println("word2 added with id: " + w2Id)
      
      SQL("insert into translations (wordid1, wordid2) values ({wordid1},{wordid2})").on('wordid1 -> w1Id,'wordid2 -> w2Id).executeInsert()
    }
  }
  
  override def update(word1:String, word2:String):Unit = DB.withConnection {
    implicit c => {
      println("updateTranslation: " + word1 + " -> " + word2)
      val w1Id = insertOrGetWord(word1,"pl",-1,"")
      val w2Id = insertOrGetWord(word2,"ns",-1,"")
      
      translationId(w1Id) match {
        case Some(id) => SQL("update translations set wordid2={wordid2} where id={id}")
        					.on('wordid2 -> w2Id,'id -> id).executeUpdate()
        case None => SQL("insert into translations (wordid1, wordid2) values ({wordid1},{wordid2})")
        					.on('wordid1 -> w1Id,'wordid2 -> w2Id).executeInsert()
      }
    }
  }
  
  override def tuples:Seq[(String,String)] = DB.withConnection {
    implicit c => SQL("""
          select w1.word as word1,w2.word as word2 
          from words w1,translations,words w2 
          where w1.id=translations.wordid1 
          and w2.id=translations.wordid2;
      """).as(DBParsers.wordTupleParser *)
  }

  override def words:Seq[String] = DB.withConnection {
    implicit c => {
      val wList = SQL("select * from words").as(DBParsers.wordParser *)
      wList.map{ w => w.toString }
    }
  }
  
  override def hasWord(word: String, lang: String):Boolean = DB.withConnection {
    implicit c => {
      val cRow = SQL("select count(*) as c from words where word={word} and lang={lang}").on('word -> word, 'lang -> lang).apply().head
      cRow[Long]("c") != 0
    }
  }
  

  
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
  
  override def addRoots(from: Root,to: Root): (Long,Long) = DB.withConnection {
    implicit c => {
      val rootId1 = addRoot(from) match {
        case Some(id) => id
        case None => throw new IllegalArgumentException("Unable to store root " + from)
      }
      val rootId2 = addRoot(to) match {
        case Some(id) => id
        case None => throw new IllegalArgumentException("Unable to store root " + to)
      }
      addRootTranslation(rootId1,rootId2)
      return (rootId1,rootId2)
    }
  }
  
  override def roots:Seq[Root] = DB.withConnection {
    implicit c => SQL("select * from roots").as(DBParsers.rootParser *)
  }
  
  override def rootPairs:Seq[(Root,Root)] = DB.withConnection {
    implicit c => {
      val rootTrans:Seq[RootTrans] = SQL("select * from roottrans").as(DBParsers.rootTransParser *)
      rootTrans.map(t => {
        val root1 = SQL("select * from roots where id="+t.rootid1).as(DBParsers.rootParser *).head
        val root2 = SQL("select * from roots where id="+t.rootid2).as(DBParsers.rootParser *).head
        (root1,root2)
      })
    }
  }
   
  private def addRootTranslation(rootId1: Long, rootId2: Long)(implicit c: java.sql.Connection)
    = SQL("insert into roottrans (rootid1,rootid2) values ({rootid1},{rootid2})")
        .on('rootid1 -> rootId1,'rootid2 -> rootId2)
        .executeInsert()
  
  private def wordId(word: String, lang: String)(implicit c: java.sql.Connection):Option[Long] = {
    val idRowOption = SQL("select id from words where word={word} and lang={lang}")
    					.on('word -> word, 'lang -> lang).apply().headOption
    idRowOption match {
      case Some(idRow) => Some(idRow[Long]("id"))
      case None => None
    }
  }
  
  private def rootId(root: Root)(implicit c: java.sql.Connection):Option[Long] = {
    val idRowOption = SQL("select id from roots where root={root} and speechpart={speechPart} and lang={lang}")
    		  			.on('root -> root.root, 'speechPart -> root.speechpart, 'lang -> root.lang).apply().headOption
    idRowOption match {
      case Some(idRow) => Some(idRow[Long]("id"))
      case None => None
    }
  }
  
  private def translationId(wordId1: Long)(implicit c: java.sql.Connection):Option[Long] = {
    val idRowOption = SQL("select id from translations where wordid1={wordid1}").on('wordid1 -> wordId1).apply().headOption
    idRowOption match {
      case Some(idRow) => Some(idRow[Long]("id"))
      case None => None
    }     
  }
  
  private def insertOrGetWord(word: String, lang: String, rootId: Long, caseId: String)
    (implicit c: java.sql.Connection):Long = wordId(word,lang) match {
      case Some(wordId) => wordId
      case None => {
        SQL("insert into words (word, lang, rootid, caseid) values ({word},{lang},{rootId},{caseId})")
          	.on('word -> word,'lang -> lang,'rootId -> rootId,'caseId -> caseId)
          	.executeInsert()
        wordId(word,lang).get
      }
    }
}