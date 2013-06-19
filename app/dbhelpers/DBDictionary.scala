package dbhelpers

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import play.api.Logger
import logic.AbstractDictionary
import logic.Root
import logic.Word
import logic.Translation

object DBParsers {
  val rootParser = {
    get[Long]("id") ~ 
    get[String]("root") ~
    get[String]("speechpart") ~
    get[String]("lang") map {
      case id~root~speechpart~lang => Root(id, root, speechpart, lang)
    }
  }
  
  val wordParser = {
    get[Long]("id") ~ 
    get[String]("word") ~
    get[String]("lang") ~
    get[Long]("rootid") ~
    get[String]("caseid") map {
      case id~word~lang~rootid~caseid => Word(id, word, lang, rootid, caseid)
    }
  }
  
  val translationParser = {
    get[Long]("id") ~
    get[Long]("wordid1") ~
    get[Long]("wordid2") map {
      case id~wordid1~wordid2 => Translation(id, wordid1, wordid2)
    }
  }  
  
  val wordTupleParser = {
    get[String]("word1") ~
    get[String]("word2") map {
      case word1~word2 => (word1,word2)
    }
  }
}

class DBDictionary extends AbstractDictionary {
  override def get(word:String):Option[String] = DB.withConnection { 
    implicit c => {
      println("DBDictionary.get, word: " + word)
      val wordRowOption = SQL("""
          select words.word as word from words, translations 
          where words.id=translations.wordid2 
          and translations.wordid1 = (select id from words where word={word} and lang='pl');
      """).on('word -> word).apply().headOption
      wordRowOption match {
        case Some(wordRow) => Some(wordRow[String]("word"))
        case None => None
      }
    }
  }
  
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
        case Some(id) => SQL("update translations (id, wordid1, wordid2) set wordid2={wordId2} where id={id}")
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
  
  override def isEmpty:Boolean = DB.withConnection {
    implicit c => {
      val cRow = SQL("select count(*) as c from translations").apply().head
      cRow[Long]("c") == 0
    }
  }
  
  override def addRoot(word: String, speechPart: String, lang: String):Option[Long] = DB.withConnection {
    implicit c => {
      rootId(word,speechPart,lang) match {
        case Some(id) => Some(id)
        case None => {
          SQL("insert into roots (root, speechpart, lang) values ({root},{speechPart},{lang})")
          .on('root -> word,'speechPart -> speechPart, 'lang -> lang)
          .executeInsert()
          rootId(word,speechPart,lang)
        }
      }
    }
  }
  
  override def roots:Seq[Root] = DB.withConnection {
    implicit c => SQL("select * from roots").as(DBParsers.rootParser *)
  }
   
  private def wordId(word: String, lang: String)(implicit c: java.sql.Connection):Option[Long] = {
    val idRowOption = SQL("select id from words where word={word} and lang={lang}")
    					.on('word -> word, 'lang -> lang).apply().headOption
    idRowOption match {
      case Some(idRow) => Some(idRow[Long]("id"))
      case None => None
    }
  }
  
  private def rootId(word: String, speechPart: String, lang: String)(implicit c: java.sql.Connection):Option[Long] = {
    val idRowOption = SQL("select id from roots where root={root} and speechpart={speechPart} and lang={lang}")
    		  			.on('root -> word, 'speechPart -> speechPart, 'lang -> lang).apply().headOption
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