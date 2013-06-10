package dbhelpers

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import play.api.Logger
import logic.AbstractDictionary

case class Root(val id:Long,val root:String,val speechpart:String,val lang:String);
case class Word(val id:Long,val word:String,val lang:String,val rootid:Long,val caseid:String);
case class Translation(val id:Long, val wordid1:Long, val wordid2:Long);

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
  
  override def add(word1:String, word2:String):Unit = DB.withConnection {
    implicit c => {
      println("addTranslation: " + word1 + " -> " + word2)
      
      val w1Id = wordId(word1,"pl") match {
        case Some(wordId) => wordId
        case None => {
          SQL("insert into words (word, lang, rootid, caseid) values ({word},'pl',-1,'')").on('word -> word1).executeInsert()
          wordId(word1,"pl").get
        }
      }
      println("word1 added with id: " + w1Id)
      
      val w2Id = wordId(word2,"ns") match {
        case Some(wordId) => wordId
        case None => {
          SQL("insert into words (word, lang, rootid, caseid) values ({word},'ns',-1,'')").on('word -> word2).executeInsert()
          wordId(word2,"ns").get
        }
      }
      println("word2 added with id: " + w2Id)
      
      SQL("insert into translations (wordid1, wordid2) values ({wordid1},{wordid2})").on('wordid1 -> w1Id,'wordid2 -> w2Id).executeInsert()
      val cRow = SQL("select count(*) as c from translations").apply().head
      println("translations found: " + cRow[Long]("c"))
    }
  }
  
  override def update(word1:String, words2:String) = {
    // TODO
  }
  
  override def tuples:Seq[(String,String)] = DB.withConnection {
    implicit c => {
      println("DBDictionary.translations")
      val tList = SQL("select * from translations").as(DBParsers.translationParser *)
      println("translations found: " + tList.size)
      tList.map{ t => {
          val word1 = SQL("select word from words where id={id}").on('id -> t.wordid1).apply().head[String]("word")
          val word2 = SQL("select word from words where id={id}").on('id -> t.wordid2).apply().head[String]("word")
          (word1,word2)
        }
      }
    }
  }

  override def words:Seq[String] = DB.withConnection {
    implicit c => {
      println("DBDictionary.words")
      val wList = SQL("select * from words").as(DBParsers.wordParser *)
      println("words found: " + wList.size)
      wList.foreach(println)
      wList.map{ w => w.toString }
    }
  }
  
  override def hasWord(word: String, lang: String):Boolean = DB.withConnection {
    implicit c => {
      val cRow = SQL("select count(*) as c from words where word={word} and lang={lang}").on('word -> word, 'lang -> lang).apply().head
      cRow[Long]("c") != 0
    }
  }
  
  private def wordId(word: String, lang: String):Option[Long] = DB.withConnection {
    implicit c => {
      val idRowOption = SQL("select id from words where word={word} and lang={lang}").on('word -> word, 'lang -> lang).apply().headOption
      idRowOption match {
        case Some(idRow) => Some(idRow[Long]("id"))
        case None => None
      }
    }
  }
  override def isEmpty:Boolean = DB.withConnection {
    implicit c => {
      val cRow = SQL("select count(*) as c from translations").apply().head
      cRow[Long]("c") == 0
    }
  }
}