package logic

import dbhelpers.DBDictionary

object DictionaryFactory {
  val DEBUG = "debug"
  val DB = "db"
    
  private val DEBUG_DICT = new Dictionary();
  private val DB_DICT = new DBDictionary();
  
  private var dictionary:AbstractDictionary = DB_DICT;
  
  def dict(name: String):AbstractDictionary = {
    name match {
      case DEBUG => dictionary = DEBUG_DICT; 
      case DB => dictionary = DB_DICT; 
      case _ => throw new IllegalArgumentException("Dictionary unknown: " + name)
    }
    dictionary
  }
  
  def dict:AbstractDictionary = dictionary
}