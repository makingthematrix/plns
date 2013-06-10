package logic

import dbhelpers.DBDictionary

object DictionaryFactory {
	final val DEBUG_DICT = new Dictionary();
	final val DB_DICT = new DBDictionary();
}