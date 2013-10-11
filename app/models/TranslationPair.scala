package models

case class TranslationPair(val source: String,val target: String)

object TranslationPair {
  val empty = new TranslationPair("","")
}

