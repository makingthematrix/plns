package models

case class TranslationPair(source: String, target: String);

object TranslationPair {
  def empty = new TranslationPair("","");
}

