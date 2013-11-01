package logic

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.{Test, Before}
import models.VerbPair
import DictionaryFactory._

class VerbSuite {
  val dict = DictionaryFactory.dict(DEBUG)
  
  private def setUp() = dict.clear
  private def tearDown() = dict.clear
  
  @Test
  def shouldContainImperfectiveAspect(){
    val word = PLVerb.word("mów", "mów", "VIa", false, -1L)
    word.conjugation.suffices.get(Conj.ACTIVE) match {
      case Some(str) => assertEquals("iąc",str)
      case None => fail()
    }
  }
  
  @Test
  def shouldGenerateTranslation(){
    setUp()
    try {
      val vp = new VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,None)
      assertEquals(None,dict.getTranslation("mówić"))
      vp.add()
      assertEquals(Option("govoriti"),dict.getTranslation("mówić"))
    } finally tearDown()
  }
  
  @Test
  def shouldGenerateTranslationWithPrefix(){
    setUp()
    try {
      val vp = new VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,Some("_,prze_pre"))
      assertEquals(None,dict.getTranslation("mówić"))
      vp.add()
      assertEquals(Option("pregovoriti"),dict.getTranslation("przemówić")) 
    } finally tearDown()
  } 
  
  @Test
  def shouldSetPerfectivesWithPrefixes(){
    setUp()
    val prefixes = Some("_,"+VerbPair.perfectiveMarker+"prze_pre")
    try{
      val vp = new VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,prefixes)
      assertEquals(None,dict.getTranslation("mówić"))
      vp.add()
      assertEquals(Option("govoriti"),dict.getTranslation("mówić"))
      assertEquals(Option("pregovoriti"),dict.getTranslation("przemówić")) 
      assertEquals(Option("govorjučy"),dict.getTranslation("mówiący"))
      assertEquals(None,dict.getTranslation("przemówiący"))
      assertEquals(Option("pregovorivšy"),dict.getTranslation("przemówiwszy"))
      assertEquals(None,dict.getTranslation("mówiwszy"))
    } finally tearDown()
  }
  
  @Test
  def shouldGenerateParticiples(){
    setUp()
    val prefixes = Some("_,"+VerbPair.perfectiveMarker+"po_po")
    try {
      val vp = new VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,prefixes)
      assertEquals(None,dict.getTranslation("mówić"))
      vp.add()
      assertEquals(Option("govoriti"),dict.getTranslation("mówić"))
      assertEquals(Option("govorjuč"),dict.getTranslation("mówiąc"))
      assertEquals(Option("govorjučy"),dict.getTranslation("mówiący"))
      assertEquals(Option("govorieny"),dict.getTranslation("mówiony"))
      assertEquals(Option("govorienje"),dict.getTranslation("mówienie"))
      assertEquals(Option("pogovorienjej"),dict.getTranslation("pomówień"))
      assertEquals(Option("pogovorivšy"),dict.getTranslation("pomówiwszy"))
    } finally tearDown()
  }
  
  @Test
  def shouldGenerateConditionals(){
    setUp()
    try {
      val vp = new VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,None)
      assertEquals(None,dict.getTranslation("mówić"))
      vp.add()
      assertEquals(Option("govoril byh"),dict.getTranslation("mówiłbym"))
      assertEquals(Option("govorila bys"),dict.getTranslation("mówiłabyś"))
      assertEquals(Option("govoril by"),dict.getTranslation("mówiłby"))
      assertEquals(Option("govorili byhom"),dict.getTranslation("mówiłybyśmy"))
      assertEquals(Option("govorili byste"),dict.getTranslation("mówilibyście"))
      assertEquals(Option("govorili by"),dict.getTranslation("mówiłyby"))
    } finally tearDown()
  }
  
  @Test
  def shouldGenerateLongPastTense(){
    setUp()
    try {
      val vp = new VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,None)
      assertEquals(None,dict.getTranslation("mówić"))
      vp.add()
      assertEquals(Option("jesm govoril"),dict.getTranslation("mówiłem"))
      assertEquals(Option("jesi govorila"),dict.getTranslation("mówiłaś"))
      assertEquals(Option("je govoril"),dict.getTranslation("mówił"))
      assertEquals(Option("jesme govorili"),dict.getTranslation("mówiłyśmy"))
      assertEquals(Option("jeste govorili"),dict.getTranslation("mówiliście"))
      assertEquals(Option("sut govorili"),dict.getTranslation("mówiły"))
    } finally tearDown()
  }      
}