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
    val word = PLVerb.word("mów", "mów", "VIa",false)
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
      assertEquals(None,dict.get("mówić"))
      vp.add()
      assertEquals(Option("govoriti"),dict.get("mówić"))
    } finally tearDown()
  }
  
  @Test
  def shouldGenerateTranslationWithPrefix(){
    setUp()
    try {
      val vp = new VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,Some("_,prze_pre"))
      assertEquals(None,dict.get("mówić"))
      vp.add()
      assertEquals(Option("pregovoriti"),dict.get("przemówić")) 
    } finally tearDown()
  } 
  
  @Test
  def shouldSetPerfectivesWithPrefixes(){
    setUp()
    val prefixes = Some("_,"+VerbPair.perfectiveMarker+"prze_pre")
    try{
      val vp = new VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,prefixes)
      assertEquals(None,dict.get("mówić"))
      vp.add()
      assertEquals(Option("govoriti"),dict.get("mówić"))
      assertEquals(Option("pregovoriti"),dict.get("przemówić")) 
      assertEquals(Option("govorjučy"),dict.get("mówiący"))
      assertEquals(None,dict.get("przemówiący"))
      assertEquals(Option("pregovorivšy"),dict.get("przemówiwszy"))
      assertEquals(None,dict.get("mówiwszy"))
    } finally tearDown()
  }
  
  @Test
  def shouldGenerateParticiples(){
    setUp()
    val prefixes = Some("_,"+VerbPair.perfectiveMarker+"po_po")
    try {
      val vp = new VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,prefixes)
      assertEquals(None,dict.get("mówić"))
      vp.add()
      assertEquals(Option("govoriti"),dict.get("mówić"))
      assertEquals(Option("govorjuč"),dict.get("mówiąc"))
      assertEquals(Option("govorjučy"),dict.get("mówiący"))
      assertEquals(Option("govorieny"),dict.get("mówiony"))
      assertEquals(Option("govorienje"),dict.get("mówienie"))
      assertEquals(Option("pogovorienjej"),dict.get("pomówień"))
      assertEquals(Option("pogovorivšy"),dict.get("pomówiwszy"))
    } finally tearDown()
  }
  
  @Test
  def shouldGenerateConditionals(){
    setUp()
    try {
      val vp = new VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,None)
      assertEquals(None,dict.get("mówić"))
      vp.add()
      assertEquals(Option("govoril byh"),dict.get("mówiłbym"))
      assertEquals(Option("govorila bys"),dict.get("mówiłabyś"))
      assertEquals(Option("govoril by"),dict.get("mówiłby"))
      assertEquals(Option("govorili byhom"),dict.get("mówiłybyśmy"))
      assertEquals(Option("govorili byste"),dict.get("mówilibyście"))
      assertEquals(Option("govorili by"),dict.get("mówiłyby"))
    } finally tearDown()
  }
  
  @Test
  def shouldGenerateLongPastTense(){
    setUp()
    try {
      val vp = new VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,None)
      assertEquals(None,dict.get("mówić"))
      vp.add()
      assertEquals(Option("jesm govoril"),dict.get("mówiłem"))
      assertEquals(Option("jesi govorila"),dict.get("mówiłaś"))
      assertEquals(Option("je govoril"),dict.get("mówił"))
      assertEquals(Option("jesme govorili"),dict.get("mówiłyśmy"))
      assertEquals(Option("jeste govorili"),dict.get("mówiliście"))
      assertEquals(Option("sut govorili"),dict.get("mówiły"))
    } finally tearDown()
  }      
}