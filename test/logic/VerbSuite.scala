package logic

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.{Test, Before}
import models.VerbPair


class VerbSuite {
  @Test
  def shouldContainActiveCase(){
    val word = PLVerb.word("mów", "mów", "VIa")
    word.conjugation.suffices.get(Conj.ACTIVE) match {
      case Some(str) => assertEquals("iąc",str)
      case None => fail()
    }
  }
  
  @Test
  def shouldGenerateTranslation(){
    val dict = NSTranslator.changeDictionary("debug")
    dict.clear
    val vp = VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,"")
    assertEquals(None,dict.get("mówić"))
    vp.add()
    assertEquals(Option("govoriti"),dict.get("mówić"))
  }
  
  @Test
  def shouldGenerateTranslationWithPrefix(){
    val dict = NSTranslator.changeDictionary("debug")
    dict.clear
    val vp = VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,":,prze:pre")
    assertEquals(None,dict.get("mówić"))
    vp.add()
    assertEquals(Option("pregovoriti"),dict.get("przemówić")) 
  } 
  
  @Test
  def shouldGenerateParticiples(){
    val dict = NSTranslator.changeDictionary("debug")
    dict.clear
    val vp = VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,":,po:po")
    assertEquals(None,dict.get("mówić"))
    vp.add()
    assertEquals(Option("govoriti"),dict.get("mówić"))
    assertEquals(Option("govorjuč"),dict.get("mówiąc"))
    assertEquals(Option("govorjučy"),dict.get("mówiący"))
    assertEquals(Option("govorieny"),dict.get("mówiony"))
    assertEquals(Option("govorienje"),dict.get("mówienie"))
    assertEquals(Option("pogovorienjej"),dict.get("pomówień"))
    assertEquals(Option("pogovorivši"),dict.get("pomówiwszy"))
    println("size: " + dict.size)
    dict.clear
  }
  
  @Test
  def shouldGenerateConditionals(){
    val dict = NSTranslator.changeDictionary("debug")
    dict.clear
    val vp = VerbPair("mów","mów","VIa",None,"govori","govor","SOFT",None,":,po:po")
    assertEquals(None,dict.get("mówić"))
    vp.add()
    assertEquals(Option("govoril byh"),dict.get("mówiłbym"))
    assertEquals(Option("govorila bys"),dict.get("mówiłabyś"))
    assertEquals(Option("govoril by"),dict.get("mówiłby"))
    assertEquals(Option("govorili byhom"),dict.get("mówiłybyśmy"))
    assertEquals(Option("govorili byste"),dict.get("mówilibyście"))
    assertEquals(Option("govorili by"),dict.get("mówiłyby"))
    println("size: " + dict.size)
    dict.clear
  }
}