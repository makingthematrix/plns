package logic

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.{Test, Before}
import models.NounPair
import DictionaryFactory._

class NounSuite {
  val dict = DictionaryFactory.dict(DEBUG)
  
  private def setUp() = dict.clear
  private def tearDown() = dict.clear
  
  @Test
  def shouldGenerateTranslation(){
    setUp()
    try {
      val np = NounPair("wilk","SOFT_MASCULINE_PERSON",None,"vlk","SOFT_MASCULINE_PERSON",None,IgnoredNumber.NONE)
      assertEquals(None,dict.get("wilk"))
      np.add()
      assertEquals(Option("vlk"),dict.get("wilk"))
    } finally tearDown()
  }
  
  @Test
  def shouldGenerateException(){
    setUp()
    try {
      val np = NounPair("wilk","SOFT_MASCULINE_PERSON",Some("NOMP:wilki,VOCP:wilki"),"vlk","SOFT_MASCULINE_PERSON",None,IgnoredNumber.NONE)
      assertEquals(None,dict.get("wilk"))
      np.add()
      assertEquals(Option("vlk"),dict.get("wilk"))
      assertEquals(Option("vlki"),dict.get("wilki"))
      assertEquals(None,dict.get("wilkowie"))
    } finally tearDown()
  }
  
  @Test
  def shouldIgnoreSingular(){
    setUp()
    try {
      val np = NounPair("spodni","SOFT_MASCULINE_OBJECT_E",
    		  			Some("NOMS:spodeń,GENP:spodni"),
    		  			"nogavic","HARD_MASCULINE_OBJECT",
    		  			Some("NOMS:nogavic,NOMP:nogavice,ACCP:nogavice"),
    		  			IgnoredNumber.SINGULAR)
      assertEquals(None,dict.get("spodnie"))
      np.add()
      assertEquals(Option("nogavice"),dict.get("spodnie"))
      assertEquals(None,dict.get("spodeń"))
    } finally tearDown()  											
  }
  
    @Test
  def shouldIgnorePlural(){
    setUp()
    try {
      val np = NounPair("człowiek","SOFT_MASCULINE_OBJECT_I",Some("GENP:człowieków"),
    		  			"muž","HARD_MASCULINE_PERSON",None,
    		  			IgnoredNumber.PLURAL)
      assertEquals(None,dict.get("człowiek"))
      np.add()
      assertEquals(Option("muž"),dict.get("człowiek"))
      assertEquals(None,dict.get("człowieków"))
    } finally tearDown()  											
  }
}