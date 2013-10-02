package logic

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.{Test, Before}
import models.NounPair

class NounSuite {
  val dict = NSTranslator.changeDictionary("debug")
  
  private def setUp() = dict.clear
  private def tearDown() = dict.clear
  
  @Test
  def shouldGenerateTranslation(){
    setUp()
    try {
      val np = NounPair("wilk","SOFT_MASCULINE_PERSON",None,"vlk","SOFT_MASCULINE_PERSON",None,"none")
      assertEquals(None,dict.get("wilk"))
      np.add()
      assertEquals(Option("vlk"),dict.get("wilk"))
    } finally tearDown()
  }
  
  @Test
  def shouldGenerateException(){
    setUp()
    try {
      val np = NounPair("wilk","SOFT_MASCULINE_PERSON",Some("NOMP:wilki,VOCP:wilki"),"vlk","SOFT_MASCULINE_PERSON",None,"none")
      assertEquals(None,dict.get("wilk"))
      np.add()
      assertEquals(Option("vlk"),dict.get("wilk"))
      assertEquals(Option("vlki"),dict.get("wilki"))
      assertEquals(None,dict.get("wilkowie"))
    } finally tearDown()
  }
}