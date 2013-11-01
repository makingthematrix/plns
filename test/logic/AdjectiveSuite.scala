package logic

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.{Test, Before}
import models.AdjectivePair
import logic.AdjectiveCase._
import logic.AdjectiveGender._
import logic.AdjectiveDegree._
import logic.HardSoftMode._
import logic.Decl._
import DictionaryFactory._

class AdjectiveSuite {
   val dict = DictionaryFactory.dict(DEBUG)
  
  private def setUp() = dict.clear
  private def tearDown() = dict.clear
  
  @Test
  def shouldGenerateTranslation(){
    setUp()
    try {
      val ap = new AdjectivePair("wielk","więk",SOFT,None,"velik","velik",None)
      assertEquals(None,dict.getTranslation("wielki"))
      ap.add()
      assertEquals(Option("veliky"),dict.getTranslation("wielki"))
      assertEquals(Option("velika"),dict.getTranslation("wielka"))
      assertEquals(Option("velike"),dict.getTranslation("wielkie"))
      assertEquals(Option("velikiejšy"),dict.getTranslation("większy"))
      assertEquals(Option("velikiejšej"),dict.getTranslation("większej"))
      assertEquals(Option("najvelikiejših"),dict.getTranslation("największych"))
    } finally tearDown()
  }
   
  @Test
  def shouldIgnoreComparative(){
    setUp()
    try {
      val ap = new AdjectivePair("polityczn",HARD,None,"političn",None)
      assertEquals(None,dict.getTranslation("polityczny"))
      ap.add()
      assertEquals(Option("političny"),dict.getTranslation("polityczny"))
      assertEquals(None,dict.getTranslation("polityczniejszy"))
      assertEquals(None,dict.getTranslation("političnejšy"))
    } finally tearDown() 
  }
  
  @Test
  def shouldUseException(){
    setUp()
    try {
      val mcNoms = AdjectiveCase(MASCULINE,COMPARATIVE,NOMS)
      val mcVocs = AdjectiveCase(MASCULINE,COMPARATIVE,VOCS)
      val ap = new AdjectivePair("duż","duż",HARD,Option(mcNoms+":większy,"+mcVocs+":większy"),"velik","velik",None)
      assertEquals(None,dict.getTranslation("wielki"))
      assertEquals(None,dict.getTranslation("duży"))
      ap.add()
      assertEquals(Option("veliky"),dict.getTranslation("duży"))
      assertEquals(Option("velikiejšy"),dict.getTranslation("większy"))
      assertEquals(None,dict.getTranslation("duższy"))
    } finally tearDown()
  }
  
}