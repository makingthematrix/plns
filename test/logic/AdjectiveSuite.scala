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
      assertEquals(None,dict.get("wielki"))
      ap.add()
      assertEquals(Option("veliky"),dict.get("wielki"))
      assertEquals(Option("velika"),dict.get("wielka"))
      assertEquals(Option("velike"),dict.get("wielkie"))
      assertEquals(Option("velikiejšy"),dict.get("większy"))
      assertEquals(Option("velikiejšej"),dict.get("większej"))
      assertEquals(Option("najvelikiejših"),dict.get("największych"))
    } finally tearDown()
  }
   
  @Test
  def shouldIgnoreComparative(){
    setUp()
    try {
      val ap = new AdjectivePair("polityczn",HARD,None,"političn",None)
      assertEquals(None,dict.get("polityczny"))
      ap.add()
      assertEquals(Option("političny"),dict.get("polityczny"))
      assertEquals(None,dict.get("polityczniejszy"))
      assertEquals(None,dict.get("političnejšy"))
    } finally tearDown() 
  }
  
  @Test
  def shouldUseException(){
    setUp()
    try {
      val mcNoms = AdjectiveCase(MASCULINE,COMPARATIVE,NOMS)
      val mcVocs = AdjectiveCase(MASCULINE,COMPARATIVE,VOCS)
      val ap = new AdjectivePair("duż","duż",HARD,Option(mcNoms+":większy,"+mcVocs+":większy"),"velik","velik",None)
      assertEquals(None,dict.get("wielki"))
      assertEquals(None,dict.get("duży"))
      ap.add()
      assertEquals(Option("veliky"),dict.get("duży"))
      assertEquals(Option("velikiejšy"),dict.get("większy"))
      assertEquals(None,dict.get("duższy"))
    } finally tearDown()
  }
  
}