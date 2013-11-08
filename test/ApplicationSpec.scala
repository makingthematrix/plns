package test

import org.specs2.mutable._
import play.api.test._
import play.api.test.Helpers._
import models.NounPair
import logic.IgnoredNumber
import logic.DictionaryFactory
import DictionaryFactory._
import logic.DictEntry

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
class ApplicationSpec extends Specification {
  
  "Application" should {
    
    "send 404 on a bad request" in {
      running(FakeApplication()) {
        route(FakeRequest(GET, "/boum")) must beNone        
      }
    }
    
    "render the index page" in {
      running(FakeApplication()) {
        val home = route(FakeRequest(GET, "/")).get
        
        status(home) must equalTo(OK)
        contentType(home) must beSome.which(_ == "text/html")
        contentAsString(home) must contain ("Your new application is ready.")
      }
    }
    
    "create entries and roots" in {
      running(FakeApplication()) {
        val dict = DictionaryFactory.dict(DB)
        val np = new NounPair("wilk","SOFT_MASCULINE_PERSON",Some("NOMP:wilki,VOCP:wilki"),"vlk","SOFT_MASCULINE_PERSON",None,IgnoredNumber.NONE)
        dict.getTranslation("wilk") must equalTo(None)
      
        val (plRoot, nsRoot) = np.add(dict)
        val pairId = plRoot.speechPartId
        val pair = dict.getPairById("noun", pairId).asInstanceOf[Option[NounPair]]
        np.plStem must equalTo(pair.get.plStem)
        
        val entry = dict.getEntryByContents(new DictEntry(plRoot.root, plRoot.lang, "", ""))
        pairId must equalTo(entry.get.speechPartId)
      
        val root = dict.getRootByWord(nsRoot.root)
        nsRoot.id must equalTo(root.get.id)
      }
    }
  }
}