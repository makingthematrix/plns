package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import dbhelpers.DBDictionary
import logic.SpeechPart
import logic.NSTranslator
import logic.DictionaryFactory
import models.UninflectedPair

object DBHelper extends Controller {
  private val dict = DictionaryFactory.dict(DictionaryFactory.DB)
  
  val translationForm = Form( tuple("word1" -> text, "word2" -> text) );
	
  def translation = Action { Ok(views.html.addTranslation(translationForm)) }
	
  def addTranslation = Action { 
    implicit request => {
      println("addTranslation")
      translationForm.bindFromRequest.fold(
    	formWithErrors => printFormErrors(formWithErrors), 
    	pair => {
    		val (word1, word2) = translationForm.bindFromRequest.get
    		add(word1,word2)    	  
    	}
      )
    }
  }
  
  private def add(word1: String,word2: String) = {
    if(dict.getWord(word1,"pl") != None){
      BadRequest("The word " + word1 + " is already in the Polish dictionary")
    }
    if(dict.getWord(word2,"ns") != None){
      BadRequest("The word " + word2 + " is already in the Novoslovienski dictionary")
    }
    dict.add(new UninflectedPair(word1,word2))
    Ok("Dodałem: " + word1 + " -> " + word2);
  }
  
  def listTranslations = Action {
    implicit request => {
      println("listTranslations")
      val sb = new StringBuilder()
      dict.listPairs.foreach( p => {
        val (plRoot, nsRoot) = p.roots
        sb.append(plRoot).append(" -> ").append(nsRoot).append("\n")
      })
      Ok(sb.toString)
    }
  }
  
  private def printFormErrors[T](formWithErrors: Form[T]) = {
    val sb = new StringBuilder("Errors: <br>\n")
    formWithErrors.errors.foreach( error => {
      sb.append(error).append("<br>\n")
      println(error)
    })
    BadRequest(sb.toString);
  }
}