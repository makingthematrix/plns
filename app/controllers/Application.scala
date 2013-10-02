package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models.TranslationPair
import logic.NSTranslator
import models.UninflectedPair
import models.AdverbPair
import logic.PLAdverb
import logic.PLAdjective
import logic.NSAdverb
import play.api.libs.json._
import models.AdjectivePair
import logic.PLMode._
import logic.NSAdjective
import logic.CaseDescription
import logic.SpeechPart
import models.SpeechPartPair
import models.NounPair
import logic.PLNoun
import logic.NSNoun
import models.DeclensionTemplate
import models.VerbPair
import logic.NSVerb
import logic.PLVerb
import logic.Noun
import logic.Verb

object Application extends Controller {

  def index = Action { Ok(views.html.index(TranslationPair.empty,Array[String](),translateForm)) }
  
  def translate = Action { 
    implicit request => {
    	val (source,_) = translateForm.bindFromRequest.get;
    	val (target,untranslated) = if(source == "") ("",Array[String]()) else {
    	  if(NSTranslator.isEmpty) NSTranslator.init()
    	  NSTranslator.translate(source)
    	}
    	Ok(views.html.index(new TranslationPair(source,target),untranslated,translateForm));
    }
  }
  
  val translateForm = Form( tuple("source" -> text, "target" -> text) );
  
  def list = Action { 
    if(NSTranslator.isEmpty) NSTranslator.init()
    Ok(views.html.list(NSTranslator.rootPairs))
  }

  val verbForm = Form(
    mapping("plInfStem" -> nonEmptyText,
            "plImpStem" -> nonEmptyText,
    		"plPattern" -> nonEmptyText,
    		"plExceptions" -> optional(text),
            "nsInfStem" -> nonEmptyText,
            "nsImpStem" -> nonEmptyText,
            "nsPattern" -> nonEmptyText,
            "nsExceptions" -> optional(text),
            "prefixes" -> optional(text)
    ) 
    (VerbPair.apply)
    (VerbPair.unapply) 
  );
  
  val plVerbPatterns = PLVerb.idsExamples
  val nsVerbPatterns = NSVerb.idsExamples
  
  def verbTemplates = Action {
    val plTemplates = PLVerb.patterns.map(p => p.template)
    val nsTemplates = NSVerb.patterns.map(p => p.template)
    val templatesJson = (plTemplates ++ nsTemplates).map(template => Json.toJson(template)).toList;
    Ok(Json.toJson(templatesJson)).as("application/json")
  }
  
  def verb(pl:String) = Action {
    val root = pl;
    Ok(views.html.verb(root,verbForm))
  }
  
  def addVerb = Action {
    implicit request => {
      Logger("MyApp").info("addVerb")
      verbForm.bindFromRequest.fold(
    	formWithErrors => printFormErrors(formWithErrors), 
    	pair => add(pair)
      )
    }
  }
  
  val nounForm = Form(
    mapping("plStem" -> nonEmptyText,
    		"plPattern" -> nonEmptyText,
    		"plExceptions" -> optional(text),
    		"nsStem" -> nonEmptyText,
            "nsPattern" -> nonEmptyText,
            "nsExceptions" -> optional(text),
            "ignored" -> nonEmptyText
    ) 
    (NounPair.apply)
    (NounPair.unapply) 
  );
  
  val plNounPatterns = PLNoun.idsExamples
  val nsNounPatterns = NSNoun.idsExamples
  
  def noun(pl:String) = Action {
    val root = pl;
    Ok(views.html.noun(root,nounForm))
  }
  
  def nounTemplates = Action {
    val plTemplates = PLNoun.patterns.map(p => p.template)
    val nsTemplates = NSNoun.patterns.map(p => p.template)
    val templatesJson = (plTemplates ++ nsTemplates).map(template => Json.toJson(template)).toList;
    Ok(Json.toJson(templatesJson)).as("application/json")
  }
  
  def addNoun = Action {
    implicit request => {
      Logger("MyApp").info("addNoun")
      nounForm.bindFromRequest.fold(
    	formWithErrors => printFormErrors(formWithErrors), 
    	pair => add(pair)
      )
    }
  };
  
  val adjectiveForm = Form(
    mapping("plInd" -> nonEmptyText,
    		"plAdvInd" -> optional(text),
    		"plCmp" -> optional(text),
    		"plAdvCmp" -> optional(text),
    		"plMode" -> nonEmptyText,
    		"plAdvMode" -> nonEmptyText,
    		"plExceptions" -> optional(text),
            "nsInd" -> nonEmptyText,
            "nsAdvInd" -> optional(text),
            "nsCmp" -> optional(text),
            "nsAdvCmp" -> optional(text),
            "nsExceptions" -> optional(text),
            "cmpIgnored" -> optional(text)
    ) 
    (AdjectivePair.apply)
    (AdjectivePair.unapply) 
  );
  
  val plModes = List[String](HARD,SOFT);
  val declSing = Noun.singDeclension.map(d => d.toString());
  val declPl = Noun.pluralDeclension.map(d => d.toString());
  
  def adjective(pl:String) = Action {
    val root = if(pl.length()>1) pl.substring(0, pl.length()-1) else pl;
    Ok(views.html.adjective(root,adjectiveForm))
  }
  
  def addAdjective = Action {
    implicit request => {
      Logger("MyApp").info("addAdjective")
      adjectiveForm.bindFromRequest.fold(
    	formWithErrors => printFormErrors(formWithErrors), 
    	pair => add(pair)
      )
    }
  };

  def adjectiveTemplates = Action {
    val plHard = PLAdjective.template(HARD);
    val plSoft = PLAdjective.template(SOFT);
    val ns = NSAdjective.template;
    val templatesJson:Seq[JsValue] = Seq(plHard, plSoft, ns).map(template => Json.toJson(template));
    Ok(Json.toJson(templatesJson)).as("application/json")
  }
  
  val adverbForm = Form( 
    mapping("plInd" -> nonEmptyText,
    		"plCmp" -> optional(text),
    		"plMode" -> nonEmptyText,
            "nsInd" -> nonEmptyText,
            "nsCmp" -> optional(text),
            "cmpIgnored" -> optional(text)
    )(AdverbPair.apply)(AdverbPair.unapply) 
  );
  
  def adverb(pl:String) = Action {
    val root = if(pl.length()>1) pl.substring(0, pl.length()-1) else pl;
    Ok(views.html.adverb(root,adverbForm))
  }
  
  def addAdverb = Action {
    implicit request => {
      adverbForm.bindFromRequest.fold(
          formWithErrors => printFormErrors(formWithErrors),
          pair => add(pair)
      )
    }
  };
  
  def adverbTemplates = Action {
    val plHard = PLAdverb.template(HARD);
    val plSoft = PLAdverb.template(SOFT);
    val ns = NSAdverb.template;
    val templatesJson:Seq[JsValue] = Seq(plHard, plSoft, ns).map(template => Json.toJson(template));
    Ok(Json.toJson(templatesJson)).as("application/json")
  }
  
  val uninflectedForm = Form( 
    mapping("plWord" -> nonEmptyText, 
            "nsWord" -> nonEmptyText
    )(UninflectedPair.apply)(UninflectedPair.unapply) 
  );
  
  def uninflected(pl:String) = Action {
    Ok(views.html.uninflected(pl,uninflectedForm))
  }
  
  def addUninflected = Action {
    implicit request => {
      uninflectedForm.bindFromRequest.fold(
    	formWithErrors => printFormErrors(formWithErrors), 
    	pair => add(pair)
      )
    }
  };
  
  private def add[T <: SpeechPart[T]](pair: SpeechPartPair[T]) = {
    val results = pair.add()
    val sb = StringBuilder.newBuilder
    results.foreach( tuple => sb.append("Dodałem ").append(tuple._1).append(" -> ").append(tuple._2).append('\n'))
    Ok(sb.toString)    
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
