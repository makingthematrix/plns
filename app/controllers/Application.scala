package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.libs.json._
import logic._
import models._
import logic.HardSoftMode._

object Application extends Controller {

  def index = Action { Ok(views.html.index(TranslationPair.empty,Seq[String](),translateForm)) }
  
  private def translate(source: String) = {
    val dict = DictionaryFactory.dict
    val (target,untranslated) = if(source == "") ("",Seq[String]()) else {
      if(dict.isEmpty) NSTranslator.init()
      dict.translate(source)
    }
    Ok(views.html.index(TranslationPair(source,target),untranslated,translateForm));
  }
  
  def translate: Action[AnyContent] = Action { 
    implicit request => {
      Logger("MyApp").info("translate")
      translateForm.bindFromRequest.fold(
        formWithErrors => printFormErrors(formWithErrors),
        source => translate(source._1)
      )
    }
  }
  
  val translateForm = Form( tuple("source" -> text, "target" -> text) );
  
  /** @todo there has to be some better way to handle it than using asInstanceOf
   *  Probably dict.listRoots should not return a Seq[SpeechPartPair[_]] but something
   *  more like Seq[SpeechPartPair[T <: SpeechPart[T]]] - but it doesn't work
   */
  def list = Action { 
    val dict = DictionaryFactory.dict
    if(dict.isEmpty) NSTranslator.init()
    val rootPairs = dict.listPairs.map( pair => {
      val pl:SpeechPart[_] = pair.pl
      val ns:SpeechPart[_] = pair.ns
      (pl.toRoot(pair.id), ns.toRoot(pair.id)) 
    })
    Ok(views.html.list(rootPairs))
  }

  val verbForm = Form(
    mapping("id" -> of[Long],
            "plInfStem" -> nonEmptyText,
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
    Ok(views.html.verb(SpeechPartPair.noId, pl, verbForm))
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
    mapping("id" -> of[Long],
            "plStem" -> nonEmptyText,
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
    Ok(views.html.noun(SpeechPartPair.noId, pl, nounForm))
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
    mapping("id" -> of[Long],
            "plInd" -> nonEmptyText,
    		"plAdvInd" -> nonEmptyText,
    		"plCmp" -> nonEmptyText,
    		"plAdvCmp" -> nonEmptyText,
    		"plMode" -> nonEmptyText,
    		"plAdvMode" -> nonEmptyText,
    		"plExceptions" -> optional(text),
            "nsInd" -> nonEmptyText,
            "nsAdvInd" -> nonEmptyText,
            "nsCmp" -> nonEmptyText,
            "nsAdvCmp" -> nonEmptyText,
            "nsExceptions" -> optional(text),
            "cmpIgnored" -> nonEmptyText
    ) 
    (AdjectivePair.apply)
    (AdjectivePair.unapply) 
  );
  
  val plModes = List[String](HARD,SOFT);
  val declSing = Noun.singularDeclension.map(d => d.toString());
  val declPl = Noun.pluralDeclension.map(d => d.toString());
  
  def adjective(pl:String) = Action {
    Ok(views.html.adjective(SpeechPartPair.noId, pl, adjectiveForm))
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
    mapping("id" -> of[Long],
            "plInd" -> nonEmptyText,
    		"plCmp" -> nonEmptyText,
    		"plMode" -> nonEmptyText,
            "nsInd" -> nonEmptyText,
            "nsCmp" -> nonEmptyText,
            "cmpIgnored" -> nonEmptyText
    )(AdverbPair.apply)(AdverbPair.unapply) 
  );
  
  def adverb(pl:String) = Action {
    Ok(views.html.adverb(SpeechPartPair.noId, pl, adverbForm))
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
    val plHard = PLAdverb.template(HARD)
    val plSoft = PLAdverb.template(SOFT)
    val ns = NSAdverb.template
    val templatesJson:Seq[JsValue] = Seq(plHard, plSoft, ns).map(template => Json.toJson(template))
    Ok(Json.toJson(templatesJson)).as("application/json")
  }
  
  val uninflectedForm = Form( 
    mapping("id" -> of[Long],
            "plWord" -> nonEmptyText, 
            "nsWord" -> nonEmptyText
    )(UninflectedPair.apply)(UninflectedPair.unapply) 
  )
  
  def uninflected(pl:String) = Action {
    Ok(views.html.uninflected(SpeechPartPair.noId, pl, uninflectedForm))
  }
  
  def addUninflected = Action {
    implicit request => {
      uninflectedForm.bindFromRequest.fold(
    	formWithErrors => printFormErrors(formWithErrors), 
    	pair => add(pair)
      )
    }
  }
  
  private def add[T <: SpeechPart[T]](pair: SpeechPartPair[T]) = {
    val results = pair.add()
    val sb = StringBuilder.newBuilder
    results.foreach( tuple => sb.append("Dodałem ").append(tuple._1).append(" -> ").append(tuple._2).append('\n'))
    Ok(sb.toString)    
  }
  
  private def printFormErrors[T](formWithErrors: Form[T]) = {
    val sb = new StringBuilder("Errors: \n")
    formWithErrors.errors.foreach( error => {
      sb.append(error).append("\n")
      println(error)
    })
    BadRequest(sb.toString);
  }
}
