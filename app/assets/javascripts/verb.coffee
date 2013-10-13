cases = ["INF", "PAST1SM", "PAST1SF", "PAST2SM", "PAST2SF", "PAST3SM", "PAST3SF", "PAST3SN", "PAST1PM", "PAST1PF", "PAST2PM", "PAST2PF", "PAST3PM", "PAST3PF","PRES1S","PRES2S","PRES3S","PRES1P","PRES2P","PRES3P","ACTIVE","IMP2S","IMP1P","IMP2P","PASSIVE", "NOUN", "PERFECT"]
infCases = ["INF", "PAST1SM", "PAST1SF", "PAST2SM", "PAST2SF", "PAST3SM", "PAST3SF", "PAST3SN", "PAST1PM", "PAST1PF", "PAST2PM", "PAST2PF", "PAST3PM", "PAST3PF", "PASSIVE", "NOUN", "PERFECT" ]
impCases = ["PRES1S","PRES2S","PRES3S","PRES1P","PRES2P","PRES3P","ACTIVE","IMP2S","IMP1P","IMP2P"]
prefixes = ["_","po_po","za_s","w_v","s_iz","za_za","do_do","prze_pre","przy_pri","u_u","od_ot","wy_vy"]
perfectiveMarker="*"

plPatterns = {}
nsPatterns = {}

getId = (lang, c) ->
  lang + c

fillCase = (id, stem, suffix, exceptions) ->
  word = ''
  if exceptions
    if exceptions[id]
      word = exceptions[id]
  if word == ''
    word = "#{stem}#{suffix}"
  $('#'+id).html(word)
  if id=="plINF" then $('.plPref').html(word)
  else if id=="nsINF" then $('.nsPref').html(word)

getCase = (lang, c) ->
  id = getId(lang,c)
  exceptions = if lang=="pl" then plExceptions else nsExceptions
  if exceptions[id]
      return exceptions[id]
  val = $('#'+id).html()
  return val

fill = (lang, infStem, impStem, suffices, exceptions) ->
  for c in infCases
    id = getId(lang,c)
    suffix = suffices[c]
    fillCase(id,infStem,suffix,exceptions)
  for c in impCases
    id = getId(lang,c)
    suffix = suffices[c]
    fillCase(id,impStem,suffix,exceptions)
                  
refreshPlResult = ->
  infStem = $('#plInfStem').val()
  impStem = $('#plImpStem').val()
  if impStem == ''
  	impStem = infStem 
  	$('#plImpStem').val(impStem)
  patternName = $('#plPattern').val()
  suffices = plPatterns[patternName]
  fill('pl',infStem,impStem,suffices,plExceptions)
  
refreshNsResult = ->
  infStem = $('#nsInfStem').val()
  impStem = $('#nsImpStem').val()
  if impStem == ''
    impStem = infStem 
    $('#nsImpStem').val(impStem)
  patternName = $('#nsPattern').val()
  suffices = nsPatterns[patternName]
  fill('ns',infStem,impStem,suffices,nsExceptions)

$('#plInfStem').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshPlResult()

$('#plImpStem').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshPlResult()
    
$('#nsInfStem').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshNsResult()

$('#nsImpStem').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshNsResult()

$('#plPattern').change -> 
  refreshPlResult()

$('#nsPattern').change -> 
  refreshNsResult()

plExceptions = {}

$('#clearPlExceptions').click ->
  for key of plExceptions
    $('#'+key+'_except').show()
    $('#'+key+'_unexcept').hide() 
  plExceptions = {}
  refreshPlExceptions()
  refreshPlResult()

refreshPlExceptions = ->
  str = ""
  strDiv = ""
  for key of plExceptions
    keyNoLang = key.substring(2)
    t = keyNoLang + ":" + plExceptions[key]
    str = str + t + ","
    strDiv = strDiv + t + "<br>"
  $('#plExceptions').val(str)
  strDiv = "<font size=-1>#{strDiv}</font>"
  $('#plExceptionsDiv').html(strDiv)

addException = (lang,key,word) ->
  excs = if lang=="pl" then plExceptions else nsExceptions
  excs[key] = word
  if lang=="pl"
    refreshPlExceptions()
    refreshPlResult()
  else
    refreshNsExceptions()
    refreshNsResult()
   
nsExceptions = {}

$('#clearNsExceptions').click ->
  for key of nsExceptions
    $('#'+key+'_except').show()
    $('#'+key+'_unexcept').hide() 
  nsExceptions = {}
  refreshNsExceptions()
  refreshNsResult()

refreshNsExceptions = ->
  str = ""
  strDiv = ""
  for key of nsExceptions
    keyNoLang = key.substring(2)
    t = keyNoLang + ":" + nsExceptions[key]
    str = str + t + ","
    strDiv = strDiv + t + "<br>"
  $('#nsExceptions').val(str)
  strDiv = "<font size=-1>#{strDiv}</font>"
  $('#nsExceptionsDiv').html(strDiv)
  
refresh = ->
  request = $.get "/verbTemplates"
  request.error (jqXHR, textStatus, errorThrown) -> 
    $('#errors').append("AJAX Error: ${textStatus}.")
  request.success (data) -> 
    $.each data, (index, item) ->
      t = item
      if(item.lang == "ns") 
        nsPatterns[item.mode] = t
      else 
        plPatterns[item.mode] = t
    refreshPlResult()
    refreshNsResult()

$('#exceptionInput').keyup (e) ->
  if(e.which == 13) 
    word = $('#exceptionInput').val()
    key = $('#exceptionKey').val()
    lang = $('#exceptionLang').val()
    addException(lang,key,word)
    $('#'+key+'_except').hide()
    $('#'+key+'_unexcept').show()    
    hidePopup()

showPopup = (event,lang,c) ->
  $('#exceptionInput').val(getCase(lang,c))
  $('#exceptionKey').val(getId(lang,c))
  $('#exceptionLang').val(lang)

  tempX = if (document.all) then event.clientX + document.body.scrollLeft else event.pageX
  tempY = if (document.all) then event.clientY + document.body.scrollLeft else event.pageY
  if (tempX < 0)
    tempX = 0
  if (tempY < 0)
    tempY = 0;

  topStr = '' + tempY + 'px'
  leftStr = '' + tempX + 'px'
 
  $('#popupcontent').css 
    position: 'absolute'
    display: 'block'
    top: topStr
    left: leftStr
    width: '200px'
    height: '50px'
    overflow: 'auto'
    border: '1px solid #CCC'
    padding: '5px'
  $('#popupcontent').css('background-color','#F9F9F9')
  $('#popupcontent').select()
  
hidePopup = ->
  $('#popupcontent').css 
    display: 'none'

unexcept = (lang, c) ->
  excs = if lang=="pl" then plExceptions else nsExceptions
  id = getId(lang,c)
  delete excs[id]

  $('#'+id+'_except').show()
  $('#'+id+'_unexcept').hide()    
  if lang=="pl"
    refreshPlExceptions()
    refreshPlResult()
  else
    refreshNsExceptions()
    refreshNsResult()

initInput = (lang) ->
  excs = if lang=="pl" then plExceptions else nsExceptions
  for c in cases 
    do(lang,c) ->
      id = getId(lang,c) 
      $('#'+id+'_except').click (e) ->  
        showPopup(e,lang,c)
      $('#'+id+'_unexcept').click ->
        unexcept(lang,c)

initPrefixes = ->
  for pre in prefixes
    $('#'+pre+'_prefix').attr('checked',pre == '_')
    $('#'+pre+'_perfective').attr('checked',pre != '_')
    $('#'+pre+'_perfective').attr('disabled',pre != '_')
    do(pre) ->
      $('#'+pre+'_prefix').change ->
        if $('#'+pre+'_prefix').is(':checked')
        	$('#'+pre+'_perfective').attr('disabled',false)
        else
        	$('#'+pre+'_perfective').attr('disabled',true)
    
initInputs = ->
  initInput('pl') 
  initInput('ns')
  initPrefixes()

$('#statusbar').click ->
  hidePopup()

setPrefixesInput = ->
  prefixesStr = ''
    
  for pre in prefixes
    if $('#'+pre+'_prefix').is(':checked')
      t = pre
      if $('#'+pre+'_perfective').is(':checked')
        t = perfectiveMarker + t
      prefixesStr += t + ','
  
  $('#prefixes').val(prefixesStr)
  
$('#submitButton').click ->
  setPrefixesInput()
  
  impStem = $('#plImpStem').val()
  if impStem == ''
    infStem = $('#plInfStem').val()
    $('#plImpStem').val(infStem)
  	
  impStem = $('#nsImpStem').val()
  if impStem == ''
    infStem = $('#nsInfStem').val()
    $('#nsImpStem').val(infStem)  	

  $('#verbForm').submit()

$ ->
  initInputs()
  refresh()
  hidePopup()
  $('#nsInfStem').focus()