cases = ["INF", "PAST1SM", "PAST1SF", "PAST2SM", "PAST2SF", "PAST3SM", "PAST3SF", "PAST3SN", "PAST1PM", "PAST1PF", "PAST2PM", "PAST2PF", "PAST3PM", "PAST3PF","PRES1S","PRES2S","PRES3S","PRES1P","PRES2P","PRES3P","ACTIVE","IMP2S","IMP1P","IMP2P","PASSIVE", "NOUN"]
infCases = ["INF", "PAST1SM", "PAST1SF", "PAST2SM", "PAST2SF", "PAST3SM", "PAST3SF", "PAST3SN", "PAST1PM", "PAST1PF", "PAST2PM", "PAST2PF", "PAST3PM", "PAST3PF", "PASSIVE", "NOUN" ]
impCases = ["PRES1S","PRES2S","PRES3S","PRES1P","PRES2P","PRES3P","ACTIVE","IMP2S","IMP1P","IMP2P"]

plPatterns = {}
nsPatterns = {}

getId = (lang, c) ->
  lang + c

fillCase = (id, root, suffix, exceptions) ->
  word = ''
  if exceptions
    if exceptions[id]
      word = exceptions[id]
  if word == ''
    word = "#{root}#{suffix}"
  $('#'+id).html(word)

getCase = (lang, c) ->
  id = getId(lang,c)
  exceptions = if lang=="pl" then plExceptions else nsExceptions
  if exceptions[id]
      return exceptions[id]
  val = $('#'+id).html()
  return val

fill = (lang, infRoot, impRoot, suffices, exceptions) ->
  for c in infCases
    id = getId(lang,c)
    suffix = suffices[c]
    fillCase(id,infRoot,suffix,exceptions)
  for c in impCases
    id = getId(lang,c)
    suffix = suffices[c]
    fillCase(id,impRoot,suffix,exceptions)
                  
refreshPlResult = ->
  infRoot = $('#plInfRoot').val()
  impRoot = $('#plImpRoot').val()
  patternName = $('#plPattern').val()
  suffices = plPatterns[patternName]
  fill('pl',infRoot,impRoot,suffices,plExceptions)
  
refreshNsResult = ->
  infRoot = $('#nsInfRoot').val()
  impRoot = $('#nsImpRoot').val()
  patternName = $('#nsPattern').val()
  suffices = nsPatterns[patternName]
  fill('ns',infRoot,impRoot,suffices,nsExceptions)

$('#plInfRoot').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshPlResult()

$('#plImpRoot').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshPlResult()
    
$('#nsInfRoot').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshNsResult()

$('#nsImpRoot').keyup (e) -> 
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
        
initInputs = ->
  initInput('pl') 
  initInput('ns')

$('#statusbar').click ->
  hidePopup()

$ ->
  initInputs()
  refresh()
  hidePopup()
  $('#nsRoot').focus()