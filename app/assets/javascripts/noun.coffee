cases = [ "NOMS","GENS","DATS","ACCS","VOCS","LOCS","INSS","NOMP","GENP","DATP","ACCP","VOCP","LOCP","INSP" ]
casesSing = [ "NOMS","GENS","DATS","ACCS","VOCS","LOCS","INSS"]
casesPl = ["NOMP","GENP","DATP","ACCP","VOCP","LOCP","INSP" ]

plPatterns = {}
nsPatterns = {}

isIgnored = (plural) ->
  ignoredValue = $('#plIgnored').val()
  if plural
    ignoredValue == "plural"
  else
    ignoredValue == "singular"

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

getCase = (lang, c) ->
  id = getId(lang,c)
  exceptions = if lang=="pl" then plExceptions else nsExceptions
  if exceptions[id]
      return exceptions[id]
  val = $('#'+id).html()
  return val
  
fill = (lang, stem, suffices, exceptions) ->
  if not isIgnored(false)
    for c in casesSing 
      id = getId(lang,c)
      suffix = suffices[c]
      fillCase(id,stem,suffix,exceptions)
  else
    for c in casesSing
      id = getId(lang,c)
      fillCase(id,'---','',null)
      
  if not isIgnored(true)
    for c in casesPl
      id = getId(lang,c)
      suffix = suffices[c]
      fillCase(id,stem,suffix,exceptions)
  else
    for c in casesPl
      id = getId(lang,c)
      fillCase(id,'---','',null)
              
refreshPlResult = ->
  stem = $('#plStem').val()
  patternName = $('#plPattern').val()
  suffices = plPatterns[patternName]
  fill('pl',stem,suffices,plExceptions)

refreshNsResult = ->
  stem = $('#nsStem').val()
  patternName = $('#nsPattern').val()
  suffices = nsPatterns[patternName]
  fill('ns',stem,suffices,nsExceptions)

$('#plStem').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshPlResult()
 
$('#ignored').change ->
  refreshPlResult()
  refreshNsResult()
    
$('#nsStem').keyup (e) -> 
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
  request = $.get "/nounTemplates"
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

$('#exceptionInput').keyup (e) ->
  if(e.which == 13) 
    word = $('#exceptionInput').val()
    key = $('#exceptionKey').val()
    lang = $('#exceptionLang').val()
    addException(lang,key,word)
    $('#'+key+'_except').hide()
    $('#'+key+'_unexcept').show()    
    hidePopup()

initInputs = ->
  initInput('pl') 
  initInput('ns')

$('#statusbar').click ->
  hidePopup()

$ ->
  initInputs()
  refresh()
  hidePopup()
  $('#nsStem').focus()
