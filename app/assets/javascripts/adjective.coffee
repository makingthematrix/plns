casesSing = [ "NOMS","GENS","DATS","ACCS","VOCS","LOCS","INSS" ]
casesPl = [ "NOMP","GENP","DATP","ACCP","VOCP","LOCP","INSP" ]

plCmp = "sz"
nsCmp = "š"

plHardSuffices = {}
plSoftSuffices = {}
nsSuffices = {}

plAdvHardSuffices = {}
plAdvSoftSuffices = {}
nsAdvSuffices = {}

getId = (lang,f,r,c) ->
  lang + f + r + c

fillCase = (id, root, suffix, exceptions) ->
  word = ''
  if exceptions
    if exceptions[id]
      word = exceptions[id]
  if word == ''
    word = "#{root}#{suffix}"
  $('#'+id).html(word)
 
getCase = (lang, f, r, c) ->
  id = getId(lang,f,r,c)
  exceptions = if lang=="pl" then plExceptions else nsExceptions
  if exceptions[id]
      return exceptions[id]
  val = $('#'+id).html()
  return val

advFill = (lang, ind, cmp, suffices) -> 
  $('#'+lang+"i").html("#{ind}#{suffices.ind}")
  if isCmpIgnored()
    $('#'+lang+"c").html("---")
  else
    $('#'+lang+"c").html("#{cmp}#{suffices.cmp}")
    
fill = (lang, f, r, root, cases, suffices, exceptions) ->
  table = suffices[f]
  ignore = if r == 'c' then isCmpIgnored() else false 
  for c in cases 
    id = getId(lang,f,r,c)
    if(ignore)
      fillCase(id,'---','',null)
    else
      suffix = table[c]
      fillCase(id,root,suffix,exceptions)

refreshPlAdvResult = ->
  ind = $('#plAdvInd').val()
  ind = $('#plInd').val() if ind == ''
  cmp = $('#plAdvCmp').val()
  cmp = ind if cmp == ''
  mode = $('#plAdvMode').val()
  suffices = if mode == "HARD" then plAdvHardSuffices else plAdvSoftSuffices
  advFill("pl",ind,cmp,suffices)
    
refreshPlResult = ->
  ind = $('#plInd').val()
  cmp = $('#plCmp').val()
  cmp = ind if cmp == ''
  mode = $('#plMode').val()
  suffices = if(mode == 'HARD') then plHardSuffices else plSoftSuffices
  fill('pl','m','i',ind,casesSing,suffices,plExceptions)
  fill('pl','f','i',ind,casesSing,suffices,plExceptions)
  fill('pl','n','i',ind,casesSing,suffices,plExceptions)
  fill('pl','p','i',ind,casesPl,suffices,plExceptions)
  fill('pl','P','i',ind,casesPl,suffices,plExceptions)
  fill('pl','m','c',cmp+plCmp,casesSing,plHardSuffices,plExceptions)
  fill('pl','f','c',cmp+plCmp,casesSing,plHardSuffices,plExceptions)
  fill('pl','n','c',cmp+plCmp,casesSing,plHardSuffices,plExceptions)
  fill('pl','p','c',cmp+plCmp,casesPl,plHardSuffices,plExceptions)
  fill('pl','P','c',cmp+plCmp,casesPl,plHardSuffices,plExceptions)

refreshNsAdvResult = ->
  ind = $('#nsAdvInd').val()
  ind = $('#nsInd').val() if ind == ''
  cmp = $('#nsAdvCmp').val()
  cmp = ind if cmp == ''
  advFill("ns",ind,cmp,nsAdvSuffices)
    
refreshNsResult = ->
  ind = $('#nsInd').val()
  cmp = $('#nsCmp').val()
  cmp = ind if cmp == ''
  fill('ns','m','i',ind,casesSing,nsSuffices,nsExceptions)
  fill('ns','f','i',ind,casesSing,nsSuffices,nsExceptions)
  fill('ns','n','i',ind,casesSing,nsSuffices,nsExceptions)
  fill('ns','p','i',ind,casesPl,nsSuffices,nsExceptions)
  fill('ns','m','c',cmp+nsCmp,casesSing,nsSuffices,nsExceptions)
  fill('ns','f','c',cmp+nsCmp,casesSing,nsSuffices,nsExceptions)
  fill('ns','n','c',cmp+nsCmp,casesSing,nsSuffices,nsExceptions)
  fill('ns','p','c',cmp+nsCmp,casesPl,nsSuffices,nsExceptions)

$('#plAdvInd').keyup (e) -> 
  if(e.which == 13) 
    false 
  else 
    refreshPlAdvResult()

$('#plAdvCmp').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshPlAdvResult()

$('#plAdvMode').change -> 
  refreshPlAdvResult()

$('#nsAdvInd').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshNsAdvResult()
        
$('#nsAdvCmp').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshNsAdvResult()
    
$('#plInd').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshPlResult()
    refreshPlAdvResult()

$('#plCmp').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshPlResult()
    refreshPlAdvResult()
    
$('#plMode').change -> 
  refreshPlResult()

$('#nsInd').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshNsResult()
    refreshNsAdvResult()
        
$('#nsCmp').keyup (e) -> 
  if(e.which == 13) 
    false
  else 
    refreshNsResult()
    refreshNsAdvResult()
    
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

isCmpIgnored = ->
  $('#cmpIgnored').val() == 'true'

refreshCmpIgnored = ->
  if(isCmpIgnored())
    $('#plCmp').hide()
    $('#plAdvCmp').hide()
    $('#nsCmp').hide()
    $('#nsAdvCmp').hide()
  else
    $('#plCmp').show()
    $('#plAdvCmp').show()
    $('#nsCmp').show()
    $('#nsAdvCmp').show()
    
$('#ignoreCheckbox').change ->
  ignored = if $('#ignoreCheckbox').is(':checked') then "true" else "false"
  $('#cmpIgnored').val( ignored )
  refreshCmpIgnored()
  refreshPlResult()
  refreshNsResult()
  refreshPlAdvResult()
  refreshNsAdvResult()

refresh = ->
  request = $.get "/adjectiveTemplates"
  request.error (jqXHR, textStatus, errorThrown) -> 
    $('#errors').append("AJAX Error: ${textStatus}.")
  request.success (data) -> 
    $.each data, (index, item) ->
      t = { cmp: item.cmp, m: item.m, f: item.f, n: item.n, p: item.p, P: item.P }
      if(item.lang == "ns") 
        nsSuffices = t
      else if(item.mode == "HARD") 
        plHardSuffices = t
      else 
        plSoftSuffices = t
    refreshCmpIgnored()
    refreshPlResult()
    refreshNsResult()
  advRequest = $.get "/adverbTemplates"
  advRequest.error (jqXHR, textStatus, errorThrown) -> 
    $('#errors').append("AJAX Error: ${textStatus}.")
  advRequest.success (data) -> 
    $.each data, (index, item) ->
      t = { ind: item.ind, cmp: item.cmp, sup: item.cmp }
      if(item.lang == "ns") 
        nsAdvSuffices = t
      else if(item.mode == "HARD") 
        plAdvHardSuffices = t
      else 
        plAdvSoftSuffices = t
    refreshPlAdvResult()
    refreshNsAdvResult()

showPopup = (event,lang,f,r,c) ->
  $('#exceptionInput').val(getCase(lang,f,r,c))
  $('#exceptionKey').val(getId(lang,f,r,c))
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

unexcept = (lang, f, r, c) ->
  excs = if lang=="pl" then plExceptions else nsExceptions
  id = getId(lang,f,r,c)
  delete excs[id]

  $('#'+id+'_except').show()
  $('#'+id+'_unexcept').hide()    
  if lang=="pl"
    refreshPlExceptions()
    refreshPlResult()
  else
    refreshNsExceptions()
    refreshNsResult()

initInput = (lang, f, r, cases) ->
  excs = if lang=="pl" then plExceptions else nsExceptions
  for c in cases 
    do(lang,f,r,c) ->
      id = getId(lang,f,r,c) 
      $('#'+id+'_except').click (e) ->  
        showPopup(e,lang,f,r,c)
      $('#'+id+'_unexcept').click ->
        unexcept(lang,f,r,c)

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
  initInput('pl','m','i',casesSing) 
  initInput('pl','m','i',casesSing)
  initInput('pl','f','i',casesSing)
  initInput('pl','n','i',casesSing)
  initInput('pl','p','i',casesPl)
  initInput('pl','P','i',casesPl)
  initInput('pl','m','c',casesSing)
  initInput('pl','f','c',casesSing)
  initInput('pl','n','c',casesSing)
  initInput('pl','p','c',casesPl)
  initInput('pl','P','c',casesPl)
  initInput('ns','m','i',casesSing)
  initInput('ns','f','i',casesSing)
  initInput('ns','n','i',casesSing)
  initInput('ns','p','i',casesPl)
  initInput('ns','m','c',casesSing)
  initInput('ns','f','c',casesSing)
  initInput('ns','n','c',casesSing)
  initInput('ns','p','c',casesPl)

$('#statusbar').click ->
  hidePopup()

$ ->
  initInputs()
  refresh()
  hidePopup()
  $('#nsInd').focus()
