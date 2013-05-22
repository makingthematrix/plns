plHardSuffices = {}
plSoftSuffices = {}
nsSuffices = {}

fill = (lang, ind, cmp, sup, suffices) ->
  $('#'+lang+"i").html("#{ind}#{suffices.ind}")
  if isCmpIgnored()
    $('#'+lang+"c").html("---")
    $('#'+lang+"s").html("---")
  else
    $('#'+lang+"c").html("#{cmp}#{suffices.cmp}")
    $('#'+lang+"s").html("#{sup}#{suffices.sup}")
	
refreshPlResult = ->
  ind = $('#plInd').val()
  cmp = $('#plCmp').val()
  cmp = ind if cmp == ''
  mode = $('#plMode').val()
  suffices = if mode == "HARD" then plHardSuffices else plSoftSuffices
  fill("pl",ind,cmp,"naj#{cmp}",suffices)
	
refreshNsResult = ->
  ind = $('#nsInd').val()
  cmp = $('#nsCmp').val()
  cmp = ind if cmp == ''
  fill("ns",ind,cmp,"naj#{cmp}",nsSuffices)

$('#plInd').keyup (e) -> 
	if(e.which == 13) 
		false
	else 
		refreshPlResult()

$('#plCmp').keyup (e) -> 
	if(e.which == 13) 
		false
	else 
		refreshPlResult()

$('#plMode').change -> 
	refreshPlResult()

$('#nsInd').keyup (e) -> 
	if(e.which == 13) 
		false
	else 
		refreshNsResult()
				
$('#nsCmp').keyup (e) -> 
	if(e.which == 13) 
		false
	else 
		refreshNsResult()

isCmpIgnored = ->
  $('#cmpIgnored').is(':checked')
  
refreshCmpIgnored = ->
  if isCmpIgnored()
    $('#plCmp').hide()
    $('#nsCmp').hide()
  else
    $('#plCmp').show()
    $('#nsCmp').show()
    
$('#cmpIgnored').change ->
  refreshCmpIgnored()
  refreshPlResult()
  refreshNsResult()
  		
refresh = ->
  request = $.get "/adverbTemplates"
  request.error (jqXHR, textStatus, errorThrown) -> 
    $('#errors').append("AJAX Error: ${textStatus}.")
  request.success (data) -> 
    $.each data, (index, item) ->
      t = { ind: item.ind, cmp: item.cmp, sup: item.cmp }
      if(item.lang == "ns") 
        nsSuffices = t
      else if(item.mode == "HARD") 
        plHardSuffices = t
      else 
        plSoftSuffices = t
    refreshPlResult()
    refreshNsResult()

$ ?= require 'jquery' # For Node.js compatibility
$ ->
  refresh()
  $('#nsInd').focus()
