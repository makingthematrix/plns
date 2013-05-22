refreshResult = ->
	pl = $('#plWord').val()
	ns = $('#nsWord').val()
	$('#result').html("#{pl} -> #{ns}")

$('#plWord').keyup (e) -> 
	if(e.which == 13) 
		false
	else 
		refreshResult()
		
$('#nsWord').keyup (e) -> 
	if(e.which == 13) 
		false 
	else 
		refreshResult()

$ ->
	refreshResult()
	$('#nsWord').focus()
