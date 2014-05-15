setMessage = (text) ->
  cover = $("#cover-all")
  cover.show()
  message = cover.children()
  message.html(text)

popup = (configure) ->
  cover = $("#cover-all")
  message = cover.children()
  if configure["url"] != undefined
    $.get(configure["url"]).success((setMessage)).error(setMessage("<div class='heading'><h2>Error 404</h2></div><p>Could not retrive the requested form.</p><button class='button' onclick=\"$('#cover-all').hide();\">Close</button>"))
    
  if configure["text"] != undefined
    setMessage(configure["text"])

closeMessage = ()->
  $("#cover-all").hide()
  return false

submitForm = (formName, action) ->
  form = $(formName)
  form.attr("action", action)
  form.submit()
  return false

$(document).ready((->

  $(document).keydown((
    (e)->
      code = e.keyCode
      if code == 27
        closeMessage()
  ))))
