setMessage = (text) ->
  cover = $("#cover-all")
  message = cover.children()
  message.html(text)

popup = (configure) ->
  cover = $("#cover-all")
  message = cover.children()
  cover.show()
  if configure["url"] != undefined
    $.get(configure["url"]).success((setMessage)).error(setMessage("<div class='heading'><h2>Error 404</h2></div><p>Could not retrive the login form. Please try again later</p>")
    )
    
  if configure["text"] != undefined
    setMessage(configure["text"])

submitForm = (formName, action) -> 
  form = $(formName)
  console.log(form)
  form.attr("action", action)
  form.submit()
  return false

$(document).ready((->
  cover = $("#cover-all")
  $(document).keydown((
    (e)->
      code = e.keyCode
      if code == 27
        cover.hide()
  ))))
