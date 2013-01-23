#Helper function to give smooth key press
holdit = (btn, action, start, speedup) ->
  t = undefined
  time = start
  repeat = ->
    action()
    t = setTimeout(repeat, time + 50)
    time = time / speedup

  btn.mousedown(() ->
    repeat()
  )

  btn.mouseup(() ->
    clearTimeout t
    time = start
  )

  btn.mouseout(() ->
    clearTimeout t
    time = start
  )
  btn.mouseleave(() ->
    clearTimeout t
    time = start
  )
