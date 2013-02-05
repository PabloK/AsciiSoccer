class Audio
  # Play start sound
  @whistle: () ->
    $("#whistle")[0].play()
        
  @music: ()->
    $("#theme")[0].play()
  
  # Unmute all audio tags
  @unmute: () ->
    Audio.toggle(false)

  # Mute all audio tags
  @mute: () ->
    Audio.toggle(true)

  # Set the provided value if set, toggle otherwise
  # Returns the last set value
  @toggle: (newValue) ->
    $("audio").each(() ->
      if (newValue == undefined)
        newValue = !$(this)[0].muted
      $(this)[0].muted = newValue
      true
    )
    newValue

