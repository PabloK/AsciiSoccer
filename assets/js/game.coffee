#player object
class Player
  constructor: (name, color, is_current_player) ->
    color = 0  if typeof color is "undefined"
    @name = name
    @color = color
    @X = 0
    @Y = 0
    @is_current_player = is_current_player

  setPosition: (X, Y) ->
    @X = X
    @Y = Y

  draw: ->
    if (@is_current_player)
      ctx.fillStyle = config["border_color"]
    else
      ctx.fillStyle = config["black"]
    @drawBorder()
    ctx.fillStyle = @color

    ctx.fillRect 10 * (@X - 1) + Player.BORDER_SIZE,
                 20 * (@Y - 1) + Player.BORDER_SIZE,
                 10 - 2 * Player.BORDER_SIZE,
                 20 - 2 * Player.BORDER_SIZE

  drawBorder: ->
    # "border_size" pixels wide border
    ctx.fillRect 10 * (@X - 1), 20 * (@Y - 1), 10, 20

Player.BORDER_SIZE = 2

class Ball extends Player
  constructor: (name, color) ->
    super(name, color)

  draw: ->
    ctx.fillStyle = @color
    ctx.fillRect 10 * (@X - 1), 20 * (@Y - 1)+5, 10, 10

# Connect to the game server
class Connect
  constructor: (server,port) ->
    try
      unless /^40[0-9]{2}$/g.test(port)
        throw "Port outside server port range"
      host = server+ port
      socket = new WebSocket(host)
      socket.onopen = ->

      socket.onmessage = (msg) ->
        current_game.collected_msg += msg.data
        if current_game.collected_msg.match(/>>(.|[\n\r])*?<</)
          do_action current_game.collected_msg
          current_game.collected_msg = ""

      socket.onclose = ->
        if not current_game.ended
          alert("The connection to the server has been broken.")
      
      return socket
    catch exception
      alert "Socket initialization failed"

#Parse an action from the server stream
parse_action = (str) ->
  regex = />>.*\n(.*)?\n((.|[\n\r])*)<</g
  match = regex.exec(str)
  @type = match[1]
  @data = match[2].replace(RegExp(" ", "g"), "").split("\n")
  @data.pop()

#The game variables
class Game
  constructor: () ->
    @ended = false
    @collected_msg = ""
    @message = $("#cover-all")
    @time = 0
    @time_display = $("#time")
    @team_1_score_display = $("#team1")
    @team_2_score_display = $("#team2")
    @team_1_name_final_display = $("#team1namefinal")
    @team_2_name_final_display = $("#team2namefinal")
    @team_1_name_display = $("#team1name")
    @team_2_name_display = $("#team2name")
    @team_1_score = 0
    @team_2_score = 0
    @team_1_color=config["color1"]
    @team_2_color=config["color2"]
    @ball = new Ball("Ball", config["white"])
    @maximum_players = parseInt(config["maximum_players"])
    @players = []
    @socket = undefined
    @court = @preRenderCourt()

  preRenderCourt: () ->
    
    buffer = document.createElement('canvas')
    buffer.height = 600
    buffer.width  = 1000
    bctx = buffer.getContext('2d')

    bctx.fillStyle = config["dark-grass"]
    bctx.fillRect(0, 0, 1000, 600)
    bctx.fillStyle = config["light-grass"]
    for i in [0..26] by 2
      bctx.fillRect((i)*45, 0, 45, 600)
    bctx.fillStyle = config["background"]
    bctx.fillRect(0, 0, 20, 600)
    bctx.fillRect(980, 0, 20, 600)
    bctx.fillRect(20, 580, 980, 20)
    bctx.fillRect(20, 0, 980, 20)
    bctx.fillStyle = config["white"]
    bctx.fillRect(20, 20, 2, 558)
    bctx.fillRect(978, 20, 2, 558)
    bctx.fillRect(20, 20, 958, 2)
    bctx.fillRect(20, 578, 958, 2)
    bctx.fillStyle = config["light-grass"]
    bctx.fillRect(0, 240, 22, 120)
    bctx.fillStyle = config["dark-grass"]
    bctx.fillRect(978, 240, 22, 120)
    bctx.fillStyle = config["white"]
    bctx.fillRect(978, 234, 6, 6)
    bctx.fillRect(978, 360, 6, 6)
    bctx.fillRect(16, 234, 6, 6)
    bctx.fillRect(16, 360, 6, 6)
    bctx.fillStyle = "#6FF56F"
    bctx.fillRect(494, 22, 2, 558)

    return buffer

  # Initiate the game action
  setup: (arr) ->
    @ball.setPosition 50, 15

    # Set correct color for each team
    @team_1_color = config["color" + arr[1]]
    @team_2_color = config["color" + arr[2]]
    @team_1_name_final_display.text(arr[3])
    @team_2_name_final_display.text(arr[4])
    @team_1_name_display.text(arr[3])
    @team_2_name_display.text(arr[4])
    # Create players for each team
    for i in [1..@maximum_players] by 1
      if i % 2 is 0
        @set_player arr[5 + (i - 1) * 4], @team_2_color
      else
        @set_player arr[5 + (i - 1) * 4], @team_1_color

    # Start the game audio
    # TODO: Make the audio part work without these toggles
    Audio.toggle()
    Audio.toggle()
    Audio.whistle();
    setTimeout(
      (()-> Audio.music())
      ,2550)


  # Create and add a new player to the game 
  set_player: (name, color) ->
    is_current_player = (@players.length + 1) == parseInt(config["current_player"])
    temp_player = new Player(name, color, is_current_player)
    @players.push temp_player

  # Update game action
  update_game: (arr) ->
    @ball.setPosition arr[0], arr[1]
    @team_1_score = arr[arr.length-2]
    @team_2_score = arr[arr.length-1]
    @time = arr[arr.length-3]
    @time_display.text(90 - Math.floor(@time/20))
    @team_1_score_display.text(@team_1_score)
    @team_2_score_display.text(@team_2_score)

    i = 0
    while i < @players.length
      @players[i].setPosition arr[i * 2 + 2], arr[3 + i * 2]
      i++

    @draw()

  # Draw the game board
  draw: () ->
    ctx.drawImage(@court,0,0)
    @ball.draw()
    i = 0
    while i < @players.length
      @players[i].draw()
      i++

  init_socket: (server,port) ->
    @socket = new Connect(server,port)

  @toggleAudio: (muteAudio) ->
    isAudioMuted = Audio.toggle(muteAudio)
    # Send ajax request if toggeling only
    if muteAudio == undefined
      $.post("/settings/audio", {play_audio: !isAudioMuted})

    if (isAudioMuted)
      $("#volumeController").text("Sound off")
    else
      $("#volumeController").text("Sound on")
  
  show_endscreen: () ->
    #TODO and sound depending on outcome
    $("#team1finalscore").text(@team_1_score)
    $("#team2finalscore").text(@team_2_score)
    @message.show()

countDown = (int) ->
  unless int == 0
    $("#waiting").html("<span>"+int+"<span>")
    int--
    setTimeout((()->countDown(int)),800)
  else
    $("#waiting").html("<span>Go!!<span>")

#Execute actions from the server as they arrive
do_action = (str) ->
  tempAction = new parse_action(str)
  switch tempAction.type
    when "chose"
      #TODO lookup has to be sent to the server to verify the user id
      #TODO make into function
      current_game.socket.send(parseInt(config["signed_on"]))
      if tempAction.data[0] is "1"
        current_game.socket.send(parseInt(config['selected_color']))

      if parseInt(config["signed_on"]) == 1
        if tempAction.data[0] is "1"
          current_game.socket.send (config['team_name'])
        current_game.socket.send(config['user_id'])
    when "update"
      current_game.update_game tempAction.data
    when "setup"
      $("#onscreen").hide()
      current_game.setup(tempAction.data)
      
    when "already_started"
      alert("This game is already underway")
    when "end"
      if not current_game.ended
        current_game.ended = true
        current_game.show_endscreen()
    when "count_down"
      countDown(3)
    when "update_con"
      players_left_to_connect = current_game.maximum_players - parseInt(tempAction.data[0])
      $("#waiting").html("<span>Waiting for " + players_left_to_connect + " players to connect...</span>")
    else

#The game variables
current_game = undefined
canvas = undefined
ctx = undefined
socket = undefined
config = []

#Initiate game and more
$(document).ready ->

  canvas = document.getElementById("canvas")
  ctx = canvas.getContext("2d")

  for element in $("#gameConfig").children()
    jQElement = $(element)
    config[jQElement.attr('id')] = jQElement.text()

  #Handling audio
  mutedAudio = config["audio_muted"] == "true"
  Game.toggleAudio(mutedAudio) 
  #Initiating game
  current_game = new Game()
  current_game.init_socket(config['host'],config['port'])

  #Handling Input
  $(window).keydown (e) ->
    switch e.keyCode
      when 40
        current_game.socket.send 2
      when 39
        current_game.socket.send 6
      when 38
        current_game.socket.send 8
      when 37
        current_game.socket.send 4
      when 99
        current_game.socket.send 3
      when 98
        current_game.socket.send 2
      when 97
        current_game.socket.send 1
      when 102
        current_game.socket.send 6
      when 101
        current_game.socket.send 5
      when 100
        current_game.socket.send 4
      when 105
        current_game.socket.send 9
      when 104
        current_game.socket.send 8
      when 103
        current_game.socket.send 7
      when 65
        current_game.socket.send "a"
      when 68
        current_game.socket.send "d"
      when 83
        current_game.socket.send "s"
      else
        console.log "Undefined key: " + e.keyCode
