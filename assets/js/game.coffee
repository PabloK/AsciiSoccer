#player object
class Player
  constructor: (name, character, color) ->
    color = 0  if typeof color is "undefined"
    @character = character
    @name = name
    @color = color
    @X = 0
    @Y = 0

  setPosition: (X, Y) ->
    @X = X
    @Y = Y

  draw: ->
    ctx.fillStyle = @color
    ctx.fillRect 10 * (@X - 1), 20 * (@Y - 1), 10, 20

class Ball extends Player
  constructor: (name, character, color) ->
    super(name, character, color)

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
    @team_1_score = 0
    @team_2_score = 0
    @team_1_color=config["color1"]
    @team_2_color=config["color2"]
    @ball = new Ball("Ball", "O", config["white"])
    @players = []
    @socket = undefined

  # Initiate the game action
  setup: (arr) ->
    @ball.setPosition 50, 15

    # Set correct color for each team
    @team_1_color = config["color" + arr[1]]
    @team_2_color = config["color" + arr[2]]
    # Create players for each team
    for i in [1..arr[0]] by 1
      if i % 2 is 0
        @set_player arr[3 + (i - 1) * 4], arr[4 + (i - 1) * 3], @team_2_color
      else
        @set_player arr[3 + (i - 1) * 4], arr[4 + (i - 1) * 3], @team_1_color

  # Create and add a new player to the game 
  set_player: (name, character, color) ->
    temp_player = new Player(name, character, color)
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
    
    draw_court()
    @draw()

  # Draw the game board
  draw: () ->
    @ball.draw()
    i = 0
    while i < @players.length
      @players[i].draw()
      i++

  init_socket: (server,port) ->
    @socket = new Connect(server,port)

  # Play start sound
  whistle: () ->
    $("#whistle")[0].play()
    setTimeout(
      (()-> current_game.music())
      ,2550)
    
  music: ()->
    $("#theme")[0].play()
  
  show_endscreen: () ->
    #TODO and sound depending on outcome
    $("#team1finalscore").text(@team_1_score)
    $("#team2finalscore").text(@team_2_score)
    @message.show()

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
      current_game.setup(tempAction.data)
      current_game.whistle()
      
    when "countdown"
      #TODO countdown action with sound
      null
    when "already_started"
      alert("This game is already underway")
    when "end"
      if not current_game.ended
        current_game.ended = true
        current_game.show_endscreen()
    else

#Draw the court
draw_court = ->
  ctx.fillStyle = config["background"]
  ctx.fillRect(0, 0, 1000, 600)
  ctx.fillStyle = config["white"]
  ctx.fillRect(20, 20, 960, 560)
  ctx.fillStyle = config["foreground"]
  ctx.fillRect(22, 22, 956, 556)
  ctx.fillRect(0, 20 * 12, 1000, 20*6)

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
      else
        console.log "Undefined key: " + e.keyCode
