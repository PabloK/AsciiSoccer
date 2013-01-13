#player object
player = (name, character, color) ->
  color = 0  if typeof color is "undefined"
  @character = character
  @name = name
  @color = country_colors[color]
  @X = 0
  @Y = 0
  @setPosition = (X, Y) ->
    @X = X
    @Y = Y

  @draw = ->
    ctx.fillStyle = @color
    ctx.fillRect 10 * (@X - 1), 20 * (@Y - 1), 10, 20

#Parse an action from the server stream
parse_action = (str) ->
  regex = /start.*\n(.*)?\n((.|[\n\r])*)end/g
  match = regex.exec(str)
  @type = match[1]
  @data = match[2].replace(RegExp(" ", "g"), "").split("\n")
  @data.pop()

#The game variables
game = (arr) ->
  @set_player = (name, character, color) ->
    temp_player = new player(name, character, color)
    @players.push temp_player

  @team_1_score = 0
  @team_2_score = 0
  @team_1_country = arr[1]
  @team_2_country = arr[2]
  @players = []
  i = 1
  while i <= arr[0]
    if i % 2 is 0
      @set_player arr[3 + (i - 1) * 4], arr[4 + (i - 1) * 3], @team_1_country
    else
      @set_player arr[3 + (i - 1) * 4], arr[4 + (i - 1) * 3], @team_2_country
    i++
  @time = 90
  @ball = new player("ball", "O")
  @ball.setPosition 50, 15
  @update_game = (arr) ->
    @ball.setPosition arr[0], arr[1]
    i = 0
    while i < @players.length
      @players[i].setPosition arr[i * 2 + 2], arr[3 + i * 2]
      i++
    game.team_1_score += 1  if arr[arr.length] is "1"
    game.team_2_score += 1  if arr[arr.length] is "2"
    draw_court()
    @draw()

  @draw = ->
    @ball.draw()
    i = 0
    while i < @players.length
      @players[i].draw()
      i++

#Execute actions from the server as they arrive
do_action = (str) ->
  tempAction = new parse_action(str)
  switch tempAction.type
    when "chose_country"
      socket.send "#{rand(4)+1}"  if tempAction.data is "1"
      
      socket.send("#{['Pablo',"Daniel","Olof","Lisa","Nille","Jimmy"][rand(6)]}");
      socket.send("#{["P","A","C","D","E","F"][rand(6)]}");
    when "update_gameboard"
      current_game.update_game tempAction.data
    when "game_setup"
      current_game = new game(tempAction.data)
    else
      null

#Draw the court
draw_court = ->
  ctx.fillStyle = "3016b0"
  ctx.fillRect 0, 0, 1000, 600
  ctx.fillStyle = "2DD700"
  ctx.fillRect 10, 20, 980, 560
  ctx.fillRect 0, 20 * 12, 10, 20 * 6
  ctx.fillRect 990, 20 * 12, 10, 20 * 6

#Initiate server contact
connect = ->
  try
    host = "ws://107.22.250.184:8080/#{@port}"
    socket = new WebSocket(host)
    socket.onopen = ->

    socket.onmessage = (msg) ->
      collected_msg += msg.data
      if collected_msg.match(/start(.|[\n\r])*?end/)
        do_action collected_msg
        collected_msg = ""

    socket.onclose = ->

    
    #TODO disconnection info
    return socket
  catch exception
    alert "Socket initialization failed"


#The game variables
current_game = undefined
canvas = undefined
ctx = undefined
socket = undefined
collected_msg = ""
country_colors = ["#FFFFFF", "#FF0000", "#FFFF00", "#FF00FF", "00FFFF"]

#Initiate game and more
$(document).ready ->
  canvas = document.getElementById("canvas")
  ctx = canvas.getContext("2d")
  draw_court()
  socket = connect()
  $(window).keydown (e) ->
    switch e.keyCode
      when 40
        socket.send 2
      when 39
        socket.send 6
      when 38
        socket.send 8
      when 37
        socket.send 4
      when 99
        socket.send 3
      when 98
        socket.send 2
      when 97
        socket.send 1
      when 102
        socket.send 6
      when 101
        socket.send 5
      when 100
        socket.send 4
      when 105
        socket.send 9
      when 104
        socket.send 8
      when 103
        socket.send 7
      when 83
        socket.send "s"
      else
        console.log "Undefined key: " + e.keyCode
