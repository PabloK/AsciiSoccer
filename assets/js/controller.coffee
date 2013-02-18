# Controler Object
class Controller
  constructor: (socket) ->
    $(window).keydown (e) ->
      switch e.keyCode
        when 40 , 98
          socket.send 2
        when 39 , 102
          socket.send 6
        when 38 , 104
          socket.send 8
        when 37 , 100
          socket.send 4
        when 99
          socket.send 3
        when 98
          socket.send 2
        when 97
          socket.send 1
        when 101
          socket.send 5
        when 105
          socket.send 9
        when 103
          socket.send 7
        when 65
          socket.send "a"
        when 68
          socket.send "d"
        when 83
          socket.send "s"
        else
          null
