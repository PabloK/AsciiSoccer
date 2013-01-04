require 'rubygems'
require 'em-websocket'
require 'sinatra/base'
require 'thin'
require 'haml'
require 'socket'

EventMachine.run do   
  class App < Sinatra::Base
      get '/' do
          haml :index
      end
  end

  EventMachine::WebSocket.start(:host => '0.0.0.0', :port => 8080) do |ws|

      ws.onopen {
        host = "pablo-N53SN"
        port = 4343          
        game_server_socket = TCPSocket.open(host, port)
        @game_server_message_queue = EventMachine::Channel.new
        @game_server_message_queue.subscribe {|msg| ws.send msg}
        EventMachine.defer get_gameserver_messages
  
        get_gameserver_messages = proc do
          while line = game_server_socket.gets
            @game_server_message_queue.push line
          end
        end

        ws.send "Connection Established"
      }

      ws.onmessage { |msg|
        game_server_socket.send(msg)
      }

      ws.onclose   {
          @game_server_message_queue.unsubscribe
          ws.send "Connection closed"
      }

    end

  App.run!({:port => 3000})   
end
