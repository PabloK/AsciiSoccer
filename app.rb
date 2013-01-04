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
        @game_server_socket = TCPSocket.open(host, port)
        @game_server_message_queue = EventMachine::Channel.new
        @queue = @game_server_message_queue.subscribe {|msg| ws.send msg}
  
        get_gameserver_messages = proc do |ws|
          puts ws
          loop do
            while line = @game_server_socket.gets
              puts "Server -> Client: " + line 
              @game_server_message_queue.push line
            end
          end
        end

        EventMachine.defer get_gameserver_messages
      }

      ws.onmessage { |msg|
        puts "Client -> Server: " + msg
        @game_server_socket.puts(msg)
      }

      ws.onclose   {
          @game_server_message_queue.unsubscribe @queue
          @game_server_socket.close()
      }

    end

  App.run!({:port => 3000})   
end
