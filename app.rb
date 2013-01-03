require 'rubygems'
require 'em-websocket'
require 'sinatra/base'
require 'thin'
require 'haml'

EventMachine.run do   
  class App < Sinatra::Base
      get '/' do
          haml :index
      end
  end

  EventMachine::WebSocket.start(:host => '0.0.0.0', :port => 8080) do |ws|
      ws.onopen {
          ws.send "connected!!!!"
      }

      ws.onmessage { |msg|
          puts "got message #{msg}"
      }

      ws.onclose   {
          ws.send "WebSocket closed"
      }
  end

  App.run!({:port => 3000})   
end
