require 'rubygems'
require 'em-websocket'
require 'sinatra/base'
require 'thin'
require 'haml'
require 'socket'


class App < Sinatra::Base
    get '/' do
        haml :index
    end
end

class ServerConnection < EventMachine::Connection

  def initialize(input, output)
    super
    @input = input
    @output = output
    @input_sid = @input.subscribe { |msg| send_data msg+ "\n" }
  end

  def receive_data(msg)
    @output.push(msg)
  end

  def unbind
    @input.unsubscribe(@input_sid)
  end

end

# Configuration of server
options = {:remote_host => 'pablo-N53SN', :remote_port => 4343}

EventMachine.run do   

  EventMachine::WebSocket.start(:host => '0.0.0.0', :port => 8080) do |ws|
    ws.onopen {
      output = EM::Channel.new
      input = EM::Channel.new

      output_sid = output.subscribe { |msg| ws.send msg; }
      
      EventMachine::connect options[:remote_host], options[:remote_port], ServerConnection, input, output

      ws.onmessage { |msg| input.push(msg)}

      ws.onclose {
        output.unsubscribe(output_sid)
      }
    }
  end

  App.run!({:port => 3000})   

end
