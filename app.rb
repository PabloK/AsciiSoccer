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
