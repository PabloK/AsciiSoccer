require './app'

#
# Server configuration
#
options = {:remote_host => 'asciisoccer.herokuapp.com', :remote_port => 4343}
options = {:remote_host => 'pablo-N53SN', :remote_port => 4343}

#
# Event machine proxy and sinatra Application
#
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
