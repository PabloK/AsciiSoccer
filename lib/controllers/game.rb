require 'haml'

class Game < Sinatra::Base
    get '/:port' do
        @port = params[:port]
        haml :game, :layout => false
    end
end
