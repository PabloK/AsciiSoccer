require 'haml'

class GameController < Sinatra::Base
  before do
    lookup_user
  end

  get '/:port' do
    @muted = session[:muted] ||= false
    @game = Game.get(params[:port])
    unless @game.full?
      if @game and @game.code == session[:code]
        if @game.join!
          return haml :game, :layout => false
        end
        popup("Unable to join game on port #{@game.port}.")
      end
      popup("Unauthorized atempt to join game.")
    end
    popup("Game is already full.")
  end
end
