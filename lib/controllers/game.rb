require 'haml'

class GameController < Sinatra::Base
  before do
    if session[:user]
      @user = User.get(session[:user])
      if @user == nil or not @user.valid?(session[:lookup])
        session.delete(:user)
        session.delete(:lookup)
        halt 404
      end
    end
  end

  get '/:port' do
    game = Game.get(params[:port])
    if game and game.code == session[:code]
      if game.join!
        @port = params[:port]
        haml :game, :layout => false
      end
      popup("Unable to join game on port #{game.port}. Check firewall.")
    end
    popup("Unauthorized atempt to join game.")
  end
end
