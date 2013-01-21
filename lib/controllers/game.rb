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
    @port = params[:port]
    haml :game, :layout => false
  end
end
