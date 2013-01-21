require 'haml'

class PortalController < Sinatra::Base
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

  get '/' do
    @message = flash[:message]
    haml :index
  end

  get '/loginform' do
    @text = "Log in"
    @type = "logon"
    @login = true
    @submit = '/log/on'
    haml :login, :layout => false
  end

  get '/signupform' do
    @text = "Sign up"
    @type = "signup"
    @submit = '/signup'
    haml :login, :layout => false
  end
end
