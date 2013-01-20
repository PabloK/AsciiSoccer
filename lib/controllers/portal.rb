require 'haml'

class PortalController < Sinatra::Base
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
