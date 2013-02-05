require 'haml'

class PortalController < Sinatra::Base
  before do
    lookup_user
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

  get '/credits' do
    haml :credits, :layout => false
  end
end
