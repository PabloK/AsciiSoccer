require 'haml'

class PortalController < Sinatra::Base
  get '/' do
    @message = flash[:message]
    @delete = flash[:delete]
    haml :index
  end
    
  post '/signup' do
    new_user = User.new(:email => params[:email], :password => params[:password])
    if new_user.save
      redirect "/registered/#{new_user.id}"
    end
    haml :index
  end
    
  get '/registered/:id' do
    @user = User.get(params[:id])
    haml :registered
  end

  get '/loginform' do
    haml :login, :layout => false
  end
end
