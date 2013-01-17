require 'haml'

class PortalController < Sinatra::Base
    get '/' do
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
end
