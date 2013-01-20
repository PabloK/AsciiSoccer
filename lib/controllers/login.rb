class LogController < Sinatra::Base

  def login(user)
      if user.new_lookup
        puts user.lookup
        session[:user]=user.id
        session[:lookup]=user.lookup
        return
      end
      
      flash[:message] = "The login could not be completed."
      redirect '/'
  end

  post '/on' do
    user = User.first(:email => params[:email].downcase)
    if user and user.password == params[:password] 
      login(user)
      flash[:message] = "Login successfull."
      redirect '/' 
    end
    
    flash[:message] = "Password or username was incorrect."
    redirect '/'
    
  end

  get '/out' do
    session.delete(:user)
    session.delete(:lookup)
    redirect '/'
  end

end

class RecoverController < Sinatra::Base
  post '/' do

  end
end
