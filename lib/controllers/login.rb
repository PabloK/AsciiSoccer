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
    user = User.first(:email => params[:email])
    if user and user.password == params[:signature] 
      login(user)
      flash[:message] = "Login successfull."
      redirect '/' 
    elsif user == nil
      new_user = User.new(:email => params[:email], :password => params[:signature])
      if new_user.save
        login(new_user)
        flash[:message] = "New account #{new_user.email} was successfully created.
                           if you accedently created this account please remove it!"
        flash[:delete] = true
        redirect '/'
      end
      flash[:message] = "The new account could not be created."
      redirect '/'
    end
    
    flash[:message] = "Password or username was incorrect."
    redirect '/'
    
  end

  post '/out' do

  end

end

class RecoverController < Sinatra::Base

  post '/' do

  end

end
class UserController < Sinatra::Base

  post '/delete/:id' do
    haml :delete
  end

  post '/delete' do
    user.get(session[:user]).destroy!
    session = nil
  end

  post '/change' do

  end

end
