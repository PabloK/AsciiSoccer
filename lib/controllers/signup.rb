class SignupCotroller < Sinatra::Base

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

  post '/' do
    user = User.first(:email => params[:email].downcase)
    if user
      flash[:message] = "Username already registered"
      redirect '/'
    end 
    new_user = User.new(:email => params[:email], :password => params[:signature])
    if new_user.save
      login(new_user)
      flash[:message] = "New account #{new_user.email} was successfully created."
      redirect '/'
    end

    flash[:message] = "The new account could not be created."
    redirect '/'
  end
end
