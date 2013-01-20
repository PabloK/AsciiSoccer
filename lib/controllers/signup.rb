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

  def valid_password(password)
    return password.length >= 8
  end

  post '/' do
    unless valid_password(params[:password])
      @error = [["Password needs to be atleast eight characters long."]]
      flash[:message] = haml :error, :layout => false
      redirect '/'
    end
      
    user = User.first(:email => params[:email].downcase)
    if user
      flash[:message] = "Username already registered"
      redirect '/'
    end 
    new_user = User.new(:email => params[:email], :password => params[:password])
    if new_user.save
      login(new_user)
      flash[:message] = "New account #{new_user.email} was successfully created."
      redirect '/'
    end
    @error = new_user.errors
    flash[:message] = haml :error, :layout => false
    redirect '/'
  end
end
