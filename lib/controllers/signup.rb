class SignupCotroller < Sinatra::Base
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
