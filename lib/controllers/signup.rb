class SignupController < Sinatra::Base
  post '/' do
    unless valid_password(params[:password])
      @error = [["Password needs to be at least eight characters long."]]
      flash[:message] = haml :error, :layout => false
      redirect '/'
    end
      
    user = User.first(:email => params[:email].downcase)
    if user
      popup("Username already registered")
    end 
    new_user = User.new 
    new_user.email = params[:email]
    new_user.password = params[:password]

    if new_user.save
      login(new_user)
      popup("New account #{new_user.email} was successfully created.")
    end

    @error = new_user.errors
    flash[:message] = haml :error, :layout => false
    redirect '/'
  end
end
