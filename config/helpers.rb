class  Sinatra::Base
  def login(user)
      if user.new_lookup!
        session[:user]=user.id
        session[:lookup]=user.lookup
        return
      end
      
      @error = user.errors 
      flash[:message] = haml :error, :layout => false
      redirect '/'
  end

  def valid_password(password)
    return password.length >= 8
  end

  def popup(message)
    @message = message
    flash[:message] = haml :message, :layout => false
    redirect '/'
  end

  def lookup_user
    if session[:user]
      @user = User.get(session[:user])
      if @user == nil or not @user.valid?(session[:lookup])
        session.delete(:user)
        session.delete(:lookup)
        halt 404
      end
      if @user
        @user.update_last_activity
      end
    end

  end
end
