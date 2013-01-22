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
end