class SettingsCotroller < Sinatra::Base

  before do
    if session[:user]
      @user = User.get(session[:user])
      if @user == nil or not @user.valid?(session[:lookup])
        session.delete(:user)
        session.delete(:lookup)
        halt 404
      end
    end
  end

  get '/' do
    haml :usersettings, :layout => false
  end

  post '/' do
    if @user.update!(:name => params[:name], :color => params[:color])
      flash[:message] = "Your settings were successfully saved."
      redirect '/'
    end
    @error = @user.errors
    flash[:message] = haml :error, :layout => false
    redirect '/'
  end

end
