class SettingsController < Sinatra::Base

  before do
    lookup_user
  end

  get '/' do
    haml :usersettings, :layout => false
  end

  post '/' do
    if @user.update(:name => params[:name], :color => params[:color], :audio_muted => !params[:play_audio])
      popup("Your settings were successfully saved.")
    end
    @error = @user.errors
    flash[:message] = haml :error, :layout => false
    redirect '/'
  end

  post '/audio' do
    # TODO: Errors are not handled using ajax, fix?
    session[:muted] = params[:play_audio] != "true"
    if @user
      unless @user.update(:audio_muted => params[:play_audio] != "true")
        @error = @user.errors
      end
    end
  end

end

