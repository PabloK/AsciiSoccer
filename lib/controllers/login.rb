class LogController < Sinatra::Base

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
    @user = User.first(:email => params[:email])

    if @user
      if @user.generate_recover_key!
        options = {
          :to => @user.email,
          :subject => 'AsciiSoccer - Password Reset',
          :body => 'Plain',
          :html_body => (haml :recover),
        }
        Pony.mail(options)

        flash[:message] = "A recovery email was sent to \"#{@user.email}\"."
        redirect '/'
      end

      flash[:message] = "No recovery key could be generated for user \"#{@user.email}\"."
      redirect '/'
    end
    
    flash[:message] = "Your email \"#{params[:email]}\" could not be found in our systems."
    redirect '/'
  end

  get '/:recover_key' do

  end
end
