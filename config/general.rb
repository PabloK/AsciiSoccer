#encoding: utf-8

$config = {}
$config[:first_port] = 4000
$config[:last_port] = 4100

colorfile = File.new(File.dirname(__FILE__) + "/../assets/css/color.sass","r")
while line = colorfile.gets 
  line = line.delete(" $\n")
  name,value = line.split(":")
  $config[name.to_sym] = value
end

class Sinatra::Base
  set :views, File.dirname(__FILE__) + "/../views"
  register Sinatra::Flash

    options = {      
    :from => "noreply@asciisoccer.herokuapps.com",
    :via => :smtp,
    :via_options => {
        :user_name => ENV['SENDGRID_USERNAME'],
        :password => ENV['SENDGRID_PASSWORD'],
        :domain => "localhost",
        :address => "smtp.sendgrid.net",
        :port => 587,
        :authentication => :plain,
        :enable_starttls_auto => true
      }
    }
  Pony.options = options
end
