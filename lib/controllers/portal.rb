require 'haml'

class Portal < Sinatra::Base
    get '/' do
      haml :index
    end
end
