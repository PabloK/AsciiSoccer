class Game
  include DataMapper::Resource

  property :port, Integer, :key => true, :unique => true, :required => true
  property :code, String,  :required => true 
  property :created_date, DateTime, :required => true

end
