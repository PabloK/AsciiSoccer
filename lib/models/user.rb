class User
  include DataMapper::Resource  

  property :id,       Serial
  property :name,     String
  property :email,    String, :required => true, :unique => true
  property :password, String, :required => true
  property :color,    String, :length => 7, :default => "#903c3b"

end
