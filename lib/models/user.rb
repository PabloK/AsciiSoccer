require 'bcrypt'

class User
  include DataMapper::Resource  
  include BCrypt

  property :id,       Serial
  property :name,     String
  property :email,    String, :required => true, :unique => true
  property :password_hash, String, :required => true, :lazy => true
  property :lookup, String, :lazy => false
  property :color,    String, :length => 7, :default => "#903c3b"
  
  def new_lookup
    (0...50).map{ ('a'..'z').to_a[rand(26)] }.join
  end

  def password
    @password ||= Password.new(password_hash)
  end

  def password=(new_password)
    @password = Password.create(new_password)
    self.password_hash = @password
  end

  def is_valid_session key
    return self.lookup.session_key == key
  end

  def email= email
    super email.downcase
  end

  def name= new_name
    super new_name.downcase
  end

end
