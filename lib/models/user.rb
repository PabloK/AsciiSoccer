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
  
  validates_format_of :email , :as => /^.*@.*\..*{3,}$/i, :message => "Email adress format must be valid."
  validates_length_of :email , :within => 5..250, :message => "Email needs to be between 5 and 250 characters."
  validates_length_of :name , :within => 0..20, :message => "A team name must have be between 3 and 20 characters long."
  validates_format_of :color , :with => /^#[0-9ABCDEFabcdef]{6}$/ , :message => "Colors must be in hexadecimal format." , :if => lambda {|u| u.color != nil }

  
  def new_lookup
    (0...50).map{ ('a'..'z').to_a[rand(26)]}.join
  end

  def password
    @password ||= Password.new(password_hash)
  end

  def password=(new_password)
    #TODO validate password length
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
