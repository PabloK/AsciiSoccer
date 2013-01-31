require 'bcrypt'

class User
  include DataMapper::Resource  
  include BCrypt
  property :id,             Serial
  property :name,           String
  property :email,          String, :required => true, :unique => true
  property :password_hash,  String, :required => true, :lazy => true
  property :lookup,         String, :lazy => false
  property :recover_key,    String, :lazy => false
  property :color,          Integer, :default => 1
  
  validates_format_of :email , :as => /^.*@.*\..*{3,}$/i, :message => "Email adress format must be valid."
  validates_length_of :email , :within => 5..250, :message => "Email needs to be between 5 and 250 characters."
  validates_length_of :name , :within => 3..10, :message => "A team name must have be between 3 and 10 characters long."

  def name
    if @name == '' or @name == nil
      return "User#{@id}"
    end
    return @name
  end

  def lookup_valid?(session_lookup)
    return session_lookup == @lookup
  end
  
  def generate_recover_key!
    update!(:recover_key => (0...64).map{ ('a'..'z').to_a[rand(26)]}.join)
  end

  def new_lookup!
    update!(:lookup => (0...50).map{ ('a'..'z').to_a[rand(26)]}.join)
  end

  def password
    @password ||= Password.new(password_hash)
  end

  def password=(new_password)
    @password = Password.create(new_password)
    self.password_hash = @password
  end

  def email= email
    super email.downcase
  end

end
