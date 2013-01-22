class Game
  include DataMapper::Resource

  property :port, Integer, :key => true, :unique => true, :required => true
  property :code, String,  :required => true 
  property :number_of_players, Integer, :required => true, :default => 0
  property :created_date, DateTime, :required => true
  property :maximum_players, Integer, :required => true, :default => 2

  def self.generate_game!
    game = Game.new
    game.code = (0...64).map{ ('a'..'z').to_a[rand(26)]}.join
    game.created_date = Time.now
    game.port = new_port
    if game.port
      return game.save
    end
    
    return false
  end

  def self.get_available_game
    #TODO remove old games and loop
    Game.first(:conditions => ['number_of_players != maximum_players']) 
  end
  
  def self.new_port
    total_ports = Array ($config[:first_port]..$config[:last_port])
    current_ports = Game.all(:fields => [:port])

    if current_ports == nil
      return total_ports.first
    end

    available_ports = total_ports - current_ports 
      
    unless available_ports.empty?
      return available_ports.first 
    end
      
    return false
  end

  def join!
    return update(:number_of_players => (self.number_of_players +1) )
  end

end
