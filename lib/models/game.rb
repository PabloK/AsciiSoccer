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
    if game.port and game.save
      return game
    end
    
    return false
  end

  def self.get_available_game
    self.remove_old_games()
    Game.first(:conditions => ['number_of_players != maximum_players']) 
  end

  def self.remove_old_games
    old_games = Game.all(:conditions => ['number_of_players = maximum_players AND created_date < ?', (Time.now - 120)]) 
    old_games.destroy if old_games
  end
  
  def self.new_port
    total_ports = Array ($config[:first_port]..$config[:last_port])
    current_ports = Game.all(:fields => [:port]).map{|game| game.port}

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
    new_number = self.number_of_players + 1
    return self.update(:number_of_players => new_number, :created_date => Time.now )
  end

end
