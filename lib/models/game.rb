class Game
  include DataMapper::Resource

  property :port, Integer, :key => true, :unique => true, :required => true
  property :code, String,  :required => true 
  property :number_of_players, Integer, :required => true, :default => 0
  property :created_date, DateTime, :required => true
  property :maximum_players, Integer, :required => true, :default => 4
  property :waittime, Float, :required => true, :default => 0
  property :finnished, Boolean, :required => true, :default => false
  
  def self.generate_game!
    puts "Generating game"
    game = Game.new
    game.code = (0...64).map{ ('a'..'z').to_a[rand(26)]}.join
    game.created_date = DateTime.now
    game.port = new_port
    if game.port and game.save
      return game
    end
    
    return false
  end

  def self.get_available_game
    self.remove_old_games()
    Game.first(:conditions => ['number_of_players < maximum_players']) 
  end

  def self.remove_old_games
    old_games = Game.all(:conditions => ['created_date < ?', (Time.now - 210)]) 
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
    return false if self.number_of_players >= self.maximum_players
    new_number = self.number_of_players + 1
    updated_waittime = 0
    if new_number == self.maximum_players
      updated_waittime = (DateTime.now.to_time - self.created_date.to_time).to_f
    end
    return self.update(:number_of_players => new_number, :waittime => updated_waittime)
  end

  def self.current_games
    Game.count(:finnished => false)
  end

  def self.queued_games
    Game.count(:finnished => false, :conditions => ['number_of_players != maximum_players'])
  end

  def self.mean_wait
    mean = Game.avg(:waittime, :conditions => ['finnished = TRUE AND created_date > ?', 10.minutes.ago])
    return mean.round(1) if mean
    return nil
  end

  def full?
    return @maximum_players == @number_of_players
  end

end
