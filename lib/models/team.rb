class Team
  include DataMapper::Resource  
  property :id, Serial
  has n, :users, :through => Resource

  def find_finnished_games
    return FinnishedGame.all(:team1 => @id) | FinnishedGame.all(:team2 => @id)
  end
end
