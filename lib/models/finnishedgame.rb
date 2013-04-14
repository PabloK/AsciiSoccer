class FinnishedGame
  include DataMapper::Resource  
  property :id, Serial
  property :team1_score,  Integer, :required => true
  property :team2_score,  Integer, :required => true
  property :play_date, DateTime, :default => DateTime.now
  belongs_to :team1, 'Team'
  belongs_to :team2, 'Team'
end
