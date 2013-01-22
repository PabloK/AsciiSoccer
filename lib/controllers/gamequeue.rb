class GameQueueController < Sinatra::Base
  get '/' do

    game = Game.get_available_game
    game = Game.generate_game! if game == nil

    if game
      session[:code] = game.code
      redirect "/game/#{game.port}" 
    end
   
    #TODO queue players 
    popup("No free ports to start new game on!")
  end
end
