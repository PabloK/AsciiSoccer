DataMapper::Property::String.length(255)
Dir["./lib/models/*.rb"].each { |file| require file }
DataMapper.finalize
