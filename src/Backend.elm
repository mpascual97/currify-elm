port module Backend exposing (..)
import Types exposing(Song)

import Utils exposing (..)
import Models exposing (Model)
import List exposing (range, map, intersperse, filter, append)
import String exposing (concat, contains, toLower)

-- Existe la funcion findSong que recibe
-- una condicion y una lista de canciones
-- findSong : (Song -> Bool) -> List Song -> Song

-- Existe la funcion tailSafe que recibe
-- una lista de canciones y se queda con la cola
-- si la lista no tiene cola (tiene un solo elemento)
-- se queda con una lista vacia
-- tailSafe : List Song -> List Song

-- Existe idFirst que recibe una lista
-- de canciones y devuelve el id de la primera
-- idFirst : List Song -> String

-- Debería darnos la url de la cancion en base al id
urlById : String -> List Song -> String
urlById id songs = (findSong (.id >> (==) id) songs).url

-- Debería darnos las canciones que tengan ese texto en nombre o artista
filterByName : String -> List Song -> List Song
filterByName text songs = filter (songMatch text) songs

songMatch : String -> Song -> Bool
songMatch text song = (containsText text song.name) || (containsText text song.artist)

containsText : String -> String -> Bool
containsText text aString = contains (toLower(text)) (toLower(aString))
-- Recibe un id y tiene que likear/dislikear una cancion
-- switchear song.liked
toggleLike : String -> List Song -> List Song
toggleLike id songs = map (switchLiked id) songs

switchLiked : String -> Song -> Song
switchLiked id song = if song.id == id then { song | liked = not(song.liked)} else song

--mapLiked function song = { song | liked = song.liked}
-- Esta funcion tiene que decir si una cancion tiene
-- nuestro like o no, por ahora funciona mal...
-- hay que arreglarla
isLiked : Song  -> Bool
isLiked song = song.liked

-- Recibe una lista de canciones y nos quedamos solo con las que
-- tienen un like
filterLiked : List Song -> List Song
filterLiked songs = filter isLiked songs

-- Agrega una cancion a la cola de reproduccion
-- (NO es necesario preocuparse porque este una sola vez)
addSongToQueue : Song -> List Song -> List Song
addSongToQueue song queue = queue ++ [song]

-- Saca una cancion de la cola
-- (NO es necesario que se elimine una sola vez si esta repetida)
removeSongFromQueue : String -> List Song -> List Song
removeSongFromQueue id queue = filter (.id >> (/=) id) queue

-- Hace que se reproduzca la canción que sigue y la saca de la cola
playNextFromQueue : Model -> Model
playNextFromQueue model = {model | queue = tailSafe model.queue, playerUrl = urlById (idFirst model.queue) model.songs}


-------- Funciones Listas --------

-- Esta funcion recibe el modelo y empieza a reproducir la
-- cancion que tenga el id que se pasa...
-- Mirar la función urlById
playSong : Model -> String -> Model
playSong model id = { model | playerUrl = urlById id model.songs, playing = (if id /= "" then Just True else Nothing) }

applyFilters : Model -> List Song
applyFilters model =
  model.songs
    |> filterByName model.filterText
    |> if model.onlyLiked then filterLiked else identity

port togglePlay : Bool -> Cmd msg
port songEnded : (Bool -> msg) -> Sub msg