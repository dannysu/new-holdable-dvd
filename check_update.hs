{-# LANGUAGE DeriveGeneric #-}

import Network.HTTP.Conduit
import Network.HTTP.Base (urlEncodeVars)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Regex.TDFA ((=~))
import Control.Monad (mapM)

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import GHC.Generics (Generic)

newMoviesURL = "http://www.torontopubliclibrary.ca/books-video-music/video/new-holdable-adult.jsp"
libraryURLPrefix = C.pack "http://www.torontopubliclibrary.ca"
rottenTomatoesAPIPrefix = "http://api.rottentomatoes.com/api/public/v1.0/movies.json?page_limit=1&"

rottenTomatoesAPIKey = ""

type MovieName = L.ByteString
type LibraryURL = L.ByteString
type Cast = L.ByteString
type Rating = Int

data MovieInfo = Movie {
      movieName :: MovieName
    , libraryURL :: LibraryURL
    , casts :: [Cast]
    , criticRating :: Rating
    , peopleRating :: Rating
    } deriving (Show)

data RTRatings = RTRatings {
      critics_rating :: L.ByteString
    , critics_score :: Rating
    , audience_rating :: L.ByteString
    , audience_score :: Rating
    } deriving (Show, Generic)

data RTCast = RTCast {
      name :: L.ByteString
    } deriving (Show, Generic)

data RTMovie = RTMovie {
      year :: Int
    , ratings :: RTRatings
    , abridged_cast :: [RTCast]
    } deriving (Show, Generic)

data RTInfo = RTInfo {
      movies :: [RTMovie]
    } deriving (Show, Generic)

instance FromJSON RTInfo
instance FromJSON RTMovie
instance FromJSON RTCast
instance FromJSON RTRatings

updated :: L.ByteString -> L.ByteString
updated s = if length matches > 0
            then last $ head matches
            else L.empty
              where matches = s =~ "<h3[^>]*>Updated (.*)</h3>"

movieTuple :: [L.ByteString] -> MovieInfo
movieTuple ms = Movie name url [] 0 0
                  where name = last ms
                        url = L.append libraryURLPrefix (head (tail ms))

htmlToMovies :: L.ByteString -> [MovieInfo]
htmlToMovies s = map movieTuple matches
             where matches = s =~ "<a +href=\"(/detail.jsp[^\"]*)\">([^<]*)</a>"

getCastName :: [L.ByteString] -> Cast
getCastName ms = L.append firstName $ L.append (C.pack " ") lastName
                   where lastName = head $ tail ms
                         firstName = head $ drop 1 $ tail ms

getCasts :: MovieInfo -> L.ByteString -> [Cast]
getCasts m s = map getCastName matches
              where name = movieName m
                    url = libraryURL m
                    matches = s =~ "<a +href=\"/search\\.jsp\\?N=[0-9]+\">([a-zA-Z ]+), ([a-zA-Z ]+).*</a>"

getMovieYear :: L.ByteString -> L.ByteString
getMovieYear s = head $ head $ tail matches
                   where matches = s =~ "Year/Format: [^0-9]*([0-9]+), "

getMovieDetail :: MovieInfo -> IO MovieInfo
getMovieDetail m = do
    content <- simpleHttp $ C.unpack $ libraryURL m
    return $ Movie name url (getCasts m content) 0 0
      where name = movieName m
            url = libraryURL m

getMoviesDetail :: [MovieInfo] -> IO [MovieInfo]
getMoviesDetail ms = mapM getMovieDetail ms

castMatches :: MovieInfo -> RTMovie -> Bool
castMatches m rt = foldl f False ps
    where people = casts m
          ps = abridged_cast rt
          f True b = True
          f False b = any (\x -> x == (name b)) people

findMovieMatch :: MovieInfo -> Maybe RTInfo -> Maybe RTMovie
findMovieMatch m Nothing = Nothing
findMovieMatch m (Just rt) = foldl f Nothing potential_movies
    where potential_movies = movies rt
          f Nothing b = if castMatches m b
                        then Just b
                        else Nothing
          f z b = z

getRottenTomatoesRating :: MovieInfo -> IO MovieInfo
getRottenTomatoesRating m = do
    content <- simpleHttp ratingURL
    let rt = decode content :: Maybe RTInfo
    let result = findMovieMatch m rt
    case result of
        Nothing    -> return m
        Just value -> return $ Movie name url people c_score a_score
                        where rs = ratings value
                              c_score = critics_score rs
                              a_score = audience_score rs
      where name = movieName m
            url = libraryURL m
            people = casts m
            args = urlEncodeVars [("apikey", rottenTomatoesAPIKey), ("q", C.unpack $ name)]
            ratingURL = rottenTomatoesAPIPrefix ++ args

getMoviesRating :: [MovieInfo] -> IO [MovieInfo]
getMoviesRating ms = mapM getRottenTomatoesRating ms

main :: IO ()
main = do
    content <- simpleHttp newMoviesURL
    let movies = htmlToMovies content
    more <- getMoviesDetail movies
    moviesRatings <- getMoviesRating more
    print moviesRatings
