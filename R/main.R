# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(curl)
library(jsonlite)
library(httpuv)

apiKey <- 'getoffmylawn'
setApiKey <- function ( key ) {
  apiKey <<- key
}
getHandle <- function ( otherOptions = list() ) {
  handle <- new_handle()
  handle_setheaders(
    handle,
    'X-API-KEY' = 'getoffmylawn',
    'X-TITLE-ID' = 'semc-vainglory',
    'Accept' = 'application/vnd.api+json',
    .list = otherOptions )
  handle
}

rootUrl <- 'https://api.dc01.gamelockerapp.com/'
endPoint <- function ( path ) paste0( rootUrl, path )
fetchData <- function ( path, handleOptions = list() )
  fromJSON( rawToChar( curl_fetch_memory(
    endPoint( path ), handle = getHandle( handleOptions ) )$content ),
    simplifyDataFrame = FALSE )

#' Fetching JSON data for many matches
#'
#' Fetches one page of matches from all available matches.
#' Each match is a very large data structure.
#' See the documentation for \code{getMatch()} for details.
#'
#' @param pageOffset defaults to 0, can be used to page through the huge list of matches
#'
#' @return A large data table converted from the JSON response with jsonlite
#'
#' @examples
#' getMatches()
#'
#' @export
getMatches <- function ( pageOffset = 0 ) {
  jsonapi <- fetchData( paste0( 'matches?', encodeURIComponent( 'page[offset]' ), '=', pageOffset ) )
  jsonapi
}

#' Fetching JSON data for one match
#'
#' Fetches the data for one match, given the ID for that match.
#' This will be a large and complex data structure, described below.
#'
#' @param id the unique ID for the match, such as \code{'00cbe304-d7d7-11e6-ad79-062445d3d668'}
#'
#' @return A large data table converted from the JSON response with jsonlite.
#'     \code{result$data$type} will be the type of object, usually \code{"match"}.
#'     \code{result$data$id} will be the unique ID of the match, such as
#'     \code{'00cbe304-d7d7-11e6-ad79-062445d3d668'}.
#'     \code{result$data$attributes} is a list with various attributes of the match, including its
#'     time, length, game mode, region, and how it ended.
#'     \code{result$data$relationships} will be a list of two team rosters, each of which is just an
#'     ID, of the same format as the match ID.
#'     \code{result$included} is a table whose rows are players, participants, and rosters,
#'     in no particular order.  The \code{type} column tells which of the three types the row holds.
#'     The \code{id} column holds the unique ID for the object in that row.
#'     The \code{attributes} column holds another data frame; its \code{$actor} column lists the
#'     names of the heroes for each row that is a player, its \code{$stats} column has copious data
#'     not documented here about activity throughout the match, and its \code{$name} column has the
#'     in-game names for each row that is a player.
#'
#' @examples
#' getMatch( '00cbe304-d7d7-11e6-ad79-062445d3d668' )
#'
#' @export
getMatch <- function ( id ) fetchData( paste0( 'matches/', id ) )

#' Fetching JSON data for many players
#'
#' This does not yet work; the API doesn't support it.  This stub is here for now.
#'
#' @return probably just a 404 error at this point
#'
#' @examples
#' getPlayers()
#'
#' @export
getPlayers <- function () fetchData( 'players' )

#' Fetching JSON data for one player
#'
#' This does not yet work; the API doesn't support it.  This stub is here for now.
#'
#' @param id the unique ID for the player, such as \code{'83fc8814-b40a-11e6-8acb-06d90c28bf1a'}
#'
#' @return probably just a 404 error at this point
#'
#' @examples
#' getPlayer( '83fc8814-b40a-11e6-8acb-06d90c28bf1a' )
#'
#' @export
getPlayer <- function ( id ) fetchData( paste0( 'players/', id ) )

#' Fetching JSON data for many teams
#'
#' This does not yet work; the API doesn't support it.  This stub is here for now.
#'
#' @return probably just a 404 error at this point
#'
#' @examples
#' getTeams()
#'
#' @export
getTeams <- function () fetchData( 'teams' )

#' Fetching JSON data for one team
#'
#' This does not yet work; the API doesn't support it.  This stub is here for now.
#'
#' @param id the unique ID for the player, such as \code{'080a11a6-cfdd-11e6-90c5-0606daf7e67d'}
#'
#' @return probably just a 404 error at this point
#'
#' @examples
#' getTeam( '080a11a6-cfdd-11e6-90c5-0606daf7e67d' )
#'
#' @export
getTeam <- function ( id ) fetchData( paste0( 'teams/', id ) )
