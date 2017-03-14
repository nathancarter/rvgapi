# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(curl)
library(jsonlite)
library(httpuv)
library(R6)

VGAPI <- R6Class(
  'VGAPI',
  public = list(
    initialize = function ( APIKey ) {
      private$APIKey = APIKey
      private$handleOptions <- list()
    },
    setRegion = function ( region ) private$region <- region,
    setStartTime = function ( startTime ) {
      if ( is.character( startTime ) ) {
        result <- as.POSIXct( startTime, format='%Y-%m-%dT%H:%M:%SZ' )
        if ( !is.na( result ) ) {
          attr( result, 'tzone' ) <- 'UTC'
          startTime <- result
        }
      }
      private$startTime <- startTime
    },
    setEndTime = function ( endTime ) {
      if ( is.character( endTime ) ) {
        result <- as.POSIXct( endTime, format='%Y-%m-%dT%H:%M:%SZ' )
        if ( !is.na( result ) ) {
          attr( result, 'tzone' ) <- 'UTC'
          endTime <- result
        }
      }
      private$endTime <- endTime
    },
    setPlayerNames = function ( playerNames ) private$playerNames <- playerNames,
    setTeamNames = function ( teamNames ) private$teamNames <- teamNames,
    matchQuery = function ( pageOffset = 0 ) {
      query <- paste0( 'matches?', encodeURIComponent( 'page[offset]' ), '=', pageOffset )
      if ( !is.null( private$startTime ) )
        query <- paste0( query, '&', encodeURIComponent( 'filter[createdAt-start]' ), '=',
                         encodeURIComponent( strftime( private$startTime, format='%Y-%m-%dT%H:%M:%SZ', tz='UTC' ) ) )
      if ( !is.null( private$endTime ) )
        query <- paste0( query, '&', encodeURIComponent( 'filter[createdAt-end]' ), '=',
                         encodeURIComponent( strftime( private$endTime, format='%Y-%m-%dT%H:%M:%SZ', tz='UTC' ) ) )
      if ( !is.null( private$playerNames ) )
        query <- paste0( query, '&', encodeURIComponent( 'filter[playerNames]' ), '=',
                         paste( private$playerNames, collapse=',' ) )
      if ( !is.null( private$teamNames ) )
        query <- paste0( query, '&', encodeURIComponent( 'filter[teamNames]' ), '=',
                         paste( private$teamNames, collapse=',' ) )
      query
    },
    fetchMatches = function ( pageOffset = 0 )
      private$fetchedMatches <- private$fetchData( self$matchQuery( pageOffset ) ),
    getError = function () private$error,
    count = function () length( private$fetchedMatches$data ),
    getMatch = function ( index )
      VGMatch$new( private$fetchedMatches$data[[index]], self ),
    lookup = function ( id ) {
      print( id )
      if ( is.null( id ) ) return( NULL )
      for ( included in private$fetchedMatches$included )
        if ( included$id == id ) return( included )
      return( NA )
    }
  ),
  private = list(
    APIKey = NULL, # must be provided at construction time
    rootUrl = 'https://api.dc01.gamelockerapp.com/',
    region = 'na',
    startTime = NULL, # for filtering
    endTime = NULL, # for filtering
    playerNames = NULL, # for filtering
    teamNames = NULL, # for filtering
    handleOptions = NULL,
    fetchedMatches = NULL,
    error = NULL,
    getHandle = function () {
      handle <- new_handle()
      handle_setheaders(
        handle,
        'Authorization' = private$APIKey,
        'X-TITLE-ID' = 'semc-vainglory',
        'Accept' = 'application/vnd.api+json',
        .list = private$handleOptions
      )
      handle
    },
    endPoint = function ( path )
      paste0( private$rootUrl, 'shards/', private$region, '/', path ),
    fetchData = function ( path ) {
      result <- curl_fetch_memory( private$endPoint( path ), handle = private$getHandle() )
      if ( result$status_code == 200 ) {
        private$error <- NULL
        return( fromJSON( rawToChar( result$content ), simplifyDataFrame = FALSE ) )
      } else {
        private$error <- result
        return( NULL )
      }
    }
  )
)

VGMatch <- R6Class(
  'VGMatch',
  public = list(
    initialize = function ( match, apiObject ) {
      private$match <- match
      private$api <- apiObject
    },
    getTime = function () {
      result <- as.POSIXct( private$match$attributes$createdAt, format='%Y-%m-%dT%H:%M:%SZ' )
      attr( result, 'tzone' ) <- 'UTC'
      result
    },
    getDuration = function () private$match$attributes$duration,
    getGameMode = function () private$match$attributes$gameMode,
    getRegion = function () private$match$attributes$shardId,
    getEndGameReason = function () private$match$attributes$stats$endGameReason,
    getRoster = function ( index ) {
      VGRoster$new(
        private$lookup( private$match$relationships$rosters$data[[index]]$id ),
        private$api )
    },
    cheater = function () private$fetchedMatches
  ),
  private = list(
    match = NULL,
    api = NULL,
    lookup = function ( id ) private$api$lookup( id )
  )
)

VGRoster <- R6Class(
  'VGRoster',
  public = list(
    initialize = function ( roster, apiObject ) {
      private$roster <- roster
      private$api <- apiObject
    },
    getStats = function () private$roster$attributes$stats,
    getMembers = function ()
      lapply( private$roster$relationships$participants$data,
              function ( participant )
                VGPlayer$new( private$lookup( participant$id ), private$api ) ),
    getTeamName = function () {
      if ( !is.null( private$roster$relationships$team ) )
        return( private$lookup( private$roster$relationships$team$data$id )$name )
      else
        return( NA )
    },
    cheater = function () private$roster
  ),
  private = list(
    roster = NULL,
    api = NULL,
    lookup = function ( id ) private$api$lookup( id )
  )
)

VGPlayer <- R6Class(
  'VGPlayer',
  public = list(
    initialize = function ( member, apiObject ) {
      private$teamMember <- member
      private$api <- apiObject
      private$player <- private$lookup( member$relationships$player$data$id )
    },
    getName = function () private$player$attributes$name,
    getStats = function () private$player$attributes$stats,
    getHeroName = function () private$teamMember$attributes$actor,
    getHeroStats = function () private$teamMember$attributes$stats
  ),
  private = list(
    teamMember = NULL,
    api = NULL,
    player = NULL,
    lookup = function ( id ) private$api$lookup( id )
  )
)
