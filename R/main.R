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
    initialize = function ( bearer = 'aaa.bbb.ccc' ) {
      private$handleOptions <- list()
    },
    setRegion = function ( region ) private$region <- region,
    fetchMatches = function ( pageOffset = 0 ) {
      private$fetchedMatches <- private$fetchData(
        paste0( 'matches?', encodeURIComponent( 'page[offset]' ), '=', pageOffset ) )
    },
    count = function () length( private$fetchedMatches$data ),
    getMatch = function ( index )
      VGMatch$new( private$fetchedMatches$data[[index]], self ),
    lookup = function ( id ) {
      for ( included in private$fetchedMatches$included )
        if ( included$id == id ) return( included )
      return( NA )
    }
  ),
  private = list(
    rootUrl = 'https://api.dc01.gamelockerapp.com/',
    region = 'na',
    handleOptions = NULL,
    fetchedMatches = NULL,
    getHandle = function () {
      handle <- new_handle()
      handle_setheaders(
        handle,
        'Authorization' = 'Bearer aaa.bbb.ccc',
        'X-TITLE-ID' = 'semc-vainglory',
        'Accept' = 'application/vnd.api+json',
        .list = private$handleOptions
      )
      handle
    },
    endPoint = function ( path )
      paste0( private$rootUrl, 'shards/', private$region, '/', path ),
    fetchData = function ( path )
      fromJSON( rawToChar( curl_fetch_memory(
        private$endPoint( path ), handle = private$getHandle() )$content ),
        simplifyDataFrame = FALSE )
  )
)

VGMatch <- R6Class(
  'VGMatch',
  public = list(
    initialize = function ( match, apiObject ) {
      private$match <- match
      private$api <- apiObject
    },
    getTime = function ()
      strptime( private$match$attributes$createdAt, '%Y-%m-%dT%H:%M:%SZ' ),
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
