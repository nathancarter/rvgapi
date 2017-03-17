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
    getTelemetry = function () {
      id <- private$match$relationships$assets$data[[1]]$id
      url <- private$lookup( id )$attributes$URL
      if ( is.null( url ) ) return( NULL )
      VGTelemetry$new( self, fromJSON( url, simplifyDataFrame = FALSE ) )
    },
    cheater = function () private$match
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

VGTelemetry <- R6Class(
  'VGTelemetry',
  public = list(
    initialize = function ( match, data ) {
      private$match <- match
      private$data <- data
    },
    # total number of events
    getCount = function () length( private$data ),
    # get one event
    getEvent = function ( i ) private$data[[i]],
    # or get its attributes
    getTime = function ( i ) self$getEvent( i )$time,
    getType = function ( i ) self$getEvent( i )$type,
    getTeam = function ( i ) self$getEvent( i )$payload$Team,
    getActor = function ( i ) self$getEvent( i )$payload$Actor,
    getLevel = function ( i ) self$getEvent( i )$payload$Level,
    getLifetimeGold = function ( i ) self$getEvent( i )$payload$LifetimeGold,
    getItem = function ( i ) self$getEvent( i )$payload$Item,
    getCost = function ( i ) self$getEvent( i )$payload$Cost,
    getKilled = function ( i ) self$getEvent( i )$payload$Killed,
    getKilledTeam = function ( i ) self$getEvent( i )$payload$KilledTeam,
    getGold = function ( i ) self$getEvent( i )$payload$Gold,
    getIsHero = function ( i ) self$getEvent( i )$payload$IsHero,
    getTargetIsHero = function ( i ) self$getEvent( i )$payload$TargetIsHero,
    getPosition = function ( i ) self$getEvent( i )$payload$Position,
    # functions about the whole set of data
    getActors = function ( team ) {
      if ( team == 1 ) team <- 'Left'
      if ( team == 2 ) team <- 'Right'
      result <- c()
      for ( event in private$data ) {
        if ( event$payload$Team == team ) {
          if ( !( event$payload$Actor %in% result ) ) {
            result <- c( result, event$payload$Actor )
            if ( length( result ) == 3 ) break
          }
        }
      }
      result
    },
    getStartTime = function () self$getTime( 1 ),
    getEndTime = function () self$getTime( self$getCount() ),
    getDuration = function () difftime( self$getEndTime(), self$getStartTime(), units='secs' ),
    lastKnownPosition = function ( team, actor, i ) {
      if ( team == 1 ) team <- 'Left'
      if ( team == 2 ) team <- 'Right'
      while ( i >= 1 ) {
        e <- self$getEvent( i )
        if ( ( e$payload$Killed == actor ) && ( e$payload$KilledTeam == team ) )
          return( NULL )
        if ( ( e$payload$Team == team ) && ( e$payload$Actor == actor )
                                        && !is.null( e$payload$Position ) )
          return( e$payload$Position )
        i <- i - 1
      }
      return( NULL )
    }
  ),
  private = list(
    match = NULL,
    data = NULL
  )
)
