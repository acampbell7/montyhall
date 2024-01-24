#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' CONTESTANT SELECTS A DOOR
#' @description
#' select_door() function allows the contestant to select a door, number 1:3.
#' @details
#' The contestant will be selecting 1 our of 3 doors on the "game show."
#' @param ... no arguments are used by the function.
#' @return 
#' The function allows for one door to be selected, numbered 1:3
#' @examples
#' select_door()
#' @export
select_door <- function()
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' HOST OPENS GOAT DOOR
#' @description
#' The Host will want to open one door, not the contestant door selected. Out of the two doors left
#' the Host will have to choose a GOAT door to open. If the contestant selected the car door, then host
#' can open either other door. If the contestant selected a goat door, host has to open the other goat door.
#' @details
#' open_goat_door <- function( game, a.pick )
#' @param 
#' Arguments used that if else statement, depending on the door the contestant opened
#' @return 
#'  One door should be revealed, this should be the one with a goat behind it. 
#' @examples
#' return( opened.door ) 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' CHANGE DOORS
#' @description
#' change_door() will allow the contestant to switch doors or stay
#' @details
#' The contestant will be able to switch doors or stay with their first selection. If they stay, they will 
#' keep their original pick. If the contestant switches, they will chose the other door, the one the host
#' did not open.
#' @param 
#' if the contestant stays they will have their first pick, if changes will need to choose the opposing ! argument 
#' for their choice
#' @return 
#' Which door that the contestant chooses
#' @examples
#' return( final.pick )
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' DETERMINE IF CONTESTANT HAS WON
#' @description
#' return function will allow us to see if the contestant won the car or chose the goat door.
#' @details
#' If the contestant chooses the door that is linked with car, they will WIN. If the contestant get the door that 
#' links with the goat, they will LOSE
#' @param 
#' Depending on the value choosen, this will depend the if argument that will be selected 
#' @return 
#' the final pick the contestant chose and if its linked  with car, they will WIN or links with the goat, they will LOSE
#' @examples
#' return( "LOSE" ) or return( "WIN" )
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' PLAY GAME IN ORDER
#' @description
#'  play_game() function which executes each step of a single game in order
#' @details
#' Using the functions you created last week or those provided in the solutions, package them all 
#' together into a single play_game() function which executes each step of a single game in order.
#' @param  ... no arguments are used by the function.
#' @return 
#' To see if the contestant won or lost
#' @examples
#' return( game.results ) to see answer
#' @export
play_game <- function()
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Adding the Game to a Loop, Running Multiple Times
#' @description
#' Adding a for loop to run the game multiple time to get results
#' @details
#' When running simulations for inferential purposes the general rule of thumb is they should run at
#'  least 10,000 times in order for the simulated statistics to converge 
#' close to the actual theoretial value (average proportion of wins achieved by each strategy in this case).
#' In this case we will be running 100 times, n=100. 
#' @param ... no arguments are used by the function.
#' @return 
#' We will want a statistic outcome layout of strategies of staying versus switching, 
#' In this we will want to see if we win or lose
#' @examples
#'   return( results.df )
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
