#' Construct S3 door Class
#'
#' Allows the user to pick a door number and creates an S3 class holding that
#' door choice.
#'
#' @param choice A numeric scalar whole number from 1 to 3
#'
#' @return A door object containing the user's choice
#'
#' @export
#' 

door <- function(choice) {
  # stops function if choice is not numeric
  if (!is.numeric(choice)) stop('Choice must be a numeric!') 
  # stops function if choice is not integer less than 4
  if (choice %% 1 != 0 | choice > 3) stop('Choice must be 1, 2, or 3!')
  # stops function if non-scalar choice
  if (length(choice) > 1) stop('You can only choose one door!')
  # creates class w/ specified choice
  structure(list('choice' = choice), class = "door")
}

#' Creates S3 Generic Function for PlayGame
#'
#' Creates a PlayGame generic
#'
#' @param x An input corresponding to the proper method
#'
#' @return See specific method for return
#'
#' @export
#' 

PlayGame <- function(x) UseMethod('PlayGame')

#' Creates door method for PlayGame Generic
#'
#' Allows the user to play Let's Make a Deal
#'
#' @param door An S3 object of class door, containing a valid door choice.
#'
#' @return No object returned. Function prints outcome of game in console.
#'
#' @export
#' 

PlayGame.door <- function(door){
  # stops function if class is not door
  if(class(door) != 'door') stop('You must input an object of class door!')
  # assigns choice
  choice <- door$choice
  # assigns random door
  car <- sample(1:3, 1)
  # if choice is same as door, print win message, else print lose message
  ifelse(choice == car, 
         print('Congratulations! You win a new car!'),
         print('You chose the wrong door. Just a goat. Sad!'))
  # returns invisible
  invisible()
}

#### Conduct a series of test for the S3 version of Let's Make a Deal

# test non-numeric choice
NumericTest <- door('a')

# test non-integer choice
DecimalTest <- door(1.23)

# test > 3 choice
ValidDoorTest <- door(4)

# test working example
FunctionalTest <- door(1)
PlayGame(FunctionalTest)


#' Construct S4 door Class
#'
#' Creates an S4 class named door. Door objects can be created with new()
#'
#' @export

setClass('door', slots = list(choice = 'numeric'))

#' Tests S4 door object for valid structure and input
#'
#' Tests whether an object is (1) an S4 object, (2) is of class door,
#' (3) is a single door choice, (4) is an integer less than 4. 
#'
#' @param door Any object. The function will only return if door meets the 
#' above criteria.
#'
#' @return None. Prints a message to the console indicating whether the door 
#' object is valid for play. 
#'
#' @export

TestDoor <- function(door){
  # stops function if door is not S4
  if(!isS4(door)) stop('Your door is not an S4 object!')
  # stops function if class is not door
  if(is(door) != 'door') stop('Your S4 object is not a door!')
  # stops function is non-scalar choice
  if (length(door@choice) > 1) stop('You can only choose one door!')
  # stops function if choice is invalid number
  if(door@choice > 3 | door@choice %% 1 != 0) 
    stop('Your door choice must be 1, 2, or 3!')
  # confirms valid door choice
  else ('That door is a fair pick. Good luck!')
}

#' Creates S4 Generic Function for PlayGame
#'
#' Creates a PlayGame generic
#'
#' @param choice An input corresponding to the proper method
#'
#' @return See specific method for return
#'
#' @export
#' 

setGeneric('PlayGame', function(choice){
  standardGeneric('PlayGame')
})

#' Creates door method for PlayGame Generic
#'
#' Allows the user to play Let's Make a Deal with S4 door object.
#'
#' @param door An S4 object of class door, containing a valid door choice.
#'
#' @return No object returned. Function prints outcome of game in console.
#'
#' @export
#' 

setMethod('PlayGame',
          c(choice = 'door'),
          function(choice){
            # assigns choice
            choice <- choice@choice
            # assigns random door
            car <- sample(1:3, 1)
            # if choice is same as door, print win message, else print lose message
            ifelse(choice == car, 
                   print('Congratulations! You win a new car!'),
                   print('You chose the wrong door. Just a goat. Sad!'))
            # returns invisible
            invisible()
          })

#### Conduct a series of test for the S3 version of Let's Make a Deal

# test non-S4 object
S4Test <- 1
TestDoor(S4Test)

# create non-door S4 class
setClass('notdoor', slots = list(choice = 'numeric'))

# test non-door S4 object
ClassTest <- new('notdoor', choice = 1)
TestDoor(ClassTest)

# test non-numeric choice
NumericTest <- new('door', 'a') # this already throws an error

# test > 3 choice
ValidDoorTest <- new('door', choice = 4)
TestDoor(ValidDoorTest)

# test decimal choice
DecimalTest <- new('door', choice = 1.23)
TestDoor(DecimalTest)

# test working example
FunctionalTest <- new('door', choice = 1)
TestDoor(FunctionalTest)
PlayGame(FunctionalTest)

