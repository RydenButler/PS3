### S3
# constructs class
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

# create generic
PlayGame <- function(x) UseMethod('PlayGame')

# create method
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

# test non-numeric choice
NumericTest <- door('a')

# test non-integer choice
DecimalTest <- door(1.23)

# test > 3 choice
ValidDoorTest <- door(4)

# test working example
FunctionalTest <- door(1)
PlayGame(FunctionalTest)

# S4
library(stats4)

# create door class
setClass('door', slots = list(choice = 'numeric'))

# create test function
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

# create new generic for PlayGame
setGeneric('PlayGame', function(choice){
  standardGeneric('PlayGame')
})

# create method for door
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

