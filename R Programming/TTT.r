#building essential functions
'%!in%' <- function(x,y)!('%in%'(x,y))                                                                        #Not in Operator
user_input <- function(prom){
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  cat(prom)
  symbol <- readLines(con = con, n = 1)
}
print_board<- function(){
  printable_board <- replace(board,(board %!in% players), '-')
  print(printable_board)
}
board <- matrix(nrow=3,ncol=3,data=as.character(c(1:9)),byrow = TRUE,dimnames=list(c(1:3),c(" 1"," 2"," 3"))) #Creating the main board. 
players <- c('X','O')                                                                                         #Creating a vector containing two players
choices <- c("1","2","3")                                                                                     #Valid inputs
round <- 0                                                                                                    #Initializing Round
win_combo <- list(                                                                                            #A list of all the possible winning combinations
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7)
)
hum_pos <- c()                                                                                               #A list to keep record of where the "X" player has placed a move. 
comp_pos<- c()                                                                                               #A list to keep record of where the "O" player has placed a move.
game<-TRUE
cat('\n',"Welcome to Tic Tac Toe in R!",'\n', "You're playing X. Enter which column and row where you want to make a move. Type 'Quit' anytime to exit the game.",'\n')
Sys.sleep(2)
while(game==TRUE){
  round<-round+1 
  if (round == 6){
    cat("It's a draw!")
    quit()
  }                                                          #Round 6 is always a draw
  cat('\n')
  cat("##### Round: ")
  cat(round,' #####', '\n','\n')
  print_board()
  cat('\n')
  user_col <- NA                                                                #User inputs stored in a variable.
  user_row <- NA
  while (is.na(user_col)){                                                      #Ask for column
    user_col <- user_input("Which Column? ")
    if (tolower(user_col)=="quit"){
      cat('Goodbye!')
      quit()
    }
    if (is.na(user_col)||user_col %!in% choices) {                              #Prompt again if the input isn't contained in valid choices. 
      cat("Invalid Input. Please specify a column from 1 to 3. ")
      user_col <- NA
    }
    user_col<-as.integer(user_col)
  }
  while (is.na(user_row)){                                                      #Ask for row
    user_row <- user_input("Which Row? ")
    if (tolower(user_row)=='quit'){
      cat('Goodbye!')
      quit()
    }
    if (is.na(user_row)||user_row %!in% choices) {
      cat("Invalid Input. Please specify a row from 1 to 3. ")
      user_row <- NA
    }
    user_row<-as.integer(user_row)
  }
  if (board[user_row,user_col]=='X'||board[user_row,user_col]=='O'){            #Check if space is available
    cat("Space is already occupied. Please try again.","\n")
    user_col <- NA
    user_row <- NA
    round <- round-1
    next
  }
  hum_pos <- append(hum_pos,as.integer(board[user_row,user_col]))               #Add X on the board and record it in hum_pos variable.
  board[user_row,user_col]<- "X"
  for (i in c(1:8)){                                                            #Check if player has won
    has_won <- setdiff(win_combo[[i]], hum_pos)
    if (sum(has_won) == 0){
      print_board()
      cat("Congratulations! You have won the game!")
      Sys.sleep(3)
      quit()
    }
  }
  cat("Computer's turn...","\n")                                                #Computer's turn
  Sys.sleep(1)
  comp_turn <- FALSE
  if (board[2,2]=="5"){                                                         #For round 1, The computer will always try to go for the middle position, as it has a winning rate of 75%
    comp_pos<-append(comp_pos,5)
    board[2,2]<- "O"
  } else if (round == 1){                                                       #Go to a random position if the middle is occupied. 
    while (comp_turn==FALSE){
      comp_place <- cbind(sample(1:3, 1),sample(1:3, 1))
      if (board[comp_place] != "X"){
        comp_pos<-append(comp_pos,as.integer(board[comp_place]))
        board[comp_place]<-"O"
        comp_turn <- TRUE
      }
    }
  } else {                                                                      #Check if the computer can win and then play the winning move.
    for (i in c(1:8)){
      check_win <- setdiff(win_combo[[i]], comp_pos)
      if (length(check_win)==1){
        bc<-which(board == check_win,arr.ind=TRUE)
        if (length(board[bc])!=0&&board[bc]!='X'){
          board[bc]<-'O'
          print_board()
          cat('You Lost! Better luck next time')
          Sys.sleep(3)
          quit()
        }
      }
    }
    possibilities <- c()                                                        #stop the player from winning.
    comp_played <- FALSE
    for (i in c(1:8)){
      check_move <- setdiff(win_combo[[i]], hum_pos)                            #Compare the player's current positions with the winning combos. Store the difference in a vector. 
      possibilities[[length(possibilities)+1]] <- check_move
    }
    possibilities <- possibilities[order(sapply(possibilities, length))]
    pos_len <- length(possibilities)
    for (p in c(1:pos_len)){
      suppressWarnings(ac <- which(board == possibilities[[p]],arr.ind=TRUE))
      if (length(board[ac])== 1){                                               #If the player is one move away from winning, place the next move there. 
        comp_pos<-append(comp_pos,as.integer(board[ac]))
        comp_played <- TRUE
        board[ac]<-'O'
        break
      } 
      if (p == pos_len && comp_played==FALSE){                                  #If the player isn't trying to win, make a move on the corners. 
        for (y in c(c(1,1),c(1,3),c(3,1),c(3,3))){
          if (board[y] != 'X' && board[y] != 'O'){
            comp_pos<-append(comp_pos,as.integer(board[y]))
            board[y] <- 'O'
            break
          }
        }
      }
    }
  }
}