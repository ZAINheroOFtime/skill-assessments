# Loading packages
import pandas as pd
import numpy as np
import sys
import random
import time

# Building board
board = pd.DataFrame(
    [["-", "-", "-"], ["-", "-", "-"], ["-", "-", "-"]], columns=[1, 2, 3]
)
board.index = np.arange(1, len(board) + 1)

# Define winning combos
win_combos = [
    [(0, 0), (0, 1), (0, 2)],
    [(1, 0), (1, 1), (1, 2)],
    [(2, 0), (2, 1), (2, 2)],
    [(0, 0), (1, 0), (2, 0)],
    [(0, 1), (1, 1), (2, 1)],
    [(0, 2), (1, 2), (2, 2)],
    [(0, 0), (1, 1), (2, 2)],
    [(2, 0), (1, 1), (0, 2)],
]

# A list to keep record of player and computer moves
player_lst = []
comp_lst = []


def select_player():
    print("Welcome to Tic Tac Toe in Python!")
    while True:
        player = input("Please select X or O: ").upper()
        if player == "O" or player == "X":
            comp = "O" if player == "X" else "X"
            print(
                f"You're playing {player}. Enter which column and row where you want to make a move. Type 'Quit' anytime to exit the game."
            )
            return player, comp
        else:
            print("Invalid input. Please select either X or O.")
            continue


def userinput():
    valid_input = ["1", "2", "3"]
    while True:
        while True:
            p_col = input("Which Column? ")
            if p_col.lower() == "quit":
                print("Goodbye!")
                sys.exit()
            elif p_col not in valid_input:
                print("Invalid input. Please specify a number ranging from 1 to 3.")
                continue
            else:
                p_col = int(p_col) - 1
                break
        while True:
            p_row = input("Which Row? ")
            if p_row.lower() == "quit":
                print("Goodbye!")
                sys.exit()
            elif p_row not in valid_input:
                print("Invalid input. Please specify a number ranging from 1 to 3.")
                continue
            else:
                p_row = int(p_row) - 1
                break
        if board.iloc[p_row, p_col] != "-":
            print("Space already occupied.")
            continue
        return p_row, p_col


def user_move(player, p_row, p_col):
    board.iloc[p_row, p_col] = player
    player_lst.append(
        (p_row, p_col),
    )


def comp_move(comp, i):
    global comp_lst

    print("Computer's turn...")
    print("\n")
    time.sleep(1.0)

    if (
        i == 0 and board.iloc[1, 1] == "-"
    ):  # Computer will always place the first move in the center if not already occupied.
        board.iloc[1, 1] = comp
        comp_lst.append(
            (1, 1),
        )
        return
    elif i == 0:  # If the center is occupied, place at random.
        c_col, c_row = random.randrange(0, 2), 2
        board.iloc[c_row, c_col] = comp
        comp_lst.append(
            (c_row, c_col),
        )
        return

    for combo in win_combos:  # Check if computer can win, then play the winning move.
        if len(set(combo) & set(comp_lst)) == 2:
            move = list(set(combo) - set(comp_lst))
            if board.iloc[move[0]] == "-":
                board.iloc[move[0]] = comp
                print("\n")
                print(board)
                print("\n")
                print("You Lost! Better luck next time.")
                time.sleep(2)
                sys.exit()

    for (
        combo
    ) in (
        win_combos
    ):  # Check if the player is one move away from winning, then stop the player.
        if len(set(combo) & set(player_lst)) == 2:
            move = list(set(combo) - set(player_lst))
            if board.iloc[move[0]] == "-":
                board.iloc[move[0]] = comp
                comp_lst.append(
                    (move[0]),
                )
                return

    for (
        combo
    ) in win_combos:  # If neither of the conditions apply, take step towards winning.
        if len(set(combo) & set(comp_lst)) == 1:
            move = list(set(combo) - set(comp_lst))
            if board.iloc[move[0]] == "-" and board.iloc[move[0]] == "-":
                board.iloc[move[0]] = comp
                comp_lst.append(
                    (move[0]),
                )
                return


def print_board(i):
    print("##############")
    print(f"Round: {i+1}")
    print("##############")
    time.sleep(1)
    print("\n")
    print(board)
    print("\n")


def check_win():
    for combo in win_combos:
        win = all(item in player_lst for item in combo)
        if win:
            print(board)
            print("\n")
            print("You won!")
            time.sleep(2)
            sys.exit()


def check_draw():
    if "-" not in board.values:
        print("It's a draw!")
        time.sleep(2)
        sys.exit()


# Starting game
player, comp = select_player()
for i in range(6):
    if player == "X":
        print_board(i)
        check_draw()
        p_row, p_col = userinput()
        user_move(player, p_row, p_col)
        check_win()
        comp_move(comp, i)
    else:
        comp_move(comp, i)
        print_board(i)
        check_draw()
        p_row, p_col = userinput()
        user_move(player, p_row, p_col)
        check_win()
