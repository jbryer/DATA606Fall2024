#' Checks to see who wins the current round.
#'
#' This will compare the cards at position one. The winner gets both cards. If
#' there is a tie then the next three cards are put down and the fourth card
#' is compared. This is technically done recursively. The winner gets the
#' loosers five cards (the original, three put down, and the fourth is checked).
#'
#' The game ends when either of the players have no cards remaining.
#'
#' @param p1 cards for player 1.
#' @param p2 cards for player 2.
#' @return a list with three elements:
#' * `winner` - either a 1 or 2 for who won the round.
#' * `player1` - cards remaining for player 1.
#' * `player2` - cards remaining for player 2.
check_cards <- function(p1, p2) {
	winner <- 0
	if(length(p1) == 0) {
		winner <- 2
	} else if(length(p2) == 0) {
		winner <- 1
	} else {
		if(p1[1] == p2[1]) {
			# print('Entering a war!')
			if(length(p1) < 5) {
				p1 <- integer()
				p2 <- c(p2, p1)
				winner <- 2
			} else if(length(p2) < 5) {
				p1 <- c(p1, p2)
				p2 <- integer()
				winner <- 1
			} else {
				winners_cards <- c(p1[1:min(length(p1), 4)],
								   p2[1:min(length(p2), 4)])
				result <- check_cards(p1[min(length(p1), 5):length(p1)],
									  p2[min(length(p2), 5):length(p2)])
				if(result$winner == 1) {
					p1 <- c(result$player1, winners_cards)
					p2 <- result$player2
					winner <- 1
				} else if(result$winner == 2) {
					p1 <- result$player1
					p2 <- c(result$player2, winners_cards)
					winner <- 2
				}
			}
		} else if(p1[1] > p2[1]) {
			p1 <- c(p1[-1], p1[1], p2[1])
			p2 <- p2[-1]
			winner <- 1
		} else if(p2[1] > p1[1]) {
			p2 <- c(p2[-1], p2[1], p1[1])
			p1 <- p1[-1]
			winner <- 2
		} else {
			stop('Should not be here. What happend?!')
		}
	}
	return(list(
		player1 = p1,
		player2 = p2,
		winner = winner
	))
}

#' Initiate a game of war.
#'
#' @param cards a vector representing the cards. The default assumes a typical
#'        deck of 52 cards.
#' @param print how often should the function print the status of how many
#'        cards each player has. Set to 0 to don't print any updates.
#' @param max_games the maximum number of games to try before it is considered
#'        a draw.
#' @return a list with four elements:
#' * `games` - number of rounds it took for a player to win.
#' * `winner` - 0 for a draw, 1 if player 1 won, 2 if player 2 won.
#' * `p1_cards` - a numeric vector with the number of cards player 1 had after each round.
#' * `p2_cards` - a numeric vector with the number of cards player 2 had after each round.
play_war <- function(cards = rep(1:13, 4),
					 print = 0,
					 max_games = 2000) {
	deal <- sample(length(cards), length(cards) / 2, replace = FALSE)
	player1 <- cards[deal]
	player2 <- cards[-deal]

	games <- 0
	p1_cards <- integer(max_games)
	while(length(player1) > 0 & length(player2) > 0 & games < max_games) {
		result <- check_cards(player1, player2)
		player1 <- result$player1
		player2 <- result$player2
		p1_cards[games + 1] <- length(player1)
		if(length(player1) == 0 | length(player2) == 0) {
			break;
		}
		if(length(player1) + length(player2) != length(cards)) {
			stop(paste0('No longer `have ', length(cards), ' cards! ',
						(length(player1) + length(player2))))
		}
		games <- games + 1
		if(print > 0) {
			if(games %% print == 0) {
				print(paste0('Game ', games, ':',
							 ' Player 1 cards: ', length(player1), '; ',
							 ' Player 2 cards: ', length(player2)))
			}
		}
	}
	winner <- 0
	if(length(player1) == 0) {
		winner <- 2
	} else if(length(player2) == 0) {
		winner <- 1
	}
	return(list(games = games,
				winner = winner,
				p1_cards = p1_cards,
				p2_cards = length(cards) - p1_cards))
}

if(FALSE) {
	# Play one game
	play_war(print = 25)$games

	n_games <- 10000
	war_games <- list()
	for(i in seq_len(n_games)) {
		# set.seed(i) # Used to track down issues and debug
		war_games[[i]] <- play_war(print = 0)
	}

	plays_to_win <- sapply(war_games, FUN = function(x) { x$games })

	# Check to see if there were any draws
	draws <- which(plays_to_win == 2000)
	sum(plays_to_win == 2000)
	sum(plays_to_win == 2000) / n_games * 100

	hist(plays_to_win,
		 main = 'Histogram of Number of Plays to Win Game of War',
		 xlab = 'Number of Plays')
	mean(plays_to_win)
	median(plays_to_win)
	min(plays_to_win)
	max(plays_to_win)
}
