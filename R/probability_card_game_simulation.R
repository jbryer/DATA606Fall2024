cards <- expand.grid(c('Hearts', 'Spades', 'Diamonds', 'Clubs'), 
					 c('Ace', 2:10, 'Jack', 'Queen', 'King'))
cards <- as.data.frame(cards)
# cards <- paste(cards[,1], cards[,2])
nrow(cards)
names(cards) <- c('Suit', 'Value')
head(cards)

#' 
#' @parm card can be integer between 1 and 52 or the suit and value
#' 
card_game <- function(card) {
	if(is.numeric(card)) {
		card <- cards[card,]
	}
	tmp <- apply(card, 1, FUN = function(x) {
		if(x['Value'] == 'Ace') {
			return(5)
		} else if(x['Suit'] == 'Hearts') {
			return(1)
		} else if(x['Value'] == 'King' & x['Suit'] == 'Spades') {
			return(10)
		} else {
			return(0)
		}
	})
	return(unname(tmp))
}

cards$winning <- card_game(1:52)
card_game(c(1, 3))
cards
card_game(cards[1,])
card_game(cards[49,])
card_game(cards[4,])
card_game(cards[6,])
card_game(cards[50,])
card_game(cards[sample(1:52, 1),])


random_cards <- sample(1:52, 10000, replace = TRUE)
winnings <- sapply(random_cards, FUN = function(x) { card_game(cards[x,]) })
unique(winnings)
hist(winnings)
mean(winnings)

# Event	X	P(X)	X P(X)
# Heart (not Ace)	1	12/52 0.2307692	    12/52  0.2307692
# Ace	            5	4/52  0.07692308	20/52  0.3846154
# King of Spades	10	1/52  0.01923077	10/52  0.1923077
# All else	        0	35/52 0.6730769.   	0.      0
tmp <- table(winnings, useNA = 'ifany') %>% prop.table()
sum(as.integer(names(tmp)) * tmp)

table(winnings)
