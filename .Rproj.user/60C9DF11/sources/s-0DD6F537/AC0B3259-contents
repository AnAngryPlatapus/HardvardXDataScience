# Discrete Probablility -- Categorical Data

# Monte Calro Simulations
beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view

# Setting the Random Seed
set.seed(1986) 
# A popular way to pick the seed is the year - month - day. For example, we picked 1986 on December 20, 2018:  2018 − 12 − 20 = 1986
?set.seed

set.seed(1)
set.seed(1, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5

# Using the mean Function for Probability
# In R, applying the mean() function to a logical vector returns the proportion of elements that are TRUE

beads <- rep(c("red", "blue"), times = c(2,3))
beads
mean(beads == "blue")

# Independence
3 / (3 + 5 + 7)
12 / 15

balls <- rep(c("cyan", "magenta", "yellow"), times = c(3, 4, 7))
?rep
(3 / 15) * (12/15)



# Code: Introducing paste() and expand.grid()

# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
?expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))
expand.grid(shirt = c("white", "grey", "plaid"), pants = c("blue", "black", "plaid", stuff=c("things", "stuff")))
rep(list(c(0,1)), 6)
expand.grid(rep(list(c(0,1)), 6))
# Code: Generating a deck of cards

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

# Code: Permutations and combinations

# Correction: The code shown does not generate all 7 digit phone numbers because phone numbers can have repeated digits. It generates all possible 7 digit numbers without repeats.

library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

# Code: Probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

# Code: Probability of a natural 21 in blackjack

aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

# Code: Monte Carlo simulation of natural 21 in blackjack

# Note that your exact values will differ because the process is random and the seed is not set.

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)


# Code: The birthday problem

# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
bdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays


# Code: Function for birthday problem Monte Carlo simulations

# Note that the function body of compute_prob() is the code that we wrote in the previous video. If we write this code as a function, we can use sapply() to apply this function to several values of n.

# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)

# Code: Element-wise operation over vectors and sapply

x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)

# Code: Computing birthday problem probabilities with sapply

# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob


# Code: Estimating a practical value of B

# This code runs Monte Carlo simulations to estimate the probability of shared birthdays using several B values and plots the results. When B is large enough that the estimated probability stays stable, then we have selected a useful value of B.

B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 


# Code: Monte Carlo simulation of stick strategy

B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

# Code: Monte Carlo simulation of switch strategy

switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)

?seq

library(gtools)
library(tidyverse)

?expand.grid

nrow(permutations(8, 3))

3/8 * 2/7 * 1/6
set.seed(1)

test <- c("Jamaica", "Jamaica", "Jamaica")
length(unique(test)) == 1

race <- replicate(10000, {
  runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
  winner <- sample(runners, 3)
  length(unique(winner)) == 1
})
nrow(combinations(8, 3))

mean(race)
combinations(6,2)
combinations(6,2)
6 * nrow(combinations(6, 2)) * 2
6 * 15 * 2

6 * nrow(combinations(6, 3)) * 3

meal_combos <- function(entres) {
  entres * 15 * 3
}
sapply(1:12, meal_combos)


meal_combos <- function(sides) {
  6 * nrow(combinations(sides, 2)) * 3
}
sapply(2:12, meal_combos)

data(esoph)


####### QUESTION 3a/b/c #######
# How many groups are in the study?
nrow(esoph)

# How many cases are there?
all_cases <- sum(esoph$ncases)

# How many controls are there?
all_controls <- sum(esoph$ncontrols)


####### QUESTION 4a/b/c/d #######Divide the total amount needed per loan by the loan amount to determine the interest rate.
# What is the probability that a subject in the highest alcohol consumption group
#  is a cancer case?
esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols) + sum(ncases), probability=sum_cases/tot)

# What is the probability that a subject in the lowest alcohol consumption group
#  is a cancer case?
esoph %>% filter(alcgp == "0-39g/day") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols)+sum(ncases), probability=sum_cases/tot)

# Given that a person is a case, what is the probability that they smoke 10g or
#  more a day?
esoph %>% summarize(tot_cases = sum(ncases))
esoph %>% filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncases))
122/200

# Given that a person is a control, what is the probability that they smoke 10g or
#  more a day?
esoph %>% summarize(tot_cases = sum(ncontrols))
esoph %>% filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncontrols))
450/975


####### QUESTION 5a/b/c/d #######
# For cases, what is the probability of being in the highest alcohol group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases))
45/all_cases

# For cases, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+") %>%
  summarize(sum_cases=sum(ncases))
31/all_cases

# For cases, what is the probability of being in the highest alcohol group and
#  the highest tobacco group?
esoph %>% filter(alcgp == "120+" & tobgp =="30+") %>%
  summarize(sum_cases = sum(ncases))
10/all_cases

# For cases, what is the probability of being in the highest alcohol group or
#  the highest tobacco group?
esoph %>% filter(alcgp == "120+" | tobgp =="30+") %>%
  summarize(sum_cases = sum(ncases))
66/all_cases


####### QUESTION 6a/b/c/d/e/f #######
# For controls, what is the probability of being in the highest alcohol group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# How many times more likely are cases than controls to be in the highest alcohol
#  group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), case_sum = sum(ncases),
            co_prob = contr_sum/all_controls, ca_prob = case_sum/all_cases,
            ratio = ca_prob/co_prob)

# For controls, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# For controls, what is the probability of being in the highest alcohol group and
#  the highest tobacco group?
esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# For controls, what is the probability of being in the highest alcohol group or
#  the highest tobacco group?
esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# How many times more likely are cases than controls to be in the highest alcohol
#  group or the highest tobacco group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), case_sum = sum(ncases),
            co_prob = contr_sum/all_controls, ca_prob = case_sum/all_cases,
            ratio = ca_prob/co_prob)

# For controls, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+") %>% 
  summarize(contr_sum = sum(ncontrols), contr_prob = contr_sum/all_controls)

# For controls, what is the probability of being in the highest alcohol group 
# and the highest tobacco group?
esoph %>% filter(alcgp == "120+" & tobgp == "30+") %>% 
  summarize(contr_sum = sum(ncontrols), contr_prob = contr_sum/all_controls)

# For controls, what is the probability of being in the highest alcohol group 
# or the highest tobacco group?
esoph %>% filter(alcgp == "120+" |tobgp == "30+") %>% 
  summarize(contr_sum = sum(ncontrols), contr_prob = contr_sum/all_controls)

# How many times more likely are cases than controls to be in the highest 
# alcohol group or the highest tobacco group?
esoph %>% filter(alcgp == "120+" |tobgp == "30+")  %>%
  summarize(contr_sum = sum(ncontrols), case_sum = sum(ncases),
            co_prob = contr_sum/all_controls, ca_prob = case_sum/all_cases,
            ratio = ca_prob/co_prob)


library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches

# Code: Using pnorm() to calculate probabilities

# Given male heights x:
  
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# We can estimate the probability that a male is taller than 70.5 inches using:
  
1 - pnorm(70.5, mean(x), sd(x))

# Code: Discretionary and the normal approximation

# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

library(tidyverse)
x <- seq(-4, 4, length = 200)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()
Divide the total amount needed per loan by the loan amount to determine the interest rate.
dnorm(z, mu, sigma)

# Code: Generating normally distributed random numbers

# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

# Code: Monte Carlo simulation of tallest person over 7 feet

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)

# Code: Plotting the normal distribution with dnorm

# Use d to plot the density function of a continuous distribution. Here is the density function for the normal distribution (abbreviation norm()):
  
x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()


?rnorm
set.seed(16, sample.kind="Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)
mu <- mean(act_scores)
s <- sd(act_scores)


length(act_scores[act_scores>36])
pnorm(30, mu, s, lower.tail=FALSE)

pnorm(10, mu, s)

x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)

data.frame(x, f= f_x) %>% 
    ggplot(aes(x, f)) +
    geom_line()

z_scores <- act_scores - mu
z_scores <- z_scores / s

z_mu <- mean(z_scores)
z_s <- sd(z_scores)
pnorm(2, z_mu, z_s, lower.tail=FALSE)

reverse_score <- function(z_score) {
  retVal <- ( z_score * s) + mu
  retVal
}
reverse_score(2)
?pnorm
qnorm(.975, mu, s)
"library(dslabs)
data(murders)
head(murders)

prop.table(table(heights$sex))

my_data <- heights$height

a <- seq(min(my_data), max(my_data), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(my_data <= x)
}
cdf_values <- sapply(a, cdf_function)
cdf_values
plot(a, cdf_values)"

cdf_func <- function(value) {
  mean(value <= act_scores)
}
a <- 1:36
cdf_vals <- sapply(a, cdf_func)
plot(a, cdf_vals)
1 - cdf_func(30)

qnorm(.95, 20.9, 5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)

theoretical_quantiles <- qnorm(p, 20.9, 5.7)

plot(theoretical_quantiles, sample_quantiles)
abline(0,1)

# Code: Modeling a random variable

# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)

# Monte Carlo simulation: Chance of casino losing money on roulette

# We build a sampling model for the random variable that represents the casino's total winnings. 

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")
?rep
?sample
?qnorm
incorrect <- -.25
correct <- 1
n <- 44
choices <- 5


prob_correct <- 1/5

.8 * -.25

ev <- -.25* (1-prob_correct) + 1 * prob_correct
se <- (abs(-.25 - 1) * sqrt(prob_correct* (1-prob_correct))) * sqrt(44)


pnorm(8, ev, se, lower.tail = FALSE)
set.seed(21, sample.kind = "Rounding")

B <- 10000
S <- replicate(B, {
  X <- sample(c(1, -0.25), 44, replace = TRUE, prob = c(prob_correct, (1-prob_correct)))
  sum(X)
})

mean(8 <= S)

prob_correct <- 1/4
prob_wrong <- 1 - prob_correct

ev <- 44 * prob_correct
se <- abs(prob_correct) * sqrt(prob_correct * (1-prob_correct)) * sqrt(44)
se
p <- seq(0.25, 0.95, 0.05)
qnorm(p, ev, se, lower.tail=FALSE)


B <- 10000
p <- seq(0.25, 0.95, 0.05)
for (val in p) {
  S <- replicate(B, {
    X <- sample(c(1, 0), 44, replace = TRUE, prob = c(val, 1-val))
    sum(X)
  })
  print(paste('p:', as.character(val)))
  print(mean(35 <= S))
}

p <- seq(0.25, 0.95, 0.05)
prob_c <- 1/4
score <- sapply(p, function(v){
  ev <- 44 * (1 * v) + (0 * (1 - v))
  se <- sqrt(44) * abs(1 - 0) * sqrt(v*(1-v))
  1-pnorm (35, ev, se)
})
score

min(p[which(score > 0.8)])

p_win <- 5 / 38
p_lose <- 1 - p_win
win <- 6
lose <- -1

ep <- win * p_win + lose * p_lose
ep

se <- abs(lose-win) * sqrt(p_win*(p_lose))
se


ep_n <- win * p_win + lose * p_lose
ep_sum <- ep_n * 500
se_sum <-se * sqrt(500)

pnorm(0, ep_sum, se_sum)

# Code: Interest rate sampling model

n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

# Code: Interest rate Monte Carlo simulation

B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)Divide the total amount needed per loan by the loan amount to determine the interest rate.
})

# Code: Plotting expected losses

library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

# Code: Expected value and standard error of the sum of 1,000 loans

n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

# Code: Calculating interest rates for expected value of 0

# We can calculate the amount to add to each loan so that the expected value is 0 using the equation

# . Note that this equation is the definition of expected value given a loss per foreclosure with foreclosure probability and profit if there is no foreclosure (probability ).

# We solve for

# and calculate :
  
x = - loss_per_foreclosure*p/(1-p)
x

# On a $180,000 loan, this equals an interest rate of:
  
x/180000
  
# Code: Calculating interest rate for 1% probability of losing money

l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

# Code: Monte Carlo simulation for 1% probability of losing money

# Note that your results will vary from the video because the seed is not set.

B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money


# Code: Expected value with higher default rate and interest rate

p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

# Code: Calculating number of loans for desired probability of losing money

# The number of loans required is:
  
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

# Code: Monte Carlo simulation with known default probability

# This Monte Carlo simulation estimates the expected profit given a known probability of default . Note that your results will differ from the video because the seed is not set.

B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)


# Code: Monte Carlo simulation with unknown default probability


p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million

hist(profit)

library(dslabs)
x <- heights$height
m <- mean(x)
s <- sqrt(mean((x-m)^2))

s
sd(x)

n <- length(x)
s-sd(x)*sqrt((n-1) / n)

library(tidyverse)
library(dslabs)

data(death_prob)
head(death_prob)

