library(tidyverse)
library(gganimate)

PLAYERS = 50
INITWEALTH = 50
ROUNDS = 5000

#initialize the bank matrix, indice PLAYERS+1 represent the welfare state, which tax all the players according to their wealth.
bank = matrix(0, nrow = ROUNDS, ncol = PLAYERS + 1)
bank[1,] = c(rep(INITWEALTH,PLAYERS),0)

#function to give a dollar to someone other than oneself
get_recipient = function(player) {
    sample(setdiff(1:PLAYERS, player), 1)}

#executes trades between players 
for (i in 2:ROUNDS){
  #every players with wealth strictly superior to 0 choose another player
  recipients = sapply(which(bank[i-1,-(PLAYERS+1)] > 0),get_recipient)
  
  #table of the dollars owed each person
  count_table = table(recipients)
  
  #get the indices of the people owed money
  indices = as.integer(names(count_table))
  
  #everyone gives up a dollar, unless they are at zero
  bank[i,-(PLAYERS+1)] = ifelse(bank[i - 1,-(PLAYERS+1)] > 0, bank[i - 1,-(PLAYERS+1)] - 1, bank[i - 1,-(PLAYERS+1)])
  
  #selected people receive dollars
  bank[i, indices] = bank[i, indices] + count_table
  
  #creating a df on the fly, to sort the playersid according to their wealth, in order to tax them
  df = data.frame(playerid=1:PLAYERS,wealth=bank[i,-(PLAYERS+1)])
  df = arrange(df,wealth)
  id_sorted = as.vector(df$playerid)
  
  #the percentage of taxation is proportional to the wealthiness
  tax_profile = 1:PLAYERS * 0.01
  tax_collected = as.vector(df$wealth) * tax_profile
  df$tax_collected = tax_collected
  df = arrange(df,playerid)
  
  #each player give tax to the State
  bank[i,-(PLAYERS+1)] %<>% {. - as.vector(df$tax_collected)}
  
  #Welfare State collect all the taxes
  bank[i,PLAYERS+1] %<>% {. + sum(tax_collected)}
  
  #Welfare State redistribute the money collected fairly to all the players
  redistribution = sum(tax_collected)/PLAYERS
  bank[i,-(PLAYERS+1)] %<>% {. + rep(redistribution,PLAYERS)}
}

#ANIMATION
#Ordering data into a dataframe
simulation = as.data.frame(bank)
names(simulation) = 1:(PLAYERS+1)
simulation = simulation %>%
  mutate(frame = 1:ROUNDS) %>%
  gather(person, wealth, 1:(PLAYERS+1)) %>%
  mutate(person = as.numeric(person)) %>%
  arrange(frame) %>%
  group_by(frame) %>%
  mutate(rank = rank(wealth, ties.method = "random")) %>%
  ungroup() %>%
  gather(histtype,playerid,c(person,rank)) %>%
  mutate(histtype = sprintf("Ordered by %s", histtype))

p <- ggplot(simulation, aes(x = playerid, y = wealth, frame = frame, fill=histtype)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_rect(aes( xmin = playerid - .4, xmax = playerid +.4, ymin = 0, ymax = wealth)) +
  scale_x_continuous(breaks = 1:PLAYERS) +
  coord_cartesian(xlim = c(0, PLAYERS), y = c(0, 5 * INITWEALTH)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='players',y='dollars') +
  facet_wrap( ~ histtype,ncol=1) +
  theme(legend.position = "none")

#set options for the animation package. Need ImageMagick installed on your computer
animation::ani.options(nmax = ROUNDS,
                       convert = 'C:\\Program Files\\ImageMagick-7.0.6-Q16')
#save the movie
gganimate(p, "dollar_stacked_welfare_2.mp4", interval = .01)






