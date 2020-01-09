#Arenado vs. Schmidt: Comparison of Power 3rd Basemen by Age

library(Lahman)
library(tidyverse)

#getting the birthyear for each player:

get_birthyear <- function(Name) {
  Names <- unlist(strsplit(Name, " "))
  Master %>%
    filter(nameFirst == Names[1],
           nameLast == Names[2]) %>%
    mutate(birthyear = ifelse(birthMonth >= 7,
                              birthYear + 1, birthYear),
           Player = paste(nameFirst, nameLast)) %>%
    select(playerID, Player, birthyear)
}

Schmidt.Arenado <- bind_rows(get_birthyear("Mike Schmidt"),
                          get_birthyear("Nolan Arenado"))

#filter Batting data for Nolan and Mike, then create
#row for Nolan's 2019 HR total:

Batting %>%
  filter(playerID == "schmimi01" | playerID == "arenano01") %>%
  add_row(playerID = "arenano01", yearID = 2019, HR = 41) -> batting.new

#Creating the cumulative HR data frame:

batting.new %>%
  inner_join(Schmidt.Arenado, by = "playerID") %>%
  mutate(Age = yearID - birthyear) %>%
  select(Player, Age, HR) %>%
  group_by(Player) %>%
  mutate(CHR = cumsum(HR)) -> x3B.HR

#selecting only through age 28 seasons(Nolan's current age):

x3B.HR %>% filter(Age <= 28) -> x3B.28

#creating the plot:

giraffe <- ggplot(x3B.28, aes(x = Age, y = CHR, linetype = Player)) +
  geom_line(color = "Blue", size = 1.25) +
  xlab("Age for Season") + ylab("Career Home Runs at Season's End") +
  ggtitle("Schmidt vs Arenado:", subtitle = "Power Production of Great Third Basemen") +
  theme(legend.position = c(0.8, 0.25)) +
  scale_x_continuous( expand = c(0 ,0), limits = c(22, 28.25)) +
  scale_y_continuous( expand = c(0, 0), limits = c(0, 250))


giraffe
ggsave("Arenado_Schmidt.png")
