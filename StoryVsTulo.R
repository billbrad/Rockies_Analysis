#Story vs. Tulo: Comparison of Power Shortstops by Age

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

TroyVsTrevor <- bind_rows(get_birthyear("Troy Tulowitzki"),
                          get_birthyear("Trevor Story"))

      #creating row in "Batting" for Trevor's age 26 season

Batting %>%
  filter(playerID == "storytr01" | playerID == "tulowtr01") %>%
  add_row(playerID = "storytr01", yearID = 2019, HR = 35) -> batting.new

      #Creating the cumulative HR data frame

batting.new %>%
  inner_join(TroyVsTrevor, by = "playerID") %>%
  mutate(Age = yearID - birthyear) %>%
  select(Player, Age, HR) %>%
  group_by(Player) %>%
  mutate(CHR = cumsum(HR)) -> COL.SS.HR

      #selecting only through age 26 seasons

COL.SS.HR %>% filter(Age <= 26) -> COL.SS.26

      #creating the plot
      
giraffe <- ggplot(COL.SS.26, aes(x = Age, y = CHR, linetype = Player)) +
  geom_line(color = "Purple", size = 1.25) +
    annotate("text", x = c(21.6, 23), y = c(0, 25),
             label = c("Troy Tulowitzki", "Trevor Story")) +
  xlab("Age for Season") + ylab("Career Home Runs by Year") +
  ggtitle("Story vs. Tulo:", subtitle = "Power Shortstops of the Rockies") +
  theme(legend.position = "none",
        panel.grid.minor =   element_blank(),
        panel.grid.major =   element_line(colour = "white", size=0.75))


giraffe
ggsave("StoryVsTulo.png")

 
