## load package
library(tidyverse)
library(ggpubr)
library(lubridate)

#use readr to read in CSV from a URL
df <- read_csv("https://projects.fivethirtyeight.com/nba-model/nba_elo_latest.csv")

tail(df, n = 30)

glimpse(df)

brk_df <-
  df %>% 
  filter((team1 == "BRK" | team2 == "BRK") & date <= today()) %>%
  mutate(elo_post = if_else(team1 == "BRK", elo1_post, 
                            if_else(team2 == "BRK", elo2_post, NA_real_)),
         elo_diff = if_else(team1 == "BRK", (elo1_post - elo1_pre),
                            if_else(team2 == "BRK", (elo2_post - elo2_pre), NA_real_)))

brk_elo_diff <-
  ggplot(brk_df, mapping = aes(date, elo_diff)) +
  geom_segment(aes(xend=date), yend=0) +
  geom_point(color = 'black', shape = 21, aes(fill = 'white')) +
  scale_fill_manual(values="white") +
  theme_classic() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-14")), color = "red") +
  geom_text(aes(x=as.Date("2021-01-21"),y=50, label = "James Harden trade", color = "red")) +
  ylim(-50, 50) +
  ggtitle("Brooklyn Nets") +
  ylab("ELO postgame - ELO pregame") +
  xlab("Date in 2020-2021 NBA season")

brk_elo_raw <-
ggplot(brk_df, mapping = aes(date, elo_post)) +
  geom_line() +
  geom_point(color = 'black', shape = 21, aes(fill = 'white')) +
  scale_fill_manual(values="white") +
  theme_classic() +
  theme(legend.position = "none") +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-14")), color = "red") +
  geom_hline(yintercept = 1500, linetype = "dotted") +
  geom_hline(yintercept = 1600, linetype = "dotted") +
  geom_hline(yintercept = 1700, linetype = "dotted") +
  geom_text(aes(x=as.Date("2020-12-28"),y=1490, label = "Average rating")) +
  geom_text(aes(x=as.Date("2020-12-28"),y=1610, label = "Playoff bound")) +
  geom_text(aes(x=as.Date("2020-12-28"),y=1710, label = "Title contender")) +
  geom_text(aes(x=as.Date("2021-01-21"),y=1750, label = "James Harden trade", color = "red")) +
  ggtitle("Brooklyn Nets") +
  ylab("ELO postgame") +
  xlab("Date in 2020-2021 NBA season") +
  ylim(1490,1750)

lal_df <-
  df %>% 
  filter((team1 == "LAL" | team2 == "LAL") & date <= today()) %>%
  mutate(elo_post = if_else(team1 == "LAL", elo1_post, 
                            if_else(team2 == "LAL", elo2_post, NA_real_)),
         elo_diff = if_else(team1 == "LAL", (elo1_post - elo1_pre),
                            if_else(team2 == "LAL", (elo2_post - elo2_pre), NA_real_)))

lal_elo_diff <-
ggplot(lal_df, mapping = aes(date, elo_diff)) +
  geom_segment(aes(xend=date), yend=0, color = '#552583') +
  geom_point(color = '#FDB927') +
  theme_classic() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  ggtitle("Los Angeles Lakers") +
  ylab("ELO postgame - ELO pregame") +
  xlab("Date in 2020-2021 NBA season") +
  ylim(-50, 50)

lal_elo_raw <-
ggplot(lal_df, mapping = aes(date, elo_post)) +
  geom_line(color = '#552583') +
  geom_point(color = '#FDB927') +
  theme_classic() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 1500, linetype = "dotted") +
  geom_hline(yintercept = 1600, linetype = "dotted") +
  geom_hline(yintercept = 1700, linetype = "dotted") +
  geom_text(aes(x=as.Date("2020-12-28"),y=1490, label = "Average rating")) +
  geom_text(aes(x=as.Date("2020-12-28"),y=1610, label = "Playoff bound")) +
  geom_text(aes(x=as.Date("2020-12-28"),y=1710, label = "Title contender")) +
  ggtitle("Los Angeles Lakers") +
  ylim(1490,1750) +
  ylab("ELO postgame") +
  xlab("Date in 2020-2021 NBA season")

fig_1 <- ggpubr::ggarrange(brk_elo_raw, lal_elo_raw, brk_elo_diff, lal_elo_diff,
                           ncol = 2, nrow = 2, align = "hv",
                           labels = c("A", "B", "C", "D"),
                           font.label = list(size = 15))

ggexport(fig_1,
         filename = "Nets_vs_Lakers.pdf",
         width = 9.3,
         height = 11.3,
         unit = "in",
         dpi = 300)
