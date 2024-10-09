library(tidyverse)
library(worrrd)

df <- read_csv("wordlists.csv")

lang <- "Andi"
group <- "animals"

df |> 
  filter(group == group,
         language == lang) |> 
  select(word_id, word) |>
  mutate(word = toupper(word)) ->
  words_for_crossword

c <- crossword(words = words_for_crossword$word,
               clues = words_for_crossword$word, 
               r = 40, c = 40) 

attr(c, "positions") |> 
  group_by(word) |> 
  slice(1) |> 
  ungroup() |> 
  count(i, j)  |> 
  filter(n > 1) |> 
  rename(duplicates = n) ->
  duplicates

attr(c, "clues") |> 
  left_join(duplicates) |> 
  left_join(words_for_crossword) |> 
  select(i, j, word_id, duplicates) |> 
  arrange(word_id) |> 
  group_by(i, j) |> 
  summarise(label = str_c(word_id, collapse = "/")) ->
  labels

attr(c, "positions") |> 
  left_join(words_for_crossword) |> 
  left_join(duplicates) |> 
  ggplot(aes(x = j, y = i))+
  geom_tile(fill = "grey85", color = "black", linewidth = 0.1)+
  geom_text(aes(label = label), 
            size = 2, nudge_y = 0.35, nudge_x = -0.10,
            data = labels)+
  coord_fixed()+
  scale_y_reverse()+
  theme_void() ->
  p

p

ggsave(plot = p, 
       filename = str_c("results/", lang, "_", group, ".png"), 
       bg = "white", width = 7, height = 7)

attr(c, "positions") |> 
  left_join(words_for_crossword) |> 
  left_join(duplicates) |> 
  ggplot(aes(x = j, y = i))+
  geom_tile(fill = "grey85", color = "black", linewidth = 0.1)+
  geom_text(aes(label = letters))+
  geom_text(aes(label = label), 
            size = 2, nudge_y = 0.35, nudge_x = -0.10, 
            data = labels)+
  coord_fixed()+
  scale_y_reverse()+
  theme_void() ->
  a

a

ggsave(plot = a, 
       filename = str_c("answers/", lang, "_", group, ".png"), 
       bg = "white", width = 7, height = 7)
