library(readr)
library(dplyr)

setwd("F:\\r_adp\\r_textmining\\r_datamining\\r_textmining_tada\\data")
raw_tada <- read_csv("news_comment_tada.csv") %>%
  mutate(id = row_number())
glimpse(raw_tada)

library(stringr)
library(textclean)

tada <- raw_tada %>%
  filter(str_count(reply, " ") >= 1) %>% # 띄어쓰기 하나 이상
  mutate(reply_raw = str_squish(replace_html(reply)), # 원문 남기기
         reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply))

library(tidytext)
library(KoNLP)

word_noun <- tada %>%
  unnest_tokens(input = reply,
                output = word,
                token = extractNoun,
                drop = F)
# 단어 빈도 구하기
frequency <- word_noun %>%
  count(word, sort = T) %>%
  filter(str_count(word) > 1) # 두 글자 이상만 남기기

# 상위 단어 추출
frequency %>%
  head(30) %>%
  print(n = Inf)

# 불용어 목록 생성
stopword_noun <- c("들이", "하면", "하게", "해서")

# 주요 단어 목록
top20_noun <- frequency %>%
  filter(!word %in% stopword_noun) %>%
  head(20)

top20_noun %>%
  print(n = Inf)

# 막대 그래프 만들기
library(scales)
library(ggplot2)

ggplot(top20_noun, aes(x = reorder(word, n),  y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = comma(n, accuracy = 1)), hjust = -0.3) + 
  scale_y_continuous(limits = c(0, 3200)) +
  
  labs(title = "타다 금지법 기사 댓글 주요 단어",
       subtitle = "언급 빈도 Top 20",
       x = NULL) +
  
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic", size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 13))