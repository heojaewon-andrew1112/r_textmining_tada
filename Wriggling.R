library(KoNLP)
library(dplyr)
library(tidytext)
library(tidyr)
library(stringr)

pos_tada <- tada %>%
  unnest_tokens(input = reply,
                output = word_pos,
                token = SimplePos22,
                drop = F)

separate_pos_tada <- pos_tada %>%
  separate_rows(word_pos, sep = "[+]") %>%
  filter(str_detect(word_pos, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word_pos, "/pv|/pa"),
                       str_replace(word_pos, "/.*$", "다"),
                       str_remove(word_pos, "/.*$"))) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)

separate_pos_tada %>%
  select(word)

# 파이 계수 구하기
library(widyr)
word_cors <- separate_pos_tada %>%
  add_count(word) %>%
  filter(n >= 20) %>%
  pairwise_cor(item = word, feature = id, sort = T)
word_cors

# 관심 단어와 관련성이 큰 단어
target <- c("타다", "정부", "택시")

top_cors <- word_cors %>%
  filter(item1 %in% target) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 10)
top_cors

top_cors$item1 <- factor(top_cors$item1, levels = target)

library(ggplot2)
ggplot(top_cors, aes(x = reorder_within(item2, correlation, item1),
                     y = correlation,
                     fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  
  labs(title = "타다 금지법 기사 댓글 주요 단어",
       subtitle = "파이 계수 Top 10",
       x = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 11))

# 네트워크 그래프 만들기
library(tidygraph)
set.seed(1234)
graph_cors <- word_cors %>%
  filter(correlation >= 0.15) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1234)
library(ggraph)
library(scales)

windows()

ggraph(graph_cors, layout = "fr") +
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation,
                     edge_width = correlation),
                 show.legend = F) +
  scale_edge_width(range = c(1, 4)) +
geom_node_point(aes(size = centrality,
                    color = group),
                show.legend = F) +
  scale_size(range = c(5, 10)) +
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5,
                 family = "nanumgothic") +
  theme_graph()

# 선거-내년-총선
library(crayon)
tada %>%
  filter(str_detect(reply_raw, "선거")) %>%
  find_word(x = reply_raw, keyword = "내년", n = 10)

tada %>%
  filter(str_detect(reply_raw, "내년")) %>%
  find_word(x = reply_raw, keyword = "총선", n = 10)

# 목적지-손님-고르다
tada %>%
  filter(str_detect(reply_raw, "목적지")) %>%
  find_word(x = reply_raw, keyword = "손님", n = 10)

tada %>%
  filter(str_detect(reply_raw, "손님")) %>%
  find_word(x = reply_raw, keyword = "골라", n = 10)

# 엔그램으로 연이어 사용된 단어
line_comment <- separate_pos_tada %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))

# 바이그램으로 토큰화
bigram_comment <- line_comment %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)
bigram_comment

bigram_seprated <- bigram_comment %>%
  separate(bigram, c("word1", "word2"), sep = " ")

pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()
pair_bigram

# 그래프 만들기
set.seed(1234)
graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

window()
set.seed(1234)
ggraph(graph_bigram, layout = "fr") +
  geom_edge_link(color = "gray50",
                 alpha = 0.5) +
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  scale_size(range = c(5, 15)) +
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5,
                 family = "nanumgothic") +
  theme_graph()

# 댓글 내용 살펴보기
line_tada <- line_comment %>%
  left_join(tada, by = "id")

line_tada %>%
  select(sentence)

# 역행 - 시대 - 뛰떨어지다
line_tada %>%
  filter(str_detect(sentence, "시대 역행")) %>%
  find_word(x = reply_raw, keyword = "역행", n = 10)

line_tada %>%
  filter(str_detect(sentence, "시대 뒤떨어지다")) %>%
  find_word(x = reply_raw, keyword = "뒤떨어", n = 10)

# 택시 - 면허 - 사다
line_tada %>%
  filter(str_detect(sentence, "택시 면허")) %>%
  find_word(x = reply_raw, keyword = "면허", n = 10)

line_tada %>%
  filter(str_detect(sentence, "면허 사다")) %>%
  find_word(x = reply_raw, keyword = "사서", n = 10)

