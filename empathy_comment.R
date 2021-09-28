# 로그 오즈비 구하기

word_sympathy <- word_noun %>%
  rename(like = sympathyCount,
         dislike = antipathyCount) %>%
  
  mutate(diff = like - dislike,
         sympathy = ifelse(diff >= 1, "like",
                    ifelse(diff <= -1, "dislike", "neutral")))

# 공감 여부별 댓글 수

word_sympathy %>%
  distinct(id, .keep_all = T) %>%
  count(sympathy, sort = T)

frequency_sympathy <- word_sympathy %>%
  count(sympathy, word) %>%
  filter(str_count(word) > 1 &
           sympathy != "neutral")

library(tidyr)
frequency_wide <- frequency_sympathy %>%
  pivot_wider(names_from = sympathy,
              values_from = n,
              values_fill = list(n = 0))

# 로그 오즈비 구하기

frequency_wide <- frequency_wide %>%
  mutate(log_odds_ratio = log(((like + 1) / (sum(like +1))) /
                                ((dislike + 1) / (sum(dislike +1)))))

frequency_wide %>%
  arrange(-log_odds_ratio)

# 주요 단어 추출하기

top10_odds <- frequency_wide %>%
  filter(like >= 20 | dislike >= 20) %>%
  group_by(sympathy = ifelse(log_odds_ratio > 0, "like", "dislike")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10_odds %>%
  arrange(-log_odds_ratio)

# 막대 그래프 만들기

col_sentiment <- c("#619CFF", "#F8766D")

top10_odds$sympathy <- factor(top10_odds$sympathy,
                              levels = c("like", "dislike"))

ggplot(top10_odds, aes(x = reorder(word, log_odds_ratio),
                       y = log_odds_ratio,
                       fill = sympathy)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = col_sentiment,
                    labels = c("공감", "비공감")) +
  
  labs(title = "타다 금지법 기사 댓글 주요 단어",
       subtitle = "공감 vs 비공감 로그 오즈비 Top 10",
       x = NULL, fill = NULL) +
  
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))

# 댓글 내용 살펴보기

tada %>%
  filter(str_detect(reply_raw, "조합")) %>%
  head(3) %>%
  pull(reply)

library(crayon)
font <- combine_styles(make_style("ivory"),
                       make_style("deeppink", bg = TRUE),
                       make_style("bold"))

keyword <- "조합"

tada %>%
  filter(str_detect(reply_raw, keyword)) %>%
  head(3) %>%
  mutate(reply = paste0(str_replace_all(reply, keyword,
                                        font(keyword)))) %>%
  pull(reply) %>%
  cat(sep = "\n\n")

find_word <- function(df, x, keyword, n = 6){
  font <- combine_styles(make_style("ivory"),
                         make_style("deeppink", bg = TRUE),
                         make_style("bold"))
  
  df %>%
    filter(str_detect({{x}}, keyword)) %>%
    head(n) %>%
    mutate(reply = paste0(str_replace_all(reply, keyword,
                                          font(keyword)))) %>%
    pull(reply) %>%
    cat(sep = "\n\n")
}

tada %>% find_word(x = reply_raw, keyword = "조합", n = 6)

# 공감 비공감 댓글 원문 추출하기

tada <- tada %>%
  left_join(word_sympathy %>%
              distinct(id, .keep_all = T) %>%
              select(id, sympathy, diff),
            by = "id") 

reply_like <- tada %>%
  filter(sympathy == "like") %>%
  arrange(-diff)

reply_dislike <- tada %>%
  filter(sympathy == "dislike") %>%
  arrange(diff)

reply_like %>% find_word(reply_raw, "조합", n=10)
reply_like %>% find_word(reply_raw, "소비자", n=10)
reply_like %>% find_word(reply_raw, "동남아", n=10)

reply_dislike %>% find_word(reply_raw, "렌트카", n = 10)
reply_dislike %>% find_word(reply_raw, "댓글", n = 10)
reply_dislike %>% 
  filter(!str_detect(reply, "한국당")) %>%
  find_word(reply, "한국", n = 10)
