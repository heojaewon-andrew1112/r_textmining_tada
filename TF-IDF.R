category1 <- "택시 업계|택시업계|조합"
category2 <- "정부"
category3 <- "국회의원|한국당|자유한국당|자한당|자한|민주당|더불어민주당"

bind_category <-  bind_rows(
  word_sympathy %>%
    filter(str_detect(reply, category1)) %>%
    mutate(category = "택시업계"),
  
  word_sympathy %>%
    filter(str_detect(reply, category2)) %>%
    mutate(category = "정부"),
  
  word_sympathy %>%
    filter(str_detect(reply, category3)) %>%
    mutate(category = "국회의원"),
)

bind_category %>%
  group_by(id) %>%
  distinct(category, .keep_all = T) %>%
  ungroup() %>%
  count(category)

stopword_category <- c("택시 업계", "택시업계", "업계", "조합",
                       "정부", "국회의원", "한국당", "자유한국당",
                       "자한당", "자한", "민주당", "더불어민주당")

frequency_category <- bind_category %>%
  filter(!word %in% stopword_category) %>%
  
  group_by(id) %>%
  distinct(word, .keep_all = T) %>%
  ungroup() %>%
  
  count(category, word, sort = T) %>%
  filter(str_count(word) >= 2)

tfidf_category <- frequency_category %>%
  bind_tf_idf(term = word,
              document = category,
              n = n) %>%
  arrange(-tf_idf)

tfidf_category %>%
  group_by(category) %>%
  slice_max(tf_idf, n = 15, with_ties = F) %>%
  print(n = Inf)

stopword_tfidf <- c("국회의원님하고", "현정부", "에휴")

top10 <- tfidf_category %>%
  filter(!word %in% stopword_tfidf) %>%
  group_by(category) %>%
  slice_max(tf_idf, n = 10, with_ties = F)

# 막대 그래프 만들기

top10$category <- factor(top10$category,
                         levels = c("택시업계", "정부", "국회의원"))

ggplot(top10, aes(x = reorder_within(word, tf_idf, category),
                  y = tf_idf,
                  fill = category))+
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~ category, scales = "free", ncol = 3) +
  scale_x_reordered() +
  scale_y_continuous(n.breaks = 5,
                     labels = number_format(accuracy = .001)) +
  labs(title = "타다 금지법 기사 댓글 주요 단어",
       subtitle = "카테고리별 TF-IDF Top 10",
       x = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 11))

# 카테고리별 댓글 내용 확인

reply_category <- bind_category %>%
  group_by(category) %>%
  distinct(id, .keep_all = T)

reply_category %>%
  filter(category == "택시업계") %>%
  find_word(reply_raw, keyword = "대기업")

reply_category %>%
  filter(category == "정부") %>%
  find_word(reply_raw, keyword = "지원")

reply_category %>%
  filter(category == "국회의원") %>%
  find_word(reply_raw, keyword = "박홍근")

