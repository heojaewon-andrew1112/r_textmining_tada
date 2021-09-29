# 토픽 모델링 전처리
noun_tada <- tada %>%
  distinct(reply, .keep_all = T) %>%
  filter(str_count(reply, boundary("word")) >= 3) %>%
  unnest_tokens(input = reply,
                output = word,
                token = extractNoun,
                drop = F) %>%
  filter(str_count(word) > 1)

# 중복, 고빈도 단어 제거
unique_noun_tada <- noun_tada %>%
  group_by(id) %>%
  distinct(word, .keep_all = T) %>%
  ungroup() %>%
  add_count(word) %>%
  filter(n <= 200) %>%
  select(id, word)
unique_noun_tada

# LDA 만들기
count_word <- unique_noun_tada %>%
  count(id, word, sort = T)

dtm_tada <- count_word %>%
  cast_dtm(document = id, term = word, value = n)

# 최적의 토픽수 정하기
library(ldatuning)
models_tada <- FindTopicsNumber(dtm = dtm_tada,
                                topics = 2:20,
                                return_models = T,
                                control = list(seed = 1234))
FindTopicsNumber_plot(models_tada)

lda_model <- models_tada %>%
  filter(topics == 9) %>%
  pull(LDA_model) %>%
  .[[1]]
lda_model

# 토픽별 주요 단어
term_topic <- tidy(lda_model, matrix = "beta")

term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  print(n = Inf)

stopword_lda <- c("하게", "하다", "하려", "해라", "그것", "하면", "하네", "하기", "하나", "해서", "하면", "하지", "한거", "니들")

top_term_topic <- term_topic %>%
  filter(!term %in% stopword_lda) %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)
top_term_topic

ggplot(top_term_topic, aes(x = reorder_within(term, beta, topic),
                           y = beta,
                           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))

# 문서별 토픽 확률 gamma
doc_topic <- tidy(lda_model, matrix = "gamma")

doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)
doc_class

doc_class$document <- as.integer(doc_class$document)

tada_topic <- tada %>%
  left_join(doc_class, by = c("id" = "document"))

# 토픽별 댓글 수와 단어
top_terms <- term_topic %>%
  filter(!term %in% stopword_lda) %>%
  group_by(topic) %>%
  slice_max(beta, n = 6, with_ties = F) %>%
  summarise(term = paste(term, collapse = " "))
top_terms

count_topic <- tada_topic %>%
  count(topic) %>%
  na.omit()
count_topic

count_topic_word <- count_topic %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic_name = paste("Topic", topic))
count_topic_word

ggplot(count_topic_word,
       aes(x = reorder(topic_name, n),
           y = n,
           fill = topic_name)) +
  geom_col(show.legend = F) +
  coord_flip() +
  geom_text(aes(label = comma(n, accuracy = 1)),
            hjust = -0.2) +
  geom_text(aes(label = term),
            hjust = 1.03,
            col = "white",
            fontface = "bold",
            family = "nanumgothic") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1100)) +
  labs(title = "타다 금지법 기사 댓글 토픽",
       subtitle = "토픽별 주요 단어 및 댓글 빈도",
      x = NULL, y = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))

# 토픽 이름 짓기
reply_topic <- tada_topic %>%
  group_by(topic) %>%
  slice_max(gamma, n = 100)

reply_topic %>%
  filter(topic == 1) %>%
  pull(reply_raw)

reply_topic %>%
  filter(topic == 2) %>%
  head(1) %>%
  pull(reply_raw)

name_topic <- tibble(topic = 1:9,
                     name = c("1. 신사업 가로막는 국회",
                              "2. 시대 흐름 역행하는 법안",
                              "3. 택시 업계 보호, 국민 무시",
                              "4. 자유 시장경제 반하는 결정",
                              "5. 불만족스러운 택시 서비스",
                              "6. 국가 발전 가로막는 정부",
                              "7. 기존 업계 밥그릇 지키는 정치인",
                              "8. 총선만 신경쓰는 국회의원",
                              "9. 타다는 렌트카, 무면허 택시 안된다."))

top_term_topic_name <- top_term_topic %>%
  left_join(name_topic, name_topic, by = "topic")
top_term_topic_name

