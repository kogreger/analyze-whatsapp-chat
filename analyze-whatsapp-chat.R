## before execution make sure to remove emoji from WhatsApp chat export. This can be 
## achieved e.g. in Notepad++ using the following regular expression (replace by ""):
## [\x{D83C}-\x{DBFF}\x{DC00}-\x{DFFF}]+
## (cf. https://stackoverflow.com/questions/24840667/what-is-the-regex-to-extract-all-the-emojis-from-a-string)


## load necessary libraries
library(dplyr)
library(readr)
library(tidytext)
library(sentiment)

## set path for file locations
path <- "C:/Users/kgreger/Downloads/WhatsApp Chat - Test"

## function to parse the information from WhatsApp chat export 
## (cf. https://stat.ethz.ch/R-manual/R-devel/library/base/html/grep.html)
parse.whatsapp <- function(res, 
                           result) {
  m <- do.call(rbind, 
               lapply(seq_along(res), 
                      function(i) {
                        if(result[i] == -1) return("")
                        st <- attr(result, 
                                   "capture.start")[i, ]
                        substring(res[i], 
                                  st, 
                                  st + attr(result, 
                                            "capture.length")[i, ] - 1)
                      }))
  colnames(m) <- attr(result, 
                      "capture.names")
  m
}



## load and parse WhatsApp chat export text file
whatsapp <- read_file(paste(path, 
                            "_chat.txt", 
                            sep = "/")) %>% 
  strsplit("\r\n") %>% 
  unlist() %>% 
  # remove line breaks in messages
  gsub("\\n", 
       "", 
       .) %>% 
  # parse and split messages into tibble
  parse.whatsapp(regexpr("\\[(?<date>\\d{2}\\.\\d{2}\\.\\d{2}), (?<time>\\d{2}:\\d{2}:\\d{2})\\] (?<sender>.+?): (?<message>.*)", 
                         ., 
                         perl = TRUE)) %>% 
  as_tibble() %>% 
  slice(2:n()) %>% 
  # convert to timestamp
  mutate(timestamp = as.POSIXct(strptime(paste(date, time), 
                                         format ="%d.%m.%y %H:%M:%S"))) %>% 
  select(timestamp, sender, message) %>% 
  # remove messages missing timestamps
  filter(!is.na(timestamp)) %>% 
  # parse text or included media
  mutate(text = ifelse(!grepl("\\d{4}-\\d{2}-\\d{2}-(PHOTO|VIDEO|AUDIO)-\\d{8}\\.(jpg|mp4|opus) <.+?>", message), 
                       message, 
                       ""), 
         media = ifelse(!grepl("\\d{4}-\\d{2}-\\d{2}-(PHOTO|VIDEO|AUDIO)-\\d{8}\\.(jpg|mp4|opus) <.+?>", message), 
                        "", 
                        gsub("(\\d{4}-\\d{2}-\\d{2}-(PHOTO|VIDEO|AUDIO)-\\d{8}\\.(jpg|mp4|opus)) <.+?>", "\\1", message))) %>% 
  # remove sent images/videos/audio (version with included media)
  #filter(!grepl("\\d{4}-\\d{2}-\\d{2}-(PHOTO|VIDEO|AUDIO)-\\d{8}\\.(jpg|mp4|opus) <.+?>", message)) %>% 
  # remove sent images/videos/audio (version without included media)
  filter(!grepl("<(image|video|audio) omitted>", message)) %>% 
  select(timestamp, sender, text, media) %>% 
  # add unique message identifier
  mutate(msgid = row_number(), 
         polarity = get_polarity(text), 
         sentiment = get_emotion(text))


## export line-by-line version of WhatsApp chat
write_csv(whatsapp, 
          paste(path, 
                "chat.txt", 
                sep = "/"))


## prepare stopword dictionary (i.e. remove duplicates from multiple lexicons)
stop_words <- stop_words %>% 
  group_by(word) %>% 
  summarize() %>% 
  mutate(stopword = TRUE)


## tokenize WhatsApp chat for text analysis
token <- whatsapp %>% 
  select(-media) %>% 
  unnest_tokens(word, 
                text) %>% 
  # add unique token identifier
  mutate(tokenid = row_number(), 
         polarity = get_polarity(word), 
         sentiment = get_emotion(word)) %>% 
  # add stopword classifier
  left_join(stop_words, 
            by = "word")


## export tokenized version of WhatsApp chat
write_csv(token, 
          paste(path, 
                "token.txt", 
                sep = "/"))


## extract bigrams from WhatsApp chat
bigrams <- whatsapp %>% 
  select(-sentiment, -polarity, -media) %>% 
  unnest_tokens(bigram, 
                text, 
                token = "ngrams", 
                n = 2)


## export bigram version of WhatsApp chat
write_csv(bigrams, 
          paste(path, 
                "bigram.txt", 
                sep = "/"))
