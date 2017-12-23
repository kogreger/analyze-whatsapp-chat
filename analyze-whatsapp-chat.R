## before execution make sure to remove emoji from WhatsApp chat export. This can be 
## achieved e.g. in Notepad++ using the following regular expression (replace by ""):
## [\x{D83C}-\x{DBFF}\x{DC00}-\x{DFFF}]+


## load necessary libraries
library(dplyr)
library(readr)
library(tidytext)

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
whatsapp <- read_file("C:/Users/kgreger/Downloads/WhatsApp Chat - Test/_chat.txt") %>% 
  strsplit("\r\n") %>% 
  unlist() %>% 
  # remove line breaks in messages
  gsub("\\n", 
       "", 
       .) %>% 
  # parse and split messages into tibble
  parse.whatsapp(regexpr("(?<date>\\d{2}\\.\\d{2}\\.\\d{2}), (?<time>\\d{2}:\\d{2}:\\d{2}): (?<sender>.+?): (?<text>.*)", 
                         ., 
                         perl = TRUE)) %>% 
  as_tibble() %>% 
  # convert to timestamp
  mutate(timestamp = as.POSIXct(strptime(paste(date, time), 
                                         format ="%d.%m.%y %H:%M:%S"))) %>% 
  select(timestamp, sender, text) %>% 
  # remove messages missing timestamps
  filter(!is.na(timestamp)) %>% 
  # remove sent images/videos
  filter(!grepl("\\d{4}-\\d{2}-\\d{2}-PHOTO-\\d{8}\\.(jpg|mp4) <.+?>", text)) %>% 
  # add unique message identifier
  mutate(msgid = row_number())


## export line-by-line version of WhatsApp chat
write_csv(whatsapp, 
          "C:/Users/kgreger/Downloads/WhatsApp Chat - Test/chat.csv")


## prepare stopword dictionary (i.e. remove duplicates from multiple lexicons)
stop_words <- stop_words %>% 
  group_by(word) %>% 
  summarize() %>% 
  mutate(stopword = TRUE)


## tokenize WhatsApp chat for text analysis
token <- whatsapp %>% 
  unnest_tokens(word, text) %>% 
  # add unique token identifier
  mutate(tokenid = row_number()) %>% 
  # add stopword classifier
  left_join(stop_words, 
            by = "word")


## export tokenized version of WhatsApp chat
write_csv(token, 
          "C:/Users/kgreger/Downloads/WhatsApp Chat - Test/token.csv")
