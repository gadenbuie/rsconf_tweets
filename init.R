library(rtweet)
library(dplyr)
library(stringr)
library(glue)
library(purrr)
library(simpleCache)

if (!dir.exists('data')) system('mkdir -p data')
setCacheDir('data')

cacheFile <- 'data/rsconf_tweets.rds'

# This should go in ~/.Renviron but...
# .TWITTER_APP="rtweet_things"
# .TWITTER_CONSUMER_KEY="xxx"
# .TWITTER_CONSUMER_SECRET="xxx"
# .TWITTER_PAT="./rtweet.rds"

# Twitter token created with...
# twitter_token <- create_token(
#   app = .TWITTER_APP,
#   consumer_key = .TWITTER_CONSUMER_KEY,
#   consumer_secret = .TWITTER_CONSUMER_SECRET
# )
# saveRDS(twitter_token, .TWITTER_PAT)

get_new_tweets <- function() {
  source('twitter_secrets.R')
  twitter_token <- readRDS(.TWITTER_PAT)
  
  tip_words <- "(TIL|DYK|[Tt]ip|[Ll]earned|[Uu]seful|[Kk]now|[Tt]rick)"
  session_words <- "([Aa]vailable|[Oo]nline|[Ll]ink|[Ss]lide|[Ss]ession)"
  rstudio_conf_search <- c("rstudioconf", "rstudio::conf",
                           "rstudioconference", "rstudioconference18",
                           "rstudioconference2018", "rstudio18",
                           "rstudioconf18", "rstudioconf2018",
                           "rstudio::conf18", "rstudio::conf2018")
  
  rsconf_tweets <- search_tweets2(q = rstudio_conf_search, token = twitter_token, n = 1e5)
  rsconf_tweets %>% 
    mutate(
      relates_tip = str_detect(text, tip_words),
      relates_session = str_detect(text, session_words),
      relates_github = str_detect(text, "[Gg]it[Hh]ub") | str_detect(urls_expanded_url, 'github.com'),
      hashtags = map(hashtags, tolower)
    )
}

needs_pulled <- FALSE
if (file.exists(cacheFile)) {
  cacheTime = file.info(cacheFile)$ctime
  cacheAge = difftime(Sys.time(), cacheTime, units="min")
  initRAge = difftime(Sys.time(), file.info('init.R')$ctime, units = 'min')
  needs_pulled <- as.numeric(cacheAge) > 15 | as.numeric(initRAge) < as.numeric(cacheAge)
  rsconf_tweets <- readRDS(cacheFile)
} else {
  rsconf_tweets <- NULL
  needs_pulled <- TRUE
}

if (needs_pulled) {
  rsconf_tweets <- get_new_tweets() %>% 
    filter(!status_id %in% rsconf_tweets$status_id) %>% 
    bind_rows(rsconf_tweets, .) %>% 
    arrange(desc(created_at))
  saveRDS(rsconf_tweets, "data/rsconf_tweets.rds")
}

simpleCache('top_10_hashtags', {
  rsconf_tweets %>% 
    filter(!is.na(hashtags)) %>% 
    pull(hashtags) %>% 
    unlist %>% 
    data_frame(`Top 10 Hashtags` = .) %>% 
    filter(!`Top 10 Hashtags` %in% c('rstudioconf', 'rstudio')) %>%
    group_by(`Top 10 Hashtags`) %>% 
    tally(sort = TRUE) %>% 
    top_n(10, n) %>% select(-n)
}, recreate = needs_pulled)

simpleCache('related_hashtags', {
  rsconf_tweets %>% 
    filter(map_lgl(hashtags, function(hl) length(intersect(hl, top_10_hashtags$`Top 10 Hashtags`)) > 0)) %>% 
    pull(hashtags) %>% 
    map_dfr(function(hs) {
      x <- map(seq_along(hs), function(i) c(paste(hs[i], hs[i:length(hs)]), paste(hs[i:length(hs)], hs[i])))
      data_frame(tags = unlist(x))
    }) %>% 
    arrange(tags) %>% 
    filter(!duplicated(tags)) %>% 
    mutate(
      tags = str_split(tags, ' '),
      tag = map_chr(tags, ~ .[1]),
      related = map_chr(tags, ~ .[2])
    ) %>% 
    select(-tags) %>% 
    filter(tag != related, 
           tag %in% top_10_hashtags$`Top 10 Hashtags`,
           related %in% top_10_hashtags$`Top 10 Hashtags`)
}, recreate = needs_pulled)
