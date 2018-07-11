source("init.R")
if (!dir.exists('data')) system('mkdir -p data')
setCacheDir('data')

cacheFile <- 'data/conf_tweets.rds'

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
if (TWEET_REFRESH_ENABLED) {
  if (file.exists('twitter_secrets.R')) source('twitter_secrets.R')
  if (!exists('.TWITTER_PAT')) {
    .TWITTER_PAT <- Sys.getenv('TWITTER_PAT')
    if (.TWITTER_PAT == "") .TWITTER_PAT <- './rtweet.rds'
  }
  twitter_token <- readRDS(.TWITTER_PAT)
}

get_new_tweets <- function(max_id) {
  tip_words <- "(TIL|DYK|[Tt]ip|[Ll]earned|[Uu]seful|[Kk]now|[Tt]rick)"
  session_words <- "([Aa]vailable|[Oo]nline|[Ll]ink|[Ss]lide|[Ss]ession)"
  conf_search <- c("#user2018", "user2018", "@useR2018_conf", "user2018_conf")
  conf_search <- paste(conf_search, collapse = " OR ")
  
  conf_tweets <- search_tweets(q = conf_search, token = twitter_token, n = 1e5, since_id = max_id)
  conf_tweets %>% 
    mutate(
      relates_tip = str_detect(text, tip_words),
      relates_session = str_detect(text, session_words),
      relates_github = str_detect(text, "[Gg]it[Hh]ub") | str_detect(urls_expanded_url, 'github.com'),
      hashtags = map(hashtags, tolower)
    )
}

needs_pulled <- FALSE
if (file.exists(cacheFile)) {
  cacheTime = file.info(cacheFile)$mtime
  cacheAge = difftime(Sys.time(), cacheTime, units="min")
  initRAge = difftime(Sys.time(), file.info('init.R')$mtime, units = 'min')
  needs_pulled <- (as.numeric(cacheAge) > 15 | as.numeric(initRAge) < as.numeric(cacheAge)) && TWEET_REFRESH_ENABLED
  conf_tweets <- readRDS(cacheFile)
} else {
  conf_tweets <- NULL
  cacheAge <- 0
  needs_pulled <- TRUE
}

if (needs_pulled) {
  max_id <- if (as.numeric(cacheAge) < 60) conf_tweets$status_id
  if (lubridate::minute(lubridate::now()) < 15) max_id <- NULL
  if (!is.null(max_id)) {
    max_id <- max(max_id)
    message("Getting just new tweets, starting with ", max_id)
  }
  new_tweets <- get_new_tweets(max_id)
  if (!is.null(conf_tweets)) {
    conf_tweets <- bind_rows(
      semi_join(new_tweets, conf_tweets, by = 'status_id'), # updates old tweets
      anti_join(conf_tweets, new_tweets, by = 'status_id'), # keeps old tweets
      anti_join(new_tweets, conf_tweets, by = 'status_id')  # adds new tweets
    ) %>% 
      arrange(desc(created_at))
  } else {
    conf_tweets <- arrange(new_tweets, desc(created_at))
  }
  saveRDS(conf_tweets, "data/conf_tweets.rds")
  cacheTime <- Sys.time()
}

simpleCache('top_hashtags', {
  conf_tweets %>% 
    filter(!is.na(hashtags)) %>% 
    pull(hashtags) %>% 
    unlist %>% 
    data_frame(`Top Hashtags` = .) %>% 
    filter(!tolower(`Top Hashtags`) %in% c('user2018')) %>%
    group_by(`Top Hashtags`) %>% 
    tally(sort = TRUE) %>% 
    top_n(25, n) %>% select(-n)
}, recreate = needs_pulled)

simpleCache('related_hashtags', {
  conf_tweets %>% 
    filter(map_lgl(hashtags, function(hl) length(intersect(hl, top_hashtags$`Top Hashtags`)) > 0)) %>% 
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
           tag %in% top_hashtags$`Top Hashtags`,
           related %in% top_hashtags$`Top Hashtags`)
}, recreate = needs_pulled)

simpleCache("user2018_schedule_csv", {
  readr::read_csv(
    "user2018_schedule.csv", 
    col_types = readr::cols(
      Date = readr::col_character(), 
      Time = readr::col_character(), 
      X1   = readr::col_skip(), 
      X2   = readr::col_skip())
  ) %>% 
    mutate(`Date & Time` = paste(Date, Time)) %>% 
    select(-Date, -Time) %>% 
    select(`Date & Time`, everything())
})

simpleCache('user2018_screen_names', {
  user2018_user_search <- user2018_schedule_csv$Presenter %>%
    unique() %>%
    set_names() %>%
    map(search_users, n = 25)
  
  guess_handle <- function(df, queried_name) {
    colnames(df)[70] <- "status_url"
    mutate(
      df, 
      presenter_name = queried_name, 
      fits_profile = !protected & 
        str_detect(tolower(description), "data|rstats|code|stats|statistic(s|ian|al)|computational|modeling|bayes|r program") | 
        (!is.na(name) && name == queried_name & nrow(df) == 1)
    ) %>%
      select(presenter_name, fits_profile, user_id, screen_name, name, location, 
             description, followers_count, friends_count, statuses_count)
  }
  
  user2018_user_search %>%
    keep(~nrow(.) > 0) %>%
    imap_dfr(~guess_handle(.x, .y), .id = "presenter_name") %>% 
    filter(
      fits_profile,
      !screen_name %in% c("RLaytonDawg", "danwwilson", "aidybarnettr") # dopplegangers
    ) %>% 
    select(-fits_profile)
})

simpleCache("user2018_schedule", {
  user2018_screen_names %>% 
    select(
      Presenter = presenter_name, 
      Twitter = screen_name, 
      user_id, followers_count, friends_count) %>% 
    left_join(user2018_schedule_csv, ., by = "Presenter")
})
