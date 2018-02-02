library(shiny)
library(rtweet)
library(dplyr)
library(stringr)
library(purrr)
requireNamespace('httr', quietly = TRUE)
requireNamespace('shinythemes', quietly = TRUE)
requireNamespace('DT', quietly = TRUE)
library(glue)
source("init.R")

get_tweet_blockquote <- function(screen_name, status_id) {
  bq <- httr::GET(glue("https://publish.twitter.com/oembed?url=https://twitter.com/{screen_name}/status/{status_id}?omit_script=true")) %>% 
    httr::parsed_content()
  if (is.null(bq$html)) 
    '<blockquote style="font-size: 90%">Sorry, unable to get tweet ¯\\_(ツ)_/¯</blockquote>'
  else bq$html
}

close_to_sd <- function(lat, lng) {
  sandiego <- rtweet::lookup_coords("Sand Diego, CA")$point %>% as_radian
  # from http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates
  
  delta_lat <- 50/3958.76
  delta_lng <- asin(sin(as_radian(lat))/cos(delta_lat))
  lat <- as_radian(lat)
  lng <- as_radian(lng)
  lat >= sandiego['lat'] - delta_lat &
    lat <= sandiego['lat'] + delta_lat &
    lng >= sandiego['lng'] - delta_lng & 
    lng <= sandiego['lng'] + delta_lng
}

as_radian <- function(degree) degree * pi / 180

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src="twitter.js")),
  titlePanel("rstudio::conf_twitter()"),
  theme = shinythemes::shinytheme('yeti'),
  
  
  column(
    width = 4,
    wellPanel(
      selectInput('view', 'Tweet Group', c('Popular', 'Tips', "Talks", "Pictures", "All")),
      uiOutput('help_text'),
      uiOutput('filters')
    ),
    wellPanel(class = 'tweetbox', htmlOutput('tweet')),
    tags$div(class = 'colophon', 
             tagList(
               tags$p(
                 "Made with ❤️ + ☕️ by", tags$a(href = 'https://twitter.com/grrrck/', '@grrrck'),
                 'with 💪 from',
                 HTML(paste(
                   tags$a(href = 'http://rtweet.info/', 'rtweet'),
                   tags$a(href = 'https://www.rstudio.com/', 'RStudio'),
                   tags$a(href = 'https://shiny.rstudio.com/', 'Shiny'),
                   tags$a(href = 'https://www.tidyverse.org/', 'tidyverse'),
                   tags$a(href = 'http://code.databio.org/simpleCache/', 'simpleCache'),
                   sep = ', '
                 ))),
               tags$p(
                 "💾 ", tags$a(href = 'https://github.com/gadenbuie/rsconf_tweets', 'View on GitHub')
               ),
               tags$p(
                 "Updated:", strftime(cacheTime, "%F %T %Z", tz = 'America/New_York')
               )
             )
    )
  ),
  
  column(8, DT::dataTableOutput('tweets'))
)

server <- function(input, output) {
  output$help_text <- renderUI({
    req(input$view)
    switch(
      input$view,
      'Popular' = helpText("💯 Most popular (retweets + favs) first"),
      'Tips' = helpText("💡 Original or quote tweets that mention a tip"),
      'Talks' = helpText("🎓 Original or quote tweets that mention \"slides\", \"presentations\", etc."),
      'Pictures' = helpText("📸 Tweets that come with a picture"),
      'All' = helpText("🤗 All the tweets"),
      NULL
    )
  })
  tweets <- reactive({
    x <- switch(
      input$view,
      'Popular' = rsconf_tweets %>% 
        arrange(desc(retweet_count + favorite_count), 
                -map_int(mentions_screen_name, length)),
      'Tips' = rsconf_tweets %>% filter(relates_tip, !is_retweet),
      'Talks' = rsconf_tweets %>% filter(relates_session, !is_retweet),
      'Pictures' = rsconf_tweets %>% filter(!is_retweet, !is.na(media_url)),
      rsconf_tweets
    ) 
    
    if (input$view %in% c('All', 'Popular')) {
      if (length(input$filter_binary)) {
        for (filter_binary in input$filter_binary) {
          x <- switch(
            filter_binary,
            'Not Retweet' = filter(x, !is_retweet),
            'Not Quote' = filter(x, !is_quote),
            'Has Media' = filter(x, !is.na(media_url)),
            'Has Link' = filter(x, !is.na(urls_url)),
            'Has Github Link' = filter(x, str_detect(urls_url, "github")),
            "Retweeted" = filter(x, retweet_count > 0),
            "Favorited" = filter(x, favorite_count > 0),
            "Probably There IRL" = x %>% lat_lng() %>% 
              filter( 
                str_detect(tolower(place_full_name), "san diego") | 
                  close_to_sd(lat, lng) |
                  user_id %in% users_there_IRL
              )
          )
        }
      }
      if (length(input$filter_hashtag)) {
        x <- filter(x, !is.null(hashtags))
        for (filter_hashtag in input$filter_hashtag) {
          x <- filter(x, map_lgl(hashtags, function(h) filter_hashtag %in% h))
        }
      }
    }
    x
  })
  
  hashtags_related <- reactive({
    req(input$view %in% c('All', 'Popular'))
    if (is.null(input$filter_hashtag) || input$filter_hashtag == '') return(top_10_hashtags)
    limit_to_tags <- related_hashtags %>% 
      filter(tag %in% input$filter_hashtag) %>% 
      pull(related) %>% 
      unique()
    top_10_hashtags %>% 
      filter(`Top 10 Hashtags` %in% c(limit_to_tags, input$filter_hashtag)) %>% 
      pull(`Top 10 Hashtags`)
  })
  
  output$filters <- renderUI({
    selected_hashtags <- isolate(input$filter_hashtag)
    selected_binary <- isolate(input$filter_binary)
    if (input$view %in% c('All', 'Popular')) {
      tagList(
        checkboxGroupInput('filter_binary', 'Tweet Filters', 
                           choices = c("Not Retweet", "Not Quote", "Has Media", "Has Link", "Has Github Link", "Retweeted", "Favorited", "Probably There IRL"), 
                           selected = selected_binary,
                           inline = TRUE),
        selectizeInput('filter_hashtag', 'Hashtags', choices = c("", hashtags_related()), selected = selected_hashtags, 
                       multiple = TRUE, options = list(plugins = list('remove_button')), width = "100%")
      )
    }
  })
  
  output$tweets <- DT::renderDataTable({
    tweets() %>% 
      select(created_at, screen_name, text, retweet_count, favorite_count, mentions_screen_name) %>% 
      mutate(created_at = strftime(created_at, '%F %T', tz = 'US/Pacific'),
             mentions_screen_name = map_chr(mentions_screen_name, paste, collapse = ', '),
             mentions_screen_name = ifelse(mentions_screen_name == 'NA', '', mentions_screen_name))
  },
  selection = 'single', 
  rownames = FALSE, 
  colnames = c("Timestamp", "User", "Tweet", "RT", "Fav", "Mentioned"), 
  filter = 'top',
  options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 5)
  )
  
  output$tweet <- renderText({
    if (!is.null(input$tweets_rows_selected)) {
      tweets() %>% 
        slice(input$tweets_rows_selected) %>% 
        mutate(
          html = suppressWarnings(get_tweet_blockquote(screen_name, status_id))
        ) %>% 
        pull(html)
    } else {
      HTML('<blockquote style="font-size: 90%">Choose a tweet from the table...</blockquote>')
    }
  })
}

shinyApp(ui = ui, server = server)

