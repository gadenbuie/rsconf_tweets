packages = c("shiny", "rtweet", "dplyr", "stringr",
             "purrr", "httr", "DT", "shinythemes", 
             "shinyjs", "glue", "simpleCache")
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    warning("Installing missing package:", pkg)
    install.packages(pkg, dependencies = TRUE)
    require(pkg, character.only = TRUE)
  }
})
TWEET_REFRESH_ENABLED <- TRUE
GITHUB_REPO_URL <- 'https://github.com/gadenbuie/user-2018-tweets'
CONFERENCE_NAME <- "UseR!2018"
CONFERENCE_URL <- "https://user2018.r-project.org/"
