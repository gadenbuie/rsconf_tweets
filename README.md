# `rstudio::conf_tweets()`

A quick fix for your [rstudio::conf][rstudio-conf] FOMO.
Try it on [shinyapps.io](https://gadenbuie.shinyapps.io/rsconf_tweets/) or [run it yourself](#run-this-on-your-own-machine)!

[![](screenshot.png)](https://gadenbuie.shinyapps.io/rsconf_tweets/)


## Run this on your own machine

To run this on your own, you need to create an OAuth twitter token for [`rtweet`][rtweet].
I followed Bob Rudis's ([hrbrmstr](https://twitter.com/hrbrmstr)) excellent guide from [21 Recipes for Mining Twitter with rtweet](https://rud.is/books/21-recipes/).

To have this app recognize your twitter PAT, you have a couple options:

1. You can follow all of the steps in [Using OAuth to Access Twitter APIs](https://rud.is/books/21-recipes/using-oauth-to-access-twitter-apis.html), 

2. You can save your `twitter_token` to `rtweet.rds` in the app directory

3. You can code up another alternative by setting `.TWITTER_PAT` in a file called `twitter_secrets.R`.

(The first one is the best answer.)

### Required packages

I used the following packages to make this, all of which can all be installed from CRAN:

```r
packages = c("shiny", "rtweet", "dplyr", "stringr",
             "purrr", "httr", "DT", "shinythemes", 
             "glue", "simpleCache")
install.packages(packages)
```

## How it works

I used a simple twitter search for anything tagged or related to `rstudioconf`.
Ininitally I was just looking at anything tagged `#rstudioconf`, but I borrowed the search terms from [Michael Kearney](https://github.com/mkearney/rstudioconf_tweets).

```r
rstudioconf <- c("rstudioconf", "rstudio::conf",
  "rstudioconference", "rstudioconference18",
  "rstudioconference2018", "rstudio18",
  "rstudioconf18", "rstudioconf2018",
  "rstudio::conf18", "rstudio::conf2018")
```

The app is set to pull in new tweets if the app was last loaded more than 15 minutes ago.
To get new tweets, just reload!

Check out [`init.r`](init.R) for more details.

## Other rstudio::conf twitter fun

[\@kearneymw](https://twitter.com/kearneymw) posted a script that collects tweets and makes awesome plots at <https://github.com/mkearney/rstudioconf_tweets>

[rstudio-conf]: https://www.rstudio.com/conference/
[rtweet]: http://rtweet.info/