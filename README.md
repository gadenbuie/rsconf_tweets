# `rstudio::conf_tweets()`

A quick fix for your [rstudio::conf][rstudio-conf] FOMO.
Try it on shinyapps.io: <https://gadenbuie.shinyapps.io/rsconf_tweets/>

[![](screenshot.png)](https://gadenbuie.shinyapps.io/rsconf_tweets/)


## Run this on your own machine

To run this on your own, you need to create an OAuth twitter token for [`rtweet`][rtweet].
I followed Bob Rudis's ([hrbrmstr](https://twitter.com/hrbrmstr)) excellent guide from [21 Recipes for Mining Twitter with rtweet](https://rud.is/books/21-recipes/).

To have this app recognize your twitter PAT, you have a couple options:

1. You can follow all of the steps in [Using OAuth to Access Twitter APIs](https://rud.is/books/21-recipes/using-oauth-to-access-twitter-apis.html), 

2. You can save your `twitter_token` to `rtweet.rds` in the app directory

3. You can code up another alternative by setting `.TWITTER_PAT` in a file called `twitter_secrets.R`.

(The first one is the best answer.)

## Other rstudio::conf twitter fun

[\@kearneymw](https://twitter.com/kearneymw) posted a script that collects tweets and makes awesome plots at <https://github.com/mkearney/rstudioconf_tweets>

[rstudio-conf]: https://www.rstudio.com/conference/
[rtweet]: http://rtweet.info/