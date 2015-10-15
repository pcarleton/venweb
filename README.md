6 Degrees of Venmo
==========

This project pulls data from public Venmo transactions into a force directed graph for the user to explore and play with.

The server side of the app is written in Haskell and deployed to Heroku.  A huge thanks is in order for @mietek.  Without [halycon](https://halcyon.sh/) and his [Haskell on Heroku](https://github.com/mietek/haskell-on-heroku) I am not sure if I would have ever gotten this deployed.

The frontend of the app uses BackboneJS and d3js.

### Installation

If you want to run this application locally, you'll want to first install halcyon:

```sh
$ eval "$( curl -sL https://github.com/mietek/halcyon/raw/master/setup.sh )"
```

(You can also install from source if you don't trust `eval` on random shell scripts from the internet.  See https://halcyon.sh/ for more.)

Next, install the app with:
```sh
$ halcyon install https://github.com/pcarleton/scraper-hs
```

You should then be able to run the app by calling `scraper-hs` from the command line.

If you want to deploy our very own Haskell app, I suggest following @mietek's tutorial.

### Why Haskell?

This project started out as a way for me to try Haskell in a (quasi) practical setting.  Previously, I had used Haskell for programming problems.  I really loved the functional style (it was punishing at first, but it grew on me), but I had never deployed anything with it.

I took inspiration from this blog post: http://adit.io/posts/2012-03-10-building_a_concurrent_web_scraper_with_haskell.html.  An initial version of would concurrently traverse the transaction graph to build up the network in a single backend request, but I took this behavior out in order to simplify the API.
