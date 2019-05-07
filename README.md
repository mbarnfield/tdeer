# tdeer
`tdeer`, through its function [`tdee()`](https://github.com/mbarnfield/tdeer/blob/master/R/tdee.R), allows you to calculate your total daily energy expenditure in R. In doing so, it tells you how many calories to consume to attain your goal of gaining, losing, or maintaining weight. If you provide a goal weight, it will also run these same calculations for every kilogram interval between your current and goal weight.

This package is pretty pointless, because there are loads of [online calculators](https://tdeecalculator.net) out there which do the job perfectly well within your browser. There are also tonnes of [implementations of this on GitHub](https://github.com/search?p=1&q=tdee&type=Repositories), including one very cool [shiny app](https://jaylillico.shinyapps.io/TDEE/). But if for some reason you want to do a little task like this without leaving RStudio, then `tdeer` is the package for you...   

The calculations in `tdeer` are based on [this Steel Fit USA post](https://steelfitusa.com/2018/10/calculate-tdee/).

