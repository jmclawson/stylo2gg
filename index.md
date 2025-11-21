# stylo2gg

Visualize and explore stylo data using ggplot2.

## Installation

Using the remotes package, install stylo2gg with the following command:

`{r} remotes::install_github("jmclawson/stylo2gg")`

## Use

Pipe the output from `stylo()` into
[`stylo2gg()`](https://jmclawson.github.io/stylo2gg/reference/stylo2gg.md),
or save it as an object reused by the
[`stylo2gg()`](https://jmclawson.github.io/stylo2gg/reference/stylo2gg.md)
function:

\`\`\`{r} \# pipe it directly stylo() \|\> stylo2gg

# or save it as an object to use and re-use later

my_data \<- stylo() stylo2gg(my_data) \`\`\`

## Further details

For more explanation on use, see the [introductory blog
post](https://jmclawson.net/posts/introducing-stylo2gg/), the function
[reference
pages](https://jmclawson.github.io/stylo2gg/reference/index.html) or the
[package website](https://jmclawson.github.io/stylo2gg).
