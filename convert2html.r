require(knitr)
require(markdown)

knit('RLab4_CB.Rmd','RLab4_CB.md')
markdownToHTML('RLab4_CB.md','RLab4_CB.html')