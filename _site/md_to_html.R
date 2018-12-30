library(tidyverse)
library(markdown)
temp <- function(z) {
  filename <- z[1]
  tag <- z[2]
  outname <- z[3]
  template <- paste(readLines(con = "post_template.html"), collapse = "\n")
  x <- markdownToHTML(file = paste0("./recipes/", filename), fragment.only = TRUE)
  y <- stringr::str_replace(template, "<!--recipe-->", x)
  y <- stringr::str_replace_all(y, "<!--tag-->", tag)
  writeLines(y, sprintf("%s.html", outname))
  return(c(filename, tag, outname, gsub(".*<h1>|</h1>.*", "", x)))
}

categories <- function(filename, title) {
  paste0('<p><a href="', filename, '.html" title="">', title, '</a></p>')
}
files <- list.files("./recipes", recursive = TRUE)
filenames <- cbind(files, str_split(files, "[///.]", simplify = TRUE)[, 1:2, drop = FALSE])
condensed <- apply(filenames, 1, temp) %>%
  t() %>%
  as_tibble() %>%
  mutate(link = categories(V2, V3)) %>%
  arrange(V3)

condensed2 <- condensed %>%
  group_by(V1) %>%
  summarize(string = paste(link, collapse = "\n"))

apply(condensed2, 1, function(z) {
  filename <- z[1]
  string <- z[2]
  template <- paste(readLines(con = "category_template.html"), collapse = "\n")
  y <- stringr::str_replace(template, "<!--recipe-->", string)
  writeLines(y, sprintf("%s.html", filename))
})

condensed3 <- condensed %>%
  summarize(string = paste(link, collapse = "\n"))
template <- paste(readLines(con = "category_template.html"), collapse = "\n")
y <- stringr::str_replace(template, "<!--recipe-->", condensed3$string)
writeLines(y, sprintf("%s.html", "allarecept"))
  