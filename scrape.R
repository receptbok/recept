library(rvest)
library(tidyverse)
library(stringr)
library(jsonlite)

scrape_ica <- function(url, category) {
  webpage <- read_html(url)
  title <- html_nodes(webpage,'.recipepage__headline') %>%
    html_text() %>%
    str_trim()

  instructions <- html_nodes(webpage,'.recipe-howto-steps') %>%
    html_text() %>%
    str_split("För alla", simplify = TRUE) %>%
    .[1,1] %>%
    str_replace_all("\\.([A-z])", "\\.\n\\* \\1") %>%
    sub("^.*?([a-zA-Z])", "\\1", .) %>%
    paste0("* ", .)

  ingredients <- html_nodes(webpage,'.ingredients__list__item') %>%
    html_text() %>%
    paste(collapse = "\n* ") %>%
    paste("*", .)

  source <- paste0("Källa: [ICA](", url, ")")

  linkname <- title %>%
    str_replace_all("[åäÅÄ]", "a") %>%
    str_replace_all("[öÖ]", "o") %>%
    str_replace(",", "") %>%
    tolower() %>%
    str_replace_all("\\s", "-")
  zz <- linkname %>%
    paste0("~/Box Sync/Recept markdown/recipes/", category, "/", ., ".md") %>%
   file()
  cat(paste0("# ", title,
             "\n\n## Ingredienser\n\n", ingredients,
             "\n\n## Instruktioner\n\n", instructions,
             "\n\n ", source), file = zz)
  close(zz)
  title_link <- sprintf('[%s](/recipes/%s/%s.md)', as.character(title), category, as.character(linkname))
  return(title_link)
}

scrape_dn <- function(url, category) {

  website <- read_html(url)

  ingredients_temp <- website %>%
    html_nodes(".factbox-body") %>%
    html_nodes("p")

  if (length(ingredients_temp) <= 3) {
    ingredients <- ingredients_temp %>%
      magrittr::extract(length(ingredients_temp)) %>%
      str_split("<br>", simplify = TRUE) %>%
      c() %>%
      str_replace_all("<p>|</p>", "") %>%
      paste(collapse = "\n* ") %>%
      paste("*", .)
  } else {
    strong_count <- ingredients_temp %>%
      str_count("strong")
    ingredients <- ingredients_temp[strong_count == 0] %>%
      html_text() %>%
      paste(collapse = "\n* ") %>%
      paste("*", .)
  }

  candidates <- website %>%
    html_nodes(".article__body-content") %>%
    html_nodes("p")

  selected <- website %>%
    html_nodes(".article__body-content") %>%
    html_nodes("p") %>%
    html_text() %>%
    str_count("^[0-9]\\.\\s")

  instructions <- candidates[which(selected == 1)] %>%
    html_text() %>%
    str_replace_all("^[0-9]\\.\\s", "") %>%
    paste(collapse = "\n* ") %>%
    paste0("* ", .)

  title <- website %>%
    html_nodes(".article__body-content") %>%
    html_nodes(".title") %>%
    html_text()

  source <- paste0("Källa: [DN](", url, ")")

  linkname <- title %>%
    str_replace_all("[åäÅÄ]", "a") %>%
    str_replace_all("[öÖ]", "o") %>%
    str_replace(",", "") %>%
    tolower() %>%
    str_replace_all("\\s", "-")
  zz <- linkname %>%
    paste0("~/Box Sync/Recept markdown/recipes/", category, "/", ., ".md") %>%
    file()
  cat(paste0("# ", title,
             "\n\n## Ingredienser\n\n", ingredients,
             "\n\n## Instruktioner\n\n", instructions,
             "\n\n ", source), file = zz)
  close(zz)
  title_link <- sprintf('[%s](/recipes/%s/%s.md)', as.character(title), category, as.character(linkname))
  return(title_link)
}
scrape_koket <- function(url, category) {

  temp <- read_html(url) %>%
    html_nodes(".recipe-column-wrapper") %>%
    html_nodes(xpath = '//*[@id="react-recipe-page-wrapper"]') %>%
    magrittr::extract2(1) %>%
    html_attr("data-item") %>%
    fromJSON

  title <- temp$name

  instructions <- temp$cooking_steps %>%
    paste(collapse = "\n* ") %>%
    paste0("* ", .)

  if (str_count(instructions, "<br>") > 0) {instructions <- str_replace_all(instructions, "<br>\n", ": ")}
  if (str_count(instructions, "<b>|</b>") > 0) {
    instructions <- str_replace_all(instructions, "<b>|</b>", "")
  }

  ingredient_all <- temp$ingredients
  ingredients <- c()

  for (i in 1:nrow(ingredient_all)) {
    if (ingredient_all$ingredient[i]) {
      ingredients <- c(ingredients, sprintf("\n* %s %s %s",
                                            ingredient_all$amount[i],
                                            ingredient_all$unit[i],
                                            ingredient_all$name[i]))
    } else {
      ingredients <- c(ingredients, sprintf("\n\n### %s", ingredient_all$name[i]))
    }
  }
  ingredients <- str_replace_all(ingredients, "NA ", "")
  sourcelink <- paste0("Källa: [Köket.se](", url, ")")

  linkname <- title %>%
    str_replace_all("[åäÅÄ]", "a") %>%
    str_replace_all("[öÖ]", "o") %>%
    str_replace(",", "") %>%
    tolower() %>%
    str_replace_all("\\s", "-")
  zz <-  linkname %>%
    paste0("~/Box Sync/Recept markdown/recipes/", category, "/", ., ".md") %>%
    file()
  cat(paste0("# ", title,
             "\n\n## Ingredienser",
             rep("\n", 2-str_count(ingredients[1], "\n")),
             paste(ingredients, collapse = ""),
             "\n\n## Instruktioner\n\n", instructions,
             "\n\n", sourcelink), file = zz)
  close(zz)
  title_link <- sprintf('[%s](/recipes/%s/%s.md)', title, category, linkname)
  return(title_link)
}

scrape <- function(url, category) {

  ica <- str_count(url, "www.ica.se")
  koket <- str_count(url, "www.koket.se")
  dn <- str_count(url, "www.dn.se")

  if (ica == 1) {
    temp <- scrape_ica(url, category)
  }

  if (koket == 1) {
    temp <- scrape_koket(url, category)
  }

  if (dn == 1) {
    temp <- scrape_dn(url, category)
  }

  if (ica == 0 && koket == 0 && dn == 0) {
    title <- gsub("^#\\s", "", readLines(sprintf("recipes/%s/%s.md", category, url))[1])
    temp <- sprintf('<a href="%s.html" title="">%s</a><br>', url, title)
  }
  return(temp)
}

scrape_vec <- Vectorize(scrape)

skapa_enskild_matsedel <- function(vecka, recept) {
 x <- sprintf('## Vecka %d

  %s', vecka, paste(recept$title, collapse = "<br/>"))

   writeLines(x, sprintf("veckosedlar/vecka%d.md", vecka))
}

skapa_index <- function() {
  vecko <- list.files("./veckosedlar", full.names = TRUE) %>%
    sort(decreasing = TRUE)
  temp <- c()
  for (i in 1:length(vecko)) {
    x <- readLines(con = vecko[i])
    temp <- c(temp, "\n\n", x)
  }
  temp <- paste(temp, collapse = "\n")

  writeLines(temp, "README.md")
}

veckodata <- function(veckolista) {
  df <- veckolista$df %>%
    mutate(title = scrape_vec(url, category))

  skapa_enskild_matsedel(veckolista$vecka, df)
}

veckofiler <- list.files("./veckor", full.names = TRUE)

temp <- c()
if (length(veckofiler)>0) {
  for (i in 1:length(veckofiler)) {
    source(veckofiler[i])
    temp <- c(temp, veckolista$vecka)
  }
}


need_to_scrape <- which(file.exists(sprintf("./veckosedlar/vecka%s.md", temp)) == FALSE)
if (length(need_to_scrape)>0) {
  for (i in need_to_scrape) {
    source(veckofiler[i])
    veckodata(veckolista)
  }
}

suppressWarnings(skapa_index())



kategorier <- list.files("./recipes", full.names = TRUE)
for (i in 1:length(kategorier)) {
  recept <- list.files(kategorier[i], full.names = TRUE)
  temp <- rep("a", length(recept))
  for (j in 1:length(recept)) {
    temp[j] <- sprintf("[%s](%s)", 
            gsub("# ", "", readLines(con = list.files(kategorier[i], full.names = TRUE)[j])[1]),
            gsub("\\./", "/", list.files(kategorier[i], full.names = TRUE)[j]))
  }
  writeLines(paste0("# ", stringr::str_to_title(gsub("\\./recipes/", "", kategorier[i])), "<br/><br/>",
                    paste(temp, collapse = "<br/>"), sprintf("%s.md", gsub("\\./recipes/", "", kategorier[i]))))
}

