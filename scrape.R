library(rvest)
library(tidyverse)
library(stringr)
library(jsonlite)

scrape_coop <- function(url, category) {
  webpage <- read_html(url)
  title <- webpage %>% 
    html_nodes("h1") %>% 
    html_text() %>% 
    unique() %>% 
    .[1]
  
  instructions <- html_nodes(webpage,"ol") %>%
    html_text() %>% 
    unique() %>%
    paste(collapse = " ") %>%
    str_replace_all("\\r", "") %>%
    str_replace_all("\\n", "*") %>%
    str_trim() %>%
    gsub("\\s+", " ", .) %>%
    str_replace_all("\\*", "\n\\*") %>%
    sub("\\n\\*$", "", .) %>%
    paste0("* ", .)
  
  ingredienslista <- list(namn = c(), ingredienser = list())
  ingredienshtml <- webpage %>% 
    html_nodes("[class='List List--section']")
  
  for (i in 1:length(ingredienshtml)) {
    namn <- ingredienshtml %>%
      .[i] %>%
      html_nodes("[class='List-heading u-textLarge u-textWeightBold']") %>%
      html_text()
    if (length(namn)==0) {
      namn <- " "
    }
    ingredienslista$namn <- c(ingredienslista$namn, namn)
    ingredienslista$ingredienser[i] <- ingredienshtml %>%
      .[i] %>%
      html_nodes("[class='u-paddingHxsm u-textNormal u-colorBase']") %>%
      html_text() %>%
      gsub("\\s+", " ", .) %>%
      str_trim() %>%
      paste(collapse = "\n* ") %>%
      paste0("* ", .)
  }
  idx <- !(duplicated(ingredienslista$namn) & duplicated(ingredienslista$ingredienser))
  ingredienslista$namn <- ingredienslista$namn[idx]
  ingredienslista$ingredienser <- ingredienslista$ingredienser[idx]
  
  ingredients <- c()
  for (i in 1:length(ingredienslista$namn)) {
    if (ingredienslista$namn[i] != " ") {
      if (i>1) {
        collapsedingred <- paste0("\n### ", ingredienslista$namn[i], "\n", ingredienslista$ingredienser[i])
      } else {
        collapsedingred <- paste0("### ", ingredienslista$namn[i], "\n", ingredienslista$ingredienser[i])
      }
    } else {
      collapsedingred <- ingredienslista$ingredienser[i]
    }
    ingredients <- c(ingredients, collapsedingred)
  }
  ingredients <- paste(ingredients, collapse = "\n")

  # 
  # ingredients <- webpage %>% 
  #   html_nodes("[class='List List--section']") %>%
  #   html_nodes("[class='u-paddingHxsm u-textNormal u-colorBase']") %>%
  #   html_text() %>%
  #   gsub("\\s+", " ", .) %>%
  #   str_trim() %>%
  #   .[1:(length(.)/2)] %>%
  #   paste(collapse = "\n* ") %>%
  #   paste0("* ", .)
  
  source <- paste0("Källa: [Coop](", url, ")")
  
  linkname <- title %>%
    str_replace_all("[åäÅÄ]", "a") %>%
    str_replace_all("[öÖ]", "o") %>%
    str_replace_all("[éè]", "e") %>%
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
    html_nodes(".article__body") %>%
    html_nodes("p") 

  ingredients <- ingredients_temp %>%
    magrittr::extract(2) %>%
    str_split("<br>", simplify = TRUE) %>%
    c() %>%
    str_replace_all("<p>|</p>", "") %>%
    str_trim() %>%
    gsub("½", "0,5", .) %>%
    paste(collapse = "\n* ") %>%
    paste("*", .)

  candidates <- website %>%
    html_nodes(".article__body") %>%
    html_nodes("p")

  selected <- website %>%
    html_nodes(".article__body") %>%
    html_nodes("p") %>%
    html_text() %>%
    str_count("^[0-9]\\.\\s")

  instructions <- candidates[which(selected == 1)] %>%
    html_text() %>%
    str_replace_all("^[0-9]\\.\\s", "") %>%
    paste(collapse = "\n* ") %>%
    paste0("* ", .)

  title <- website %>%
    html_nodes(".article__title-container") %>%
    html_nodes(xpath = '//*[@class="article__title"]') %>% 
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

  # title <- read_html(url) %>%
  #   html_nodes(".recipe-column-wrapper") %>%
  #   html_nodes("h1") %>% 
  #   html_text()
  
  # instructions <- read_html(url) %>%
  #   html_nodes(".recipe-column-wrapper") %>%
  #   html_nodes(xpath = '//*[@id="step-by-step"]') %>%
  #   html_nodes(xpath = '//*[@class="step-by-step"]') %>% 
  #   html_nodes(xpath = '//*[@itemprop="recipeInstructions"]') %>% 
  #   xml_children %>% 
  #   as.character() %>% 
  #   gsub("<li>|</li>|</span>|<span>|\n|<br>|<b>", "", .) %>% 
  #   gsub("</b>", ": ", .) %>%
  #   paste(collapse = "\n* ") %>%
  #   paste0("* ", .)
  
  title <- read_html(url) %>%
    html_nodes("h1") %>%
    html_text() %>%
    .[1]
  
  instructions <- read_html(url) %>%
    html_nodes(xpath = '//*[@id="step-by-step"]') %>%
    xml_children %>% 
    as.character() %>% 
    .[2]%>% 
    gsub("<[^>]+>", "",.)%>%
    gsub("\\\n", "\\\n* ",.)
  instructions <- gsub("^\\n", "", instructions)
  instructions <- gsub("\\n\\*$", "", instructions)
  


  ingredients <- read_html(url) %>%
    html_nodes(".ingredients_ingredientList__1_0ii") %>%
    html_nodes(xpath = '//*[@id="ingredients"]') %>%
    html_nodes(xpath = '//*[@class="ingredient"]') %>% 
    xml_children %>% 
    as.character() %>% 
    gsub("<[^>]+>", "",.) %>% 
    stringr::str_trim() %>%
    paste(collapse = "\n* ") %>%
    paste("*", .)
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
             "\n\n## Ingredienser\n\n",
             ingredients,
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
  coop <- str_count(url, "www.coop.se")

  if (ica == 1) {
    temp <- scrape_ica(url, category)
  }

  if (koket == 1) {
    temp <- scrape_koket(url, category)
  }

  if (dn == 1) {
    temp <- scrape_dn(url, category)
  }

  if (coop == 1) {
    temp <- scrape_coop(url, category)
  }
  
  if (ica == 0 && koket == 0 && dn == 0 && coop == 0) {
    title <- gsub("^#\\s", "", readLines(sprintf("recipes/%s/%s.md", category, url))[1])
    temp <- sprintf('<a href="recipes/%s/%s.html" title="">%s</a>', category, url, title)
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
  vecko <- list.files("./veckor", full.names = TRUE) %>%
    sort(decreasing = TRUE)
  most_recent <- which.max(map_dbl(vecko, function(x) file.info(x)$mtime))
  file_name <- sprintf("./veckosedlar/vecka%d.md", as.numeric(gsub("[^0-9]", "", vecko)[most_recent]))
  #temp <- c()
  #for (i in 1:length(vecko)) {
  #  inf <- file.info(vecko[i])
  #  x <- readLines(con = vecko[i])
  #  temp <- c(temp, "\n\n", x)
  #}
  #temp <- paste(temp, collapse = "\n")
  temp <- readLines(con = file_name)
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
    cat("Scraping", veckofiler[i], "\n")
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
  writeLines(paste0("# ", stringr::str_to_title(gsub("\\./recipes/", "", kategorier[i])), "<br/><br/>\n\n",
                    paste(temp, collapse = "<br/>")), sprintf("%s.md", gsub("\\./recipes/", "", kategorier[i])))
}

