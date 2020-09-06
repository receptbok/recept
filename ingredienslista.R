library(tidyverse)
hamta_inkopslista <- function(category, url) {
  mdfil <- readLines(sprintf("recipes/%s/%s.md", category, url))
  ingrediens_rad <- mdfil %>%
    sapply(function(x) x == "## Ingredienser") %>%
    which
  instruktion_rad <- mdfil %>%
    sapply(function(x) x == "## Instruktioner") %>%
    which
  
  mdfil_kort <- mdfil[(ingrediens_rad+2):(instruktion_rad-2)] %>%
    gsub("\\*\\s", "", .)
  mdfil_kort <- mdfil_kort[mdfil_kort != ""]
  mdfil_kort <- mdfil_kort[!str_detect(mdfil_kort, "###")]
  mdfil_kort <- str_remove_all(mdfil_kort, "ca ")
  
  antal <- str_extract(mdfil_kort, "^[0-9,/\\s-]+")
  antal[is.na(antal)] <- ""
  enhet <- str_extract(mdfil_kort, " påse | ask | port | g | kg | hg | l | dl | ml | tsk | msk | cm | st | knippe | förp | klyftor | kruka")
  enhet[is.na(enhet)] <- ""
  ingrediens <- str_remove(mdfil_kort, paste(str_trim(antal), enhet, sep = ""))
  ingrediens[is.na(ingrediens)] <- mdfil_kort[is.na(ingrediens)]
  
  data.frame(antal, enhet, ingrediens)
}

out <- purrr::pmap_dfr(veckolista$df, hamta_inkopslista)
out <- mutate(out, 
              antal = str_replace_all(antal, "1/2", "0,5"),
              antal = as.numeric(str_replace_all(antal, ",", ".")),
              ingrediens = str_trim(ingrediens))
exportera <- group_by(out, ingrediens, enhet) %>%
  summarize(antal = sum(as.numeric(antal))) %>%
  select(antal, enhet, ingrediens) %>%
  ungroup()

write_excel_csv(exportera, "handlingslista.csv")

View(exportera)
