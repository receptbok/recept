library(tidyverse)
veckolista <- list(
  df = tribble(
    ~category, ~url,
    "asiatiskt", "shui-zu-yui",
    "asiatiskt", "tieban-tofu",
    "asiatiskt", "jing-jiang-svamp",
    "asiatiskt", "chow-mein",
    "asiatiskt", "zha-jiang-nudlar",
    "vegetariskt", "broken-sushi",
    "vegetariskt", "mexican-rice-bowl",
    "fisk", "sesampanerad-lax-med-ponzumajonnas-och-krispig-sallad",
    "fisk", "https://www.koket.se/thailandsk-lax-i-ugn-med-kokosmjolk-lime-och-ingefara",
    "vegetariskt", "https://www.koket.se/rostad-sotpotatis-med-fetaost-kikarter-och-chilimajonnas",
    "vegetariskt", "https://www.koket.se/chili-sin-carne-siri-barjes-recept",
    "vegetariskt", "https://www.koket.se/burrito-med-stekt-ostronskivling-krispig-kal-och-jalape-oaioli",
    "asiatiskt", "https://www.koket.se/asiatiska-kottbullar-med-kokosris-och-mangosalsa"
  ),
  vecka = 40)
