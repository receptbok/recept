library(tidyverse)
veckolista <- list(
  df = tribble(
    ~category, ~url,
    "fisk", "sesampanerad-lax-med-ponzumajonnas-och-krispig-sallad",
    "fisk", "sayadieh---saffransris-med-vit-fisk-och-tahinidressing",
    "fisk", "stekt-fisk-med-sojadressing",
    "fisk", "ugnsbakad-lax-med-fankal-och-paprika",
    "fisk", "nudelsoppa-med-lax-och-rod-curry",
    "vegetariskt", "broken-sushi",
    "vegetariskt", "quinoasallad",
    "vegetariskt", "mexican-rice-bowl",
    "vegetariskt", "heavenly-tofu-poké-bowl",
    "vegetariskt", "sunnyside-bibimbap",
    "vegetariskt", "vegetarisk-lasagne-med-keso",
    "kyckling", "kycklingbulgogi"
  ),
  vecka = 36)
