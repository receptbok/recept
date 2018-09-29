#!/bin/bash 
echo -e "Creating weekly files and scraping \n"
Rscript scrape.R

echo -e "Updating HTML files from md files \n"
Rscript md_to_html.R

echo -e "Uploading to ftp \n"
duck --username wb965681 --password kokbokshemsida123@ --upload ftp://privat.bahnhof.se/ ~/"Box Sync"/Recept/ --existing overwrite --assumeyes