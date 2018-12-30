#!/bin/bash 
echo -e "Creating weekly files and scraping \n"
Rscript scrape.R

echo -e "Uploading to GitHub \n"
git add .
git commit -m "Added new week"
git push -u origin master