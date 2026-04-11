# # where the heck does this curl script come from???
# curl_script <- read.delim('scripts/met_download/macav2metdata_curl.sh',
#                                  sep = ' ',
#                                  header = F)

# library(curl)

data_urls <- read.delim('scripts/met_download/macav2metdata_urls.txt',
                        header = F, sep = ' ')

library(stringr)
options(timeout = max(300, getOption('timeout')))
curl_script <- read.delim('scripts/met_download/macav2metdata_urls.txt',
                          sep = '',
                          header = F)
for(yr in 2068:2099){
  for (i in 1:nrow(curl_script)){
    tmp <- paste0('data/maca/', curl_script[i, 1])
    tmp <- str_replace(tmp, '2006',as.character(yr))
    url <- curl_script[i, 1]
    url <- str_replace(url, '2006-01-01',paste0(as.character(yr),'-01-01'))
    url <- str_replace(url, '2006-12-31',paste0(as.character(yr),'-12-31'))
    
    download.file(url, tmp)
    # curl_download(url, tmp, quiet = T, mode = 'wb', handle = new_handle())
    print(paste0(i, ' of ', nrow(curl_script)))
  }
  print(yr)
}

library(terra)
v1 <- rast('data/maca/v1.nc')
