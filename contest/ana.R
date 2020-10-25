rm(list=ls(all=T))
if(!require(pacman)) install.packages("pacman")
pacman::p_load(httr, tidyverse, rjson, base64url, digest,reticulate)
# Sys.getenv("MAX_API_KEY") # Sys.getenv("MAX_API_SECRET")
key = Sys.getenv("MAX_API_KEY")
tmp = Sys.getenv("MAX_API_SECRET")
### Python encode
use_condaenv("cloud",conda = "D:\\app\\Anaconda3\\condabin\\conda")
repl_python()
import base64
import hashlib
import hmac
import json
from time import time as _time
import requests
nonce = int(round(_time() * 1000))
body = {
  'path': f'/api/v2/members/me.json',
  'nonce': nonce,
}
headers = {
  'Accept': 'application/json',
  'User-Agent': 'pyCryptoTrader/1.0.2',
}
bd = json.dumps(body)
payload =  base64.urlsafe_b64encode(json.dumps(body).encode('utf-8')).decode('utf-8')
sign = hmac.new(bytes(r.tmp, 'utf-8'), bytes(payload, 'utf-8'), hashlib.sha256).hexdigest()
headers.update({
  # This header is REQUIRED to send JSON data.
  # or you have to send PLAIN form data instead.
  'Content-Type': 'application/json',
  'X-MAX-ACCESSKEY': r.key,
  'X-MAX-PAYLOAD': payload,
  'X-MAX-SIGNATURE': sign
})
res = requests.get("https://max-api.maicoin.com/api/v2/members/me.json",headers=headers)
exit

### test payload and sign is correct
payload <- paste0(base64_urlencode(py$bd),"=")
sign <- hmac(tmp, payload, "sha256")
payload == py$payload; sign == py$sign
body <- data.frame(path='/api/v2/members/me.json', 
                   nonce= py$nonce) %>% rjson::toJSON()
py$bd == body
py$res$content
# =============header 製作=================
### Body
gettime <- function(){
 round( (Sys.time() %>% as.numeric())*1000 )
}
body <- data.frame(path="/api/v2/members/me.json", nonce= gettime()) %>% 
  jsonlite::toJSON() %>% 
  str_replace_all("[\\[\\]]", '')
body <- paste0('{"path": "/api/v2/members/me.json", "nonce":',gettime(),'}')
payload <- paste0(base64_urlencode(body),"=")
sign <- hmac(Sys.getenv("MAX_API_SECRET"), payload, "sha256")

# ===============================================================
r <- httr::GET("https://max-api.maicoin.com/api/v2/members/me.json",
               body=body,
               add_headers(.headers=c('Accept'= 'application/json',
                                          'User-Agent'= 'pyCryptoTrader/1.0.2',
                                          'Content-Type'= 'application/json',
                                          'X-MAX-ACCESSKEY'= Sys.getenv("MAX_API_KEY"),
                                          'X-MAX-PAYLOAD'= payload,
                                          'X-MAX-SIGNATURE'= sign)))
r$status_code; content(r)
