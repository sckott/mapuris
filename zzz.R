no_na <- function(x) x[!is.na(x)]
pick <- function(x) if(is.null(x) || length(x)==0) NA else x

# x <- get_xml("10.12688/f1000research.2-191.v2")
# extract_figs(x)
get_xml <- function(x, ...){
#   id <- sub("\\.", "/", sub("10.12688/f1000research.", "", x))
  res <- GET(x, ...)
  tt <- content(res, as="text")
  try_ <- tryCatch(xmlParse(tt), error=function(e) e)
  if(is(try_, "simpleError")) NA else try_
}

extract_figs <- function(x){
  xpathSApply(x, "//graphic[@xlink:href]", xmlGetAttr, name="xlink:href")
}

extract_media <- function(x){
  xpathSApply(x, "//media", xmlGetAttr, name="xlink:href")
}

# out <- get_xml("10.12688/f1000research.5706.2")
# extract_figs(out)
# extract_media(out)

make_url <- function(x, ext="xml"){
  id <- sub("\\.", "/", sub("[0-9\\.]+/f1000research.", "", x))
  sprintf("http://f1000research.com/articles/%s/%s", id, ext)
}

make_entry <- function(cr, cm, fm){
  list(
    doi=cr$DOI,
    url=cr$URL,
    pdf=make_url(cr$DOI, "pdf"),
    xml=make_url(cr$DOI, "xml"),
#     http://f1000research.com/articles/3-133/v2
    cm_target_doi=cm$target_doi,
    cm_assertions=cm$assertions,
    figs=pick(fm$figs),
    media=pick(fm$media)
  )
}

# cross_mark("10.12688/f1000research.2-191.v2")
cross_mark <- function(doi, ...){
  url <- paste0(cmurl(), doi)
  res <- GET(url, ...)
  tt <- content(res, as="text")
  out <- tryCatch(jsonlite::fromJSON(tt), error=function(e) e)
  if(is(out, "simpleError")) NA else out
}
cmurl <- function() "http://doi.crossref.org/crossmark?doi="
