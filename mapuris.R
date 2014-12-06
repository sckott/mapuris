library("rplos")
library("rcrossref")
library("dplyr")
library("httr")
library("jsonlite")
library("XML")

# cr_members(query = "f1000")
out <- cr_members(member_ids = 4950, works = TRUE, limit = 100)$data
dois <- out$DOI

mdout <- apply(out, 1, function(x) cross_mark(x['DOI']))

figs_media <- lapply(dois, function(x){
  tmp <- get_xml(make_url(x))
  if(is.na(tmp)){
    list(
      figs=NA,
      media=NA
    )
  } else {
    list(
      figs=extract_figs(tmp),
      media=extract_media(tmp)
    )
  }
})

# F1000 isn't giving links to full text yet...
# cr_full_links("10.12688/f1000research.3817.1")
alldat <- Map(make_entry,
    apply(out, 1, function(x) as.list(x[c('DOI','URL')])),
    lapply(mdout, function(x){
      if(all(is.na(x))){
        list(cm_updates=NA, cm_assertions=NA)
      } else {
        list(target_doi=pick(x$updated_by$doi), assertions=no_na(x$assertions$href))
      }
    }),
    figs_media
)

jsonlite::toJSON(alldat, auto_unbox = TRUE, pretty = TRUE)
