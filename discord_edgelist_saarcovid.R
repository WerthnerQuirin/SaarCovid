# discordant edgelist

discord_edgelist_saarcovid <- function(dat, nw = 1) {
  
  status <- dat$attr$status
  type <- dat$attr$type
  el <- dat$el[[nw]]
  
  del <- NULL
  if (nrow(el) > 0) {
    el <- el[sample(1:nrow(el)), , drop = FALSE]
    stat <- matrix(status[el], ncol = 2)
    isInf <- matrix(stat %in% "i", ncol = 2) # for more matrix(stat %in% c("a", "i"), ncol = 2)
    isSus <- matrix(stat %in% "s", ncol = 2)
    SIpairs <- el[isSus[, 1] * isInf[, 2] == 1, , drop = FALSE]
    ISpairs <- el[isSus[, 2] * isInf[, 1] == 1, , drop = FALSE]
    pairs <- rbind(SIpairs, ISpairs[, 2:1])
    if (nrow(pairs) > 0) {
      sus <- pairs[, 1]
      inf <- pairs[, 2]
      del <- data.frame(sus = sus, inf = inf, stat = status[inf])
    }
  }
  
  return(del)
}