# Edges correct, variations on net.mod.simnet
# adjust the edge coefficiants to preserve the mean degree over the course of a dynamic network 
# simulation

edges_correct_saarcovid <- function(dat, at) {
  old.num <- dat$epi$num[at - 1]
  new.num <- sum(dat$attr$active == 1)
  
  for (i in 1:length(dat$nwparam)) {
    coef.form1 <- get_nwparam(dat, network = i)$coef.form
    coef.form1[1] <- coef.form1[1] + log(old.num) - log(new.num)
    dat$nwparam[[i]]$coef.form <- coef.form1
  }
  return(dat)
}