# Krippendorf uses a same setup as the kappa so we'll use the same farmer set-up.
# It doesn't appear to use the same set up, so I don't think that it can be done......
# for intrinsic motivation, applied to a more general test structure
col.ja <- 3
col.ct <- 4
indic <- d[,c(col.ja,col.ct)]

for(i in 1:nrow(indic)){
  if(is.na(indic[i,1] == TRUE)) {indic[i,1] <- 0}
  if(is.na(indic[i,2] == TRUE)) {indic[i,2] <- 0}
}
?sort.list


kap.indic <- kripp.alpha(indic, "nominal")
kappa.scores[2] <- kap.indic$value  # 0.609