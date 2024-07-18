
#' Cette fonction permet de calculer la moyenne des itérations .
#'
#'@param SortieIter Un data frame à partir duquel la moyenne des itérations vas être calculer.
#'@return Un data frame avec la moyenne des itérations calculé.
#' @export
#'
#' @examples
#' data <- Sortir_moyenne(SortieIter)


Sortir_moyenne <- function(SortieIter){


  NbIter=length(unique(SortieIter$iter))

  Sortie<-SortieIter %>%
    group_by(id_pe,sdom_bio,type_eco,origine, annee,temps, tbe, pert) %>%
    summarise(hd=sum(hd, na.rm=TRUE)/NbIter,is=sum(hd, na.rm=TRUE)/NbIter,stbop=sum(stbop, na.rm=TRUE)/NbIter,stpeu=sum(stpeu, na.rm=TRUE)/NbIter,
              stft=sum(stft, na.rm=TRUE)/NbIter,stri=sum(stri, na.rm=TRUE)/NbIter,strt=sum(strt, na.rm=TRUE)/NbIter,stsab=sum(stsab, na.rm=TRUE)/NbIter,
              stepn=sum(stepn, na.rm=TRUE)/NbIter,stepx=sum(stepx, na.rm=TRUE)/NbIter,sttot=sum(sttot, na.rm=TRUE)/NbIter,nbop=sum(nbop, na.rm=TRUE)/NbIter,
              npeu=sum(npeu, na.rm=TRUE)/NbIter,nft=sum(nft, na.rm=TRUE)/NbIter,nri=sum(nri, na.rm=TRUE)/NbIter,nrt=sum(nrt, na.rm=TRUE)/NbIter,
              nsab=sum(nsab, na.rm=TRUE)/NbIter,nepn=sum(nepn, na.rm=TRUE)/NbIter,nepx=sum(nepx, na.rm=TRUE)/NbIter,ntot=sum(ntot, na.rm=TRUE)/NbIter,
              vbop=sum(vbop, na.rm=TRUE)/NbIter,vpeu=sum(vpeu, na.rm=TRUE)/NbIter,vft=sum(vft, na.rm=TRUE)/NbIter,vri=sum(vri, na.rm=TRUE)/NbIter,
              vrt=sum(vrt, na.rm=TRUE)/NbIter,vsab=sum(vsab, na.rm=TRUE)/NbIter,vepn=sum(vepn, na.rm=TRUE)/NbIter,vepx=sum(vepx, na.rm=TRUE)/NbIter,
              vtot=sum(vtot, na.rm=TRUE)/NbIter,pct_bop=sum(pct_bop, na.rm=TRUE)/NbIter,pct_peu=sum(pct_peu, na.rm=TRUE)/NbIter,
              pct_ft=sum(pct_ft, na.rm=TRUE)/NbIter,pct_ri=sum(pct_ri, na.rm=TRUE)/NbIter,pct_rt=sum(pct_rt, na.rm=TRUE)/NbIter,
              pct_sab=sum(pct_sab, na.rm=TRUE)/NbIter,pct_epn=sum(pct_epn, na.rm=TRUE)/NbIter,pct_epx=sum(pct_epx, na.rm=TRUE)/NbIter,
              dqrt=ifelse(pct_rt>0,sum(dqrt, na.rm=TRUE)/NbIter,NA),dqft=ifelse(pct_ft>0,sum(dqft, na.rm=TRUE)/NbIter,NA),
              dqri=ifelse(pct_ri>0,sum(dqri, na.rm=TRUE)/NbIter,NA),dqepn=ifelse(pct_epn>0,sum(dqepn, na.rm=TRUE)/NbIter,NA),
              dqepx=ifelse(pct_epx>0,sum(dqepx, na.rm=TRUE)/NbIter,NA),dqsab=ifelse(pct_sab>0,sum(dqsab, na.rm=TRUE)/NbIter,NA),
              dqbop=ifelse(pct_bop>0,sum(dqbop, na.rm=TRUE)/NbIter,NA),dqpeu=ifelse(pct_peu>0,sum(dqpeu, na.rm=TRUE)/NbIter,NA),
              dqtot=ifelse(vtot>0,sum(dqtot, na.rm=TRUE)/NbIter,NA))

  return(Sortie)

}
