
#' Cette fonction permet de calculer la moyenne des itérations par placette/temps.
#'
#'@param SortieIter Un data frame à partir duquel la moyenne des itérations sera calculée.
#'@return Un data frame avec la moyenne des itérations calculée.
#' @export

Sortir_moyenne <- function(SortieIter){

  # NbIter=length(unique(SortieIter$iter))

  # Sortie <- SortieIter %>%
  #   group_by(id_pe, sdom_bio, type_eco, origine, annee, temps, tbe, pert) %>%
  #   summarise(hd=sum(hd, na.rm=TRUE)/NbIter,is=sum(hd, na.rm=TRUE)/NbIter,stbop=sum(stbop, na.rm=TRUE)/NbIter,stpeu=sum(stpeu, na.rm=TRUE)/NbIter,
  #             stft=sum(stft, na.rm=TRUE)/NbIter,stri=sum(stri, na.rm=TRUE)/NbIter,strt=sum(strt, na.rm=TRUE)/NbIter,stsab=sum(stsab, na.rm=TRUE)/NbIter,
  #             stepn=sum(stepn, na.rm=TRUE)/NbIter,stepx=sum(stepx, na.rm=TRUE)/NbIter,sttot=sum(sttot, na.rm=TRUE)/NbIter,nbop=sum(nbop, na.rm=TRUE)/NbIter,
  #             npeu=sum(npeu, na.rm=TRUE)/NbIter,nft=sum(nft, na.rm=TRUE)/NbIter,nri=sum(nri, na.rm=TRUE)/NbIter,nrt=sum(nrt, na.rm=TRUE)/NbIter,
  #             nsab=sum(nsab, na.rm=TRUE)/NbIter,nepn=sum(nepn, na.rm=TRUE)/NbIter,nepx=sum(nepx, na.rm=TRUE)/NbIter,ntot=sum(ntot, na.rm=TRUE)/NbIter,
  #             vbop=sum(vbop, na.rm=TRUE)/NbIter,vpeu=sum(vpeu, na.rm=TRUE)/NbIter,vft=sum(vft, na.rm=TRUE)/NbIter,vri=sum(vri, na.rm=TRUE)/NbIter,
  #             vrt=sum(vrt, na.rm=TRUE)/NbIter,vsab=sum(vsab, na.rm=TRUE)/NbIter,vepn=sum(vepn, na.rm=TRUE)/NbIter,vepx=sum(vepx, na.rm=TRUE)/NbIter,
  #             vtot=sum(vtot, na.rm=TRUE)/NbIter,pct_bop=sum(pct_bop, na.rm=TRUE)/NbIter,pct_peu=sum(pct_peu, na.rm=TRUE)/NbIter,
  #             pct_ft=sum(pct_ft, na.rm=TRUE)/NbIter,pct_ri=sum(pct_ri, na.rm=TRUE)/NbIter,pct_rt=sum(pct_rt, na.rm=TRUE)/NbIter,
  #             pct_sab=sum(pct_sab, na.rm=TRUE)/NbIter,pct_epn=sum(pct_epn, na.rm=TRUE)/NbIter,pct_epx=sum(pct_epx, na.rm=TRUE)/NbIter,
  #             dqrt=ifelse(pct_rt>0,sum(dqrt, na.rm=TRUE)/NbIter,NA),dqft=ifelse(pct_ft>0,sum(dqft, na.rm=TRUE)/NbIter,NA),
  #             dqri=ifelse(pct_ri>0,sum(dqri, na.rm=TRUE)/NbIter,NA),dqepn=ifelse(pct_epn>0,sum(dqepn, na.rm=TRUE)/NbIter,NA),
  #             dqepx=ifelse(pct_epx>0,sum(dqepx, na.rm=TRUE)/NbIter,NA),dqsab=ifelse(pct_sab>0,sum(dqsab, na.rm=TRUE)/NbIter,NA),
  #             dqbop=ifelse(pct_bop>0,sum(dqbop, na.rm=TRUE)/NbIter,NA),dqpeu=ifelse(pct_peu>0,sum(dqpeu, na.rm=TRUE)/NbIter,NA),
  #             dqtot=ifelse(vtot>0,sum(dqtot, na.rm=TRUE)/NbIter,NA))

  # il est préférable de prendre la fct mean au lieu de sum/NbIter, car s'il y a des données manquantes, ça ne fera pas la bonne moyenne
  # aussi pour les DQ, on ne calcule pas directement leur moyenne, on le recalcule avec la moyenne des N et des ST, même chose pour pct
  Sortie <- SortieIter %>%
    group_by(id_pe, sdom_bio, type_eco, origine, annee, temps, tbe, pert, message) %>%
    summarise(
              hd=mean(hd, na.rm=TRUE), is=mean(hd, na.rm=TRUE),

              stbop=mean(stbop, na.rm=TRUE), stpeu=mean(stpeu, na.rm=TRUE), stft=mean(stft, na.rm=TRUE), stri=mean(stri, na.rm=TRUE),
              strt=mean(strt, na.rm=TRUE), stsab=mean(stsab, na.rm=TRUE), stepn=mean(stepn, na.rm=TRUE), stepx=mean(stepx, na.rm=TRUE),
              sttot=mean(sttot, na.rm=TRUE),

              nbop=mean(nbop, na.rm=TRUE), npeu=mean(npeu, na.rm=TRUE), nft=mean(nft, na.rm=TRUE), nri=mean(nri, na.rm=TRUE),
              nrt=mean(nrt, na.rm=TRUE), nsab=mean(nsab, na.rm=TRUE), nepn=mean(nepn, na.rm=TRUE), nepx=mean(nepx, na.rm=TRUE),
              ntot=mean(ntot, na.rm=TRUE),

              vbop=mean(vbop, na.rm=TRUE), vpeu=mean(vpeu, na.rm=TRUE), vft=mean(vft, na.rm=TRUE), vri=mean(vri, na.rm=TRUE),
              vrt=mean(vrt, na.rm=TRUE), vsab=mean(vsab, na.rm=TRUE), vepn=mean(vepn, na.rm=TRUE), vepx=mean(vepx, na.rm=TRUE),
              vtot=mean(vtot, na.rm=TRUE),

              # on recalcule les pct à partir des moyennes
              pct_bop=stbop/sttot*100, pct_peu=stpeu/sttot*100, pct_ft=stft/sttot*100, pct_ri=stri/sttot*100,
              pct_rt=strt/sttot*100, pct_sab=stsab/sttot*100, pct_epn=stepn/sttot*100, pct_epx=stepx/sttot*100,

              # on recalcule les dq à partir des moyennes
              dqbop = ifelse(nbop>0, sqrt((stbop*40000)/(nbop*pi)), NA),
              dqpeu = ifelse(npeu>0, sqrt((stpeu*40000)/(npeu*pi)), NA),
              dqri = ifelse(nri>0, sqrt((stri*40000)/(nri*pi)), NA),
              dqrt = ifelse(nrt>0, sqrt((strt*40000)/(nrt*pi)), NA),
              dqft = ifelse(nft>0, sqrt((stft*40000)/(nft*pi)), NA),
              dqepn = ifelse(nepn>0, sqrt((stepn*40000)/(nepn*pi)), NA),
              dqepx = ifelse(nepx>0, sqrt((stepx*40000)/(nepx*pi)), NA),
              dqsab = ifelse(nsab>0, sqrt((stsab*40000)/(nsab*pi)), NA),
              dqtot = ifelse(ntot>0, sqrt((sttot*40000)/(ntot*pi)), NA)

              )

  return(Sortie)

}
