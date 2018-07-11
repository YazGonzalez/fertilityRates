#' @export
summary.frts_intvw <- function(x, level, ...){

  val <- c()
  i_ci <- c()
  u_ci <- c()
  variance <- c()

  for (i in 15:49) {
    temp <- paste('exposition_',as.character(i),sep = '')
    rate <- svyratio(~ child.dummy * (y.child == y.intvw & age.mother == i),
                     ~ x$df[,grep(temp, names(x$df), value=TRUE)], x$ds, na.rm=TRUE)
    ci <- confint(rate, level = level, df=degf(x$ds))
    val <- c(val, rate$ratio[2])
    i_ci <- c(i_ci, ci[2])
    u_ci <- c(u_ci, ci[4])
    variance <- c(variance,rate$var[2])
  }

  as_fr <- data.frame(val, i_ci, u_ci, variance)
  names(as_fr) <- c('as_fr_s','l_ci','u_ci','var')
  row.names(as_fr) <- 15:49


  rate <- apply(as_fr,2,sum)[1]
  se <- sqrt(apply(as_fr,2,sum)[4])
  a <- (1 - level)
  t_fr <- c(rate, rate + qt(a/2, df=degf(x$ds)) * se, rate + qt(1-a/2, df=degf(x$ds)) * se)
  names(t_fr) <- c('t_fr','l_ci','u_ci')


  val2 <- c()
  i_ci2 <- c()
  u_ci2 <- c()
  variance2 <- c()

  for (i in levels(x$df$age.group)[-c(1,9)]) {
    temp <- paste('expo', i, sep = '')
    rate <- svyratio(~ child.dummy * (y.child == y.intvw & age.group == i),
                     ~ x$df[,grep(temp, names(x$df), value=TRUE)], x$ds, na.rm=TRUE)
    ci <- confint(rate, level = level, df=degf(x$ds))
    val2 <- c(val2, rate$ratio[2])
    i_ci2 <- c(i_ci2, ci[2])
    u_ci2 <- c(u_ci2, ci[4])
    variance2 <- c(variance2,rate$var[2])
  }

  ag_fr <- data.frame(val2, i_ci2, u_ci2, variance2)
  names(ag_fr) <- c('as_fr_g','l_ci','u_ci','var')
  row.names(ag_fr) <- c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49')

  value <- list(as_fr_s = as_fr, t_fr = t_fr, as_fr_g = ag_fr)
  value
}



#' @export
summary.frts_yrly <- function(x, y.ref, level, ...){

  val <- c()
  i_ci <- c()
  u_ci <- c()
  variance <- c()

  for (i in 15:49) {
    temp <- paste('exposition_',as.character(i),sep = '')
    rate <- svyratio(~ child.dummy * (y.child == y.ref & age.mother == i),
                     ~ x$df[,grep(temp, names(x$df), value=TRUE)], x$ds, na.rm=TRUE)
    ci <- confint(rate, level = level, df=degf(x$ds))
    val <- c(val, rate$ratio[2])
    i_ci <- c(i_ci, ci[2])
    u_ci <- c(u_ci, ci[4])
    variance <- c(variance,rate$var[2])

  }
  as_fr <- data.frame(val, i_ci, u_ci, variance)
  names(as_fr) <- c('as_fr_s','l_ci','u_ci','var')
  row.names(as_fr) <- 15:49


  rate <- apply(as_fr,2,sum)[1]
  se <- sqrt(apply(as_fr,2,sum)[4])
  a <- (1 - level)
  t_fr <- c(rate, rate + qt(a/2, df=degf(x$ds)) * se, rate + qt(1-a/2, df=degf(x$ds)) * se)
  names(t_fr) <- c('t_fr','l_ci','u_ci')


  val2 <- c()
  i_ci2 <- c()
  u_ci2 <- c()
  variance2 <- c()

  for (i in levels(x$df$age.group)[-c(1,9)]) {
    temp <- paste('expo', i, sep = '')
    rate <- svyratio(~ child.dummy * (y.child == y.ref & age.group == i),
                     ~ x$df[,grep(temp, names(x$df), value=TRUE)], x$ds, na.rm=TRUE)
    ci <- confint(rate, level = level, df=degf(x$ds))
    val2 <- c(val2, rate$ratio[2])
    i_ci2 <- c(i_ci2, ci[2])
    u_ci2 <- c(u_ci2, ci[4])
    variance2 <- c(variance2,rate$var[2])

  }
  ag_fr <- data.frame(val2, i_ci2, u_ci2, variance2)
  names(ag_fr) <- c('as_fr_g','l_ci','u_ci','var')
  row.names(ag_fr) <- c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49')



  value <- list(as_fr_s = as_fr, t_fr = t_fr, as_fr_g = ag_fr)
  value

}



#' @export
summary.frts_3yrs <- function(x,  level, ...){

  val <- c()
  i_ci <- c()
  u_ci <- c()
  variance <- c()

  for (i in 15:49) {
    temp <- paste('exposition_',as.character(i),sep = '')
    rate <- svyratio(~ child.dummy * ((y.child == x$period[1] | y.child == x$period[2] | y.child == x$period[3])
                                      & age.mother == i),
                     ~ x$df[,grep(temp, names(x$df), value=TRUE)], x$ds, na.rm=TRUE)
    ci <- confint(rate, level = level, df=degf(x$ds))
    val <- c(val, rate$ratio[2])
    i_ci <- c(i_ci, ci[2])
    u_ci <- c(u_ci, ci[4])
    variance <- c(variance,rate$var[2])

  }
  as_fr <- data.frame(val, i_ci, u_ci, variance)
  names(as_fr) <- c('as_fr_s','l_ci','u_ci','var')
  row.names(as_fr) <- 15:49


  rate <- apply(as_fr,2,sum)[1]
  se <- sqrt(apply(as_fr,2,sum)[4])
  a <- (1 - level)
  t_fr <- c(rate, rate + qt(a/2, df=degf(x$ds)) * se, rate + qt(1-a/2, df=degf(x$ds)) * se)
  names(t_fr) <- c('t_fr','l_ci','u_ci')


  val2 <- c()
  i_ci2 <- c()
  u_ci2 <- c()
  variance2 <- c()

  for (i in levels(x$df$age.group)[-c(1,9)]) {
    temp <- paste('expo', i, sep = '')
    rate <- svyratio(~ child.dummy * ((y.child == x$period[1] | y.child == x$period[2] | y.child == x$period[3])
                                      & age.group == i),
                     ~ x$df[,grep(temp, names(x$df), value=TRUE)], x$ds, na.rm=TRUE)
    ci <- confint(rate, level = level, df=degf(x$ds))
    val2 <- c(val2, rate$ratio[2])
    i_ci2 <- c(i_ci2, ci[2])
    u_ci2 <- c(u_ci2, ci[4])
    variance2 <- c(variance2,rate$var[2])

  }
  ag_fr <- data.frame(val2, i_ci2, u_ci2, variance2)
  names(ag_fr) <- c('as_fr_g','l_ci','u_ci','var')
  row.names(ag_fr) <- c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49')

  value <- list(as_fr_s = as_fr, t_fr = t_fr, as_fr_g = ag_fr)
  value

}


#' @export
summary.frts_15yrs <- function(x, ...){

  rgp <- x$gqb/x$gqe

  m1 <- c(mean(rgp[7, 13:14]), mean(rgp[7, 12:13]), mean(rgp[7, 11:12]))
  m2 <- c(mean(rgp[6, 13:14]), mean(rgp[6, 12:13]), mean(rgp[6, 11:12]), mean(rgp[6, 10:11 ]), mean(rgp[6, 9:10]), mean(rgp[6, 8:9]), mean(rgp[6, 7:8]), mean(rgp[6, 6:7]))


  est1 <- c(rgp[7, 11],rep(sd(m1),10))
  est1 <- rev(cumsum(est1))
  est2 <- c(rgp[6, 6],rep(sd(m2),5))
  est2 <- rev(cumsum(est2))

  rgp[7, 1:11] <- est1
  rgp[6, 1:6] <- est2


  #X=15,20, 25,...,45,50
  #X=50 TGF
  FX <- function(x, year, db){
    if(x==15){
      fx=NA
    }
    else{
      rtsg <- db[,(dimnames(db)[[2]]==as.character(year))]
      fx <- 5*sum(rtsg[1:((x/5)-3)])
    }
    return(fx)
  }


  TGFQ <- function(year,db){
    rtsg <- db[,(dimnames(db)[[2]]==as.character(year))]
    tgf <- 5*sum(rtsg)
    return(tgf)
  }



  VX1 <- function(fx,tgf){
    vx1 <- log(-log( fx/tgf ))
    return(vx1)
  }


  VX2 <- function(y, year, db){
    m <- ((VX1(FX(35, year, db), TGFQ(year, db)) + VX1(FX(40, year, db), TGFQ(year, db)) + VX1(FX(45, year, db), TGFQ(year, db))- VX1(FX(20, year, db),TGFQ(year, db))- VX1(FX(25, year, db), TGFQ(year, db))-VX1(FX(30, year, db), TGFQ(year, db)))/3)/15
    vx2 <- m*(y-25)+(VX1(FX(20, year, db),TGFQ(year, db)) + VX1(FX(25, year, db), TGFQ(year, db)) + VX1(FX(30, year, db), TGFQ(year, db)))/3
    return(vx2)
  }


  auxFX <- function(vx, year, db){
    fx1 <- exp(-exp(vx)) * TGFQ(year, db)
    return(fx1)
  }


  disaggregate <- function(auxfx1,auxfx2){
    f1x1 <- auxfx2-auxfx1 #la tasa corresponde al a??o de auxfx1
    return(f1x1)
  }

  rts_dis <- matrix(0, nrow = 35, ncol = 14)

  for(j in (x$y.intvw-14):(x$y.intvw-1)){
    for(i in 15:49){
      rts_dis[i-14,j-(x$y.intvw-15)] <- disaggregate(auxFX(VX2(i, j, rgp), j, rgp), auxFX(VX2(i+1, j, rgp), j, rgp))
    }
  }

  as_frs <- x$birth/x$expo
  as_frs <- as_frs[1:35, ]

  for(i in 1:10){
    if(i==1){
      as_frs[25+i, 1]= NA }
    else{
      as_frs[i+25, c(1:i)]= NA
    }
  }


  for(i in 1:14){
    as_frs[(which(is.na(as_frs[,i])==T)),i] <- rts_dis[(which(is.na(as_frs[,i])==T)),i]
  }

  t_frs <- apply(as_frs, 2, sum)


  value <- list(as_fr_s = as_frs, t_frs = t_frs, as_fr_s = rgp)
  value

}



