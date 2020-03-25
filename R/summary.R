#' Summarize the data of year of interview in fertility rates
#'
#' Summarize the data of year of interview in age-specific fertility rates
#' by single year of age and grouped year of age, and total fertility rate.
#'
#' @param object An object of class "frts_intvw".
#' @param level The confidence level required to confidence intervals for fertility rates.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return An object of class list. The elements of this are "as_fr_s", "t_fr" and "as_fr_g".
#'
#' @examples
#'
#' ## Information from ENADID 2014, INEGI
#'
#' mg1 <- frts_intvw(m.intvw=ENTREV_M, y.intvw=2014, m.wmn=FEC_MUJ_M, y.wmn=FEC_MUJ_A,
#'  age.wmn=EDAD_M, m.child=FEC_HIJ_M, y.child=FEC_HIJ_A, wmn.dummy=MUJER, id.wmn=ID_1,
#'  ids=UPM, strata=ESTRATO, weights = FACTOR, data = enadid_2014)
#'
#' summary(mg1, level = 0.9)
#'
#' @export
summary.frts_intvw <- function(object, level, ...){
  if (requireNamespace("survey", quietly = TRUE)) {
    if(object$flag == TRUE){
      options(survey.ultimate.cluster=TRUE)
      options(survey.lonely.psu="adjust")}

  val <- c()
  i_ci <- c()
  u_ci <- c()
  variance <- c()

  for (i in 15:49) {
    temp <- paste('exposition_',as.character(i),sep = '')
    rate <- survey::svyratio(~ child.dummy * (y.child == y.intvw & age.mother == i),
                     ~ object$df[,grep(temp, names(object$df), value=TRUE)], object$ds, na.rm=TRUE)
    ci <- stats::confint(rate, level = level, df=survey::degf(object$ds))
    val <- c(val, rate$ratio[2])
    i_ci <- c(i_ci, ci[2])
    u_ci <- c(u_ci, ci[4])
    variance <- c(variance,rate$var[2])
  }

  as_fr <- data.frame(round(val, 4), round(i_ci, 4), round(u_ci, 4), variance)
  names(as_fr) <- c('as_fr_s','l_ci','u_ci','var')
  row.names(as_fr) <- 15:49


  rate <- apply(as_fr,2,sum)[1]
  a <- (1 - level)
  t_fr <- NULL


   theta <- mean(object$ta)
   JKvar <- ((object$a - 1)/(object$a)) * sum(( object$ta- theta)^2)
   se <- sqrt(JKvar)
   t_fr <- round(c(rate, rate - stats::qnorm(1-(a/2)) * se, rate + stats::qnorm(1-a/2) * se), 4)
   names(t_fr) <- c('t_fr','l_ci','u_ci')
   t_fr <- as.data.frame(t_fr)


  val2 <- c()
  i_ci2 <- c()
  u_ci2 <- c()


  for (i in levels(object$df$age.group)[-c(1,9)]) {
    temp <- paste('expo', i, sep = '')
    rate <- survey::svyratio(~ child.dummy * (y.child == y.intvw & age.group == i),
                     ~ object$df[,grep(temp, names(object$df), value=TRUE)], object$ds, na.rm=TRUE)
    ci <- stats::confint(rate, level = level, df=survey::degf(object$ds))
    val2 <- c(val2, rate$ratio[2])
    i_ci2 <- c(i_ci2, ci[2])
    u_ci2 <- c(u_ci2, ci[4])

  }

  ag_fr <- data.frame(round(val2, 4), round(i_ci2, 4), round(u_ci2, 4))
  names(ag_fr) <- c('as_fr_g','l_ci','u_ci')
  row.names(ag_fr) <- c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49')

  as_fr$var <- NULL




  value <- list(as_fr_s = as_fr, t_fr = t_fr, as_fr_g = ag_fr)
  value
  } else {
    stop("Library 'survey' must be installed")
  }
}



#' Summarize the data of last complete years up to four years in fertility rates
#'
#' Summarize the data of last complete years up to four years in age-specific fertility rates
#' by single year of age and grouped year of age, and total fertility rate.
#'
#' @param object An object of class "frts_yrly".
#' @param level The confidence level required to confidence intervals for fertility rates.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return An object of class list. The elements of this are "as_fr_s", "t_fr" and "as_fr_g".
#'
#' @examples
#'
#' ## Information from ENADID 2014, INEGI
#'
#' mg2 <- frts_yrly(m.intvw=ENTREV_M, y.intvw=2014, y.ref=2012, m.wmn=FEC_MUJ_M, y.wmn=FEC_MUJ_A,
#'  age.wmn=EDAD_M, m.child=FEC_HIJ_M, y.child=FEC_HIJ_A, wmn.dummy=MUJER, id.wmn=ID_1,
#'  ids=UPM, strata=ESTRATO, weights = FACTOR, data = enadid_2014)
#'
#' summary(mg2, level = 0.9)

#' @export
summary.frts_yrly <- function(object, level, ...){
  if (requireNamespace("survey", quietly = TRUE)) {
    if(object$flag == TRUE){
      options(survey.ultimate.cluster=TRUE)
      options(survey.lonely.psu="adjust")}


  val <- c()
  i_ci <- c()
  u_ci <- c()
  variance <- c()

  for (i in 15:49) {
    temp <- paste('exposition_',as.character(i),sep = '')
    rate <- survey::svyratio(~ child.dummy * (y.child == object$year & age.mother == i),
                     ~ object$df[,grep(temp, names(object$df), value=TRUE)], object$ds, na.rm=TRUE)
    ci <- stats::confint(rate, level = level, df=survey::degf(object$ds))
    val <- c(val, rate$ratio[2])
    i_ci <- c(i_ci, ci[2])
    u_ci <- c(u_ci, ci[4])
    variance <- c(variance,rate$var[2])

  }
  as_fr <- data.frame(round(val, 4), round(i_ci, 4), round(u_ci, 4), variance)
  names(as_fr) <- c('as_fr_s','l_ci','u_ci','var')
  row.names(as_fr) <- 15:49


  rate <- apply(as_fr,2,sum)[1]
  a <- (1 - level)
  t_fr <- NULL


    theta <- mean(object$ta)
    JKvar <- ((object$a - 1)/(object$a)) * sum(( object$ta- theta)^2)
    se <- sqrt(JKvar)
    t_fr <- round(c(rate, rate - stats::qnorm(1-(a/2)) * se, rate + stats::qnorm(1-a/2) * se), 4)
    names(t_fr) <- c('t_fr','l_ci','u_ci')
    t_fr <- as.data.frame(t_fr)


  val2 <- c()
  i_ci2 <- c()
  u_ci2 <- c()


  for (i in levels(object$df$age.group)[-c(1,9)]) {
    temp <- paste('expo', i, sep = '')
    rate <- survey::svyratio(~ child.dummy * (y.child == object$year & age.group == i),
                     ~ object$df[,grep(temp, names(object$df), value=TRUE)], object$ds, na.rm=TRUE)
    ci <- stats::confint(rate, level = level, df=survey::degf(object$ds))
    val2 <- c(val2, rate$ratio[2])
    i_ci2 <- c(i_ci2, ci[2])
    u_ci2 <- c(u_ci2, ci[4])


  }
  ag_fr <- data.frame(round(val2, 4), round(i_ci2, 4), round(u_ci2, 4))
  names(ag_fr) <- c('as_fr_g','l_ci','u_ci')
  row.names(ag_fr) <- c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49')
  as_fr$var <- NULL




  value <- list(as_fr_s = as_fr, t_fr = t_fr, as_fr_g = ag_fr)
  value
  } else {
    stop("Library 'survey' must be installed")
  }

}

#' Summarize the data of last four years to three-year periods in fertility rates
#'
#' Summarize the data of last four years to three-year periods in age-specific fertility rates
#' by single year of age and grouped year of age, and total fertility rate.
#'
#' @param object An object of class "frts_3yrs".
#' @param level The confidence level required to confidence intervals for fertility rates.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return An object of class list. The elements of this are "as_fr_s", "t_fr" and "as_fr_g".
#'
#' @examples
#'
#' ## Information from ENADID 2014, INEGI
#'
#' mg3 <- frts_3yrs(m.intvw=ENTREV_M , y.intvw=2014, y.first=2013, y.second=2012, y.third=2011,
#'  m.wmn=FEC_MUJ_M, y.wmn=FEC_MUJ_A, age.wmn=EDAD_M, m.child=FEC_HIJ_M, y.child=FEC_HIJ_A,
#'  wmn.dummy=MUJER, id.wmn=ID_1, ids=UPM, strata=ESTRATO, weights = FACTOR, data = enadid_2014)
#'
#' summary(mg3, level = 0.9)
#'
#' @export
summary.frts_3yrs <- function(object,  level, ...){
  if (requireNamespace("survey", quietly = TRUE)) {
    if(object$flag == TRUE){
      options(survey.ultimate.cluster=TRUE)
      options(survey.lonely.psu="adjust")}


  val <- c()
  i_ci <- c()
  u_ci <- c()
  variance <- c()

  for (i in 15:49) {
    temp <- paste('exposition_',as.character(i),sep = '')
    rate <- survey::svyratio(~ child.dummy * ((y.child == object$period[1] | y.child == object$period[2] | y.child == object$period[3])
                                      & age.mother == i),
                     ~ object$df[,grep(temp, names(object$df), value=TRUE)], object$ds, na.rm=TRUE)
    ci <- stats::confint(rate, level = level, df=survey::degf(object$ds))
    val <- c(val, rate$ratio[2])
    i_ci <- c(i_ci, ci[2])
    u_ci <- c(u_ci, ci[4])
    variance <- c(variance,rate$var[2])

  }
  as_fr <- data.frame(round(val, 4), round(i_ci, 4), round(u_ci, 4), variance)
  names(as_fr) <- c('as_fr_s','l_ci','u_ci','var')
  row.names(as_fr) <- 15:49


  rate <- apply(as_fr,2,sum)[1]
  a <- (1 - level)
  t_fr <- NULL

    theta <- mean(object$ta)
    JKvar <- ((object$a - 1)/(object$a)) * sum(( object$ta- theta)^2)
    se <- sqrt(JKvar)
    t_fr <- round(c(rate, rate - stats::qnorm(1-(a/2)) * se, rate + stats::qnorm(1-a/2) * se), 4)
    names(t_fr) <- c('t_fr','l_ci','u_ci')
    t_fr <- as.data.frame(t_fr)


  val2 <- c()
  i_ci2 <- c()
  u_ci2 <- c()


  for (i in levels(object$df$age.group)[-c(1,9)]) {
    temp <- paste('expo', i, sep = '')
    rate <- survey::svyratio(~ child.dummy * ((y.child == object$period[1] | y.child == object$period[2] | y.child == object$period[3])
                                      & age.group == i),
                     ~ object$df[,grep(temp, names(object$df), value=TRUE)], object$ds, na.rm=TRUE)
    ci <- stats::confint(rate, level = level, df=survey::degf(object$ds))
    val2 <- c(val2, rate$ratio[2])
    i_ci2 <- c(i_ci2, ci[2])
    u_ci2 <- c(u_ci2, ci[4])


  }
  ag_fr <- data.frame(round(val2, 4), round(i_ci2, 4), round(u_ci2, 4))
  names(ag_fr) <- c('as_fr_g','l_ci','u_ci')
  row.names(ag_fr) <- c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49')


  as_fr$var <- NULL


  value <- list(as_fr_s = as_fr, t_fr = t_fr, as_fr_g = ag_fr)
  value
  } else {
    stop("Library 'survey' must be installed")
  }

}

#' Summarize the data of last fourteen-years to annual periods in fertility rates
#'
#' Summarize the data of last fourteen-years to annual periods in age-specific fertility rates
#' by single year of age and grouped year of age, and total fertility rate.
#'
#' @param object An object of class "frts_14yrs"
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return An object of class list. The elements of this are "as_fr_s", "t_fr" and "as_fr_g".
#'
#' @examples
#'
#' ## Information from ENADID 2014, INEGI
#'
#' mg4 <- frts_14yrs(m.intvw=ENTREV_M, y.intvw=2014, m.wmn=FEC_MUJ_M, y.wmn=FEC_MUJ_A,
#'  age.wmn=EDAD_M, m.child=FEC_HIJ_M, y.child=FEC_HIJ_A, wmn.dummy=MUJER, id.wmn=ID_1,
#'  weights = FACTOR, data = enadid_2014)
#'
#' summary(mg4)
#'
#' @export
summary.frts_14yrs <- function(object, ...){

  rgp <- object$gqb/object$gqe

  mde_A <- mean(rgp[6, 6:14])
  sde_A <- stats::sd(rgp[6, 6:14])
  A <- stats::rnorm(5, mde_A, sde_A)
  rgp[6, 1:5] <- abs(A)

  mde_B <- mean(rgp[7, 11:14])
  sde_B <- stats::sd(rgp[7, 11:14])
  B <- stats::rnorm(10, mde_B, sde_B)
  rgp[7, 1:10] <- abs(B)

  row.names(rgp) <- c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49')


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


  TGFQ <- function(year, db){
    rtsg <- db[,(dimnames(db)[[2]]==as.character(year))]
    tgf <- 5*sum(rtsg)
    return(tgf)
  }


  VX1 <- function(fx, tgf){
    vx1 <- log(-log( fx/tgf ))
    return(vx1)
  }


  VX2 <- function(y, year, db){
    m <- ((VX1(FX(35, year, db), TGFQ(year, db)) +
             VX1(FX(40, year, db), TGFQ(year, db)) + VX1(FX(45, year, db), TGFQ(year, db))
           - VX1(FX(20, year, db),TGFQ(year, db))- VX1(FX(25, year, db), TGFQ(year, db))
           -VX1(FX(30, year, db), TGFQ(year, db)))/3)/15
    vx2 <- m*(y-25)+(VX1(FX(20, year, db),TGFQ(year, db)) + VX1(FX(25, year, db), TGFQ(year, db))
                     + VX1(FX(30, year, db), TGFQ(year, db)))/3
    return(vx2)
  }


  auxFX <- function(vx, year, db){
    fx1 <- exp(-exp(vx)) * TGFQ(year, db)
    return(fx1)
  }


  disaggregate <- function(auxfx1,auxfx2){
    f1x1 <- auxfx2-auxfx1 #la tasa corresponde al anio de auxfx1
    return(f1x1)
  }

  rts_dis <- matrix(0, nrow = 35, ncol = 14)

  for(j in (object$y.intvw-14):(object$y.intvw-1)){
    for(i in 15:49){
      rts_dis[i-14,j-(object$y.intvw-15)] <- disaggregate(auxFX(VX2(i, j, rgp), j, rgp),
                                                          auxFX(VX2(i+1, j, rgp), j, rgp))
    }
  }

  as_frs <- object$birth/object$expo
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
  t_frs <- as.data.frame(t_frs)



  value <- list(as_fr_s = round(as_frs, 4), t_frs = round(t_frs, 4), as_fr_g = round(rgp,4))
  value

}



