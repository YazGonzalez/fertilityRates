frts_intvw<- function(m.intvw, y.intvw, m.wmn,
                      y.wmn, m.child, y.child, children,
                      child.dummy, wmn.dummy, id.wmn, ids,
                      strata, weights, data, ...){
  if(is.data.frame(data)){
    if (is.element("survey", installed.packages()[,1])) {
      attach(data)
      database <- data.frame(m.intvw, y.intvw,
                             m.wmn, y.wmn,
                             m.child, y.child,
                             children, child.dummy,
                             wmn.dummy, id.wmn)

      age.wmn <- y.intvw-y.wmn-1
      age2.wmn <- y.intvw-y.wmn
      database$age.wmn <- age.wmn
      database$age2.wmn <- age2.wmn
      database$expo1 <- 0
      database$expo2 <- 0
      database$expo3 <- 0

      for (i in 1:dim(database)[1]) {

        if(database$wmn.dummy[i]==TRUE){
          if(database$m.intvw[i] > database$m.wmn[i]){
            database$expo1[i] <- ((database$m.wmn[i]-0.5)/12)
            database$expo2[i] <- ((database$m.intvw[i]-database$m.wmn[i]-0.5)/12)
          }else{
            database$expo3[i] <- ((database$m.intvw[i]-1)/12)
          }
        }

      }
      database$m.intvw <- NULL


      auxiliary <- function(age, age.wmn, age2.wmn, exposition1, exposition2, exposition3){
        expo1 <- rep(0,length(exposition1))
        aux<-grep(age, age.wmn, value=FALSE)
        expo1[aux] <- exposition1[aux]
        aux2<-grep(age, age2.wmn, value=FALSE)
        expo1[aux2] <- exposition2[aux2]
        expo2 <- rep(0,length(exposition2))
        aux3<-grep(age, age.wmn, value=FALSE)
        expo2[aux3] <- exposition3[aux3]
        expo <- expo1 + expo2
        return(expo)
      }

      nom <- sprintf("exposition_%s", 15:49)
      data <- data.frame(matrix(ncol =(49- 15)+1, nrow = length(database$age.wmn)))
      colnames(data) <- nom
      for(j in 1:((49 - 15)+1)){
        data[,j]<- auxiliary(j+15-1,database$age.wmn,database$age2.wmn,database$expo1,database$expo2,database$expo3)
      }


      database$expo1 <- NULL
      database$expo2 <- NULL
      database$expo3 <- NULL

      database <- cbind(database,data)

      estimate_age <- function(m.child, y.child, m.wmn, y.wmn){
        random <- rbinom(length(m.child),1,0.5)
        age <- NULL
        for(i in 1:length(m.child)){
          if(is.na(m.child[i]) == F){
            if(m.child[i] > m.wmn[i] | m.child[i] < m.wmn[i]){
              age=c(age, ceiling((12*(y.child[i]-y.wmn[i]) + (m.child[i]-m.wmn[i]))/12))
            }
            else{
              age=c(age, ceiling((12*(y.child[i]-y.wmn[i]) + (m.child[i]-m.wmn[i]-rbinom(1,1,0.5)))/12))
            }
          }
          else{age=c(age,0)}
        }
        return(age)
      }

      database$age.mother <- estimate_age(database$m.child, database$y.child, database$m.wmn, database$y.wmn)
      database$age.group <- cut(database$age.mother, c(-1, 14, 19, 24, 29, 34, 39, 44, 49, 60))
      levels(database$age.group) <- c('0_14', '15_19', '20_24', '25_29', '30_34', '35_39', '40_44', '45_49', '50_60')

      database$expo15_19 <- apply(database[,paste('exposition_',15:19, sep = '')],1,sum)
      database$expo20_24 <- apply(database[,paste('exposition_',20:24, sep = '')],1,sum)
      database$expo25_29 <- apply(database[,paste('exposition_',25:29, sep = '')],1,sum)
      database$expo30_34 <- apply(database[,paste('exposition_',30:34, sep = '')],1,sum)
      database$expo35_39 <- apply(database[,paste('exposition_',35:39, sep = '')],1,sum)
      database$expo40_44 <- apply(database[,paste('exposition_',40:44, sep = '')],1,sum)
      database$expo45_49 <- apply(database[,paste('exposition_',45:49, sep = '')],1,sum)

      database$y.wmn <- NULL
      database$m.child <- NULL


      database$ids <- ids
      database$strata <- strata
      database$weights <- weights

      detach(data)
      attach(database)


      ds <- svydesign(id = ~ids,
                      strata = ~strata,
                      weights = ~weights,
                      data = database, nest=TRUE) #JUST NEST

      value <- list(df = database, ds = ds)

      attr(value, 'class') <- 'frts_intvw'
      value


    } else {
      stop("Library 'survey' must be installed")
    }
  } else {
    stop('There is not some data.frame')

  }

}






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
  names(as_fr)<-c('as_fr','l_ci','u_ci','var')
  row.names(as_fr) <- 15:49


  rate=apply(as_fr,2,sum)[1]
  se=sqrt(apply(as_fr,2,sum)[4])
  a <- (1 - level)
  t_fr <- c(rate, rate + qt(a/2, df=degf(x$ds)) * se, rate + qt(1-a/2, df=degf(x$ds)) * se)
  names(t_fr)<-c('t_fr','l_ci','u_ci')


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
  names(ag_fr)<-c('ag_fr','l_ci','u_ci','var')
  row.names(ag_fr) <- c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49')



  value <- list(as_fr = as_fr, t_fr = t_fr, ag_fr = ag_fr)
  value

}

