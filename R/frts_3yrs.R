#' Apply fertility rates methodology for last four years to three-year periods
#'
#' Applies a given fertility rates methodology rates
#' case-by-case to a data set and returns another data set
#' with the estimates by observation.
#'
#' @param m.intvw A numeric or a vector which indicates the month in which the woman were interviewed.
#' @param y.intvw A numeric which indicate the year in which the woman were interviewed.
#' @param y.first A numeric which indicate the first year retrospectively of three-year periods.
#' @param y.second A numeric which indicate the second year retrospectively of three-year periods.
#' @param y.third A numeric which indicate third year retrospectively of three-year periods.
#' @param m.wmn A vector specifying the woman’s month of birth (mother or not mother).
#' @param y.wmn A vector specifying the woman’s year of birth (mother or not mother).
#' @param age.wmn A vector specifying the woman's age at the time of the interview (mother or not mother).
#' @param m.child A vector specifying the child’s month of birth (if the woman doesn't have child, NA).
#' @param y.child A vector specifying the child’s year of birth (if the woman doesn't have children, NA).
#' @param wmn.dummy A vector which indicate TRUE if the woman isn't duplicate or FALSE if the woman is duplicate.
#' @param id.wmn A vector wich indicate the woman's identification.
#' @param ids A vector specifying cluster ids from largest level to smallest level, ~0 or ~1 is a formula for no clusters.
#' @param strata A vector specifying strata.
#' @param weights A vetor pecifying sampling weights as an alternative to prob (1/weights).
#' @param data A data frame containing the above variables.
#'
#' @return An object of class frts_3yrs containing a data.frame, a list and a vector.
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
#'
frts_3yrs<- function(m.intvw, y.intvw, y.first, y.second, y.third, m.wmn,
                     y.wmn, age.wmn, m.child, y.child,
                     wmn.dummy, id.wmn, ids,
                     strata, weights, data){

  if(is.data.frame(data)){
    if (requireNamespace("survey", quietly = TRUE)) {

      attach(data)
      db <- data.frame(m.wmn, y.wmn,
                             m.child, y.child,
                             wmn.dummy, id.wmn)

      db$child.dummy <- ifelse(is.na(y.child)==FALSE, 1, 0)


      intvw.age <-  ifelse(m.wmn >= m.intvw, age.wmn,  age.wmn-1)
      age.wmn <- NULL



      age.wmn <- intvw.age -(y.intvw - y.third)
      age2.wmn <- age.wmn + 1

      age3.wmn <- intvw.age -(y.intvw - y.second)
      age4.wmn <- age3.wmn + 1

      age5.wmn <- intvw.age -(y.intvw - y.first)
      age6.wmn <- age5.wmn + 1

      db$age.wmn <- age.wmn
      db$age2.wmn <- age2.wmn
      db$age3.wmn <- age3.wmn
      db$age4.wmn <- age4.wmn
      db$age5.wmn <- age5.wmn
      db$age6.wmn <- age6.wmn
      db$intvw.age <- intvw.age


      db$expo1 <- 0
      db$expo2 <- 0
      db$expo3 <- 0
      db$expo4 <- 0


      for (i in 1:dim(db)[1]) {

        if(db$wmn.dummy[i]==TRUE){
          db$expo1[i] <- ((db$m.wmn[i]-0.5)/12)
          db$expo2[i] <- (1-db$expo1[i])
          db$expo3[i] <- 1
          db$expo4[i] <- 1
        }

      }

      # special databases
      DATSN <- data.frame(YCHILD = db$y.child , DUM = db$child.dummy)
      DATSD <- data.frame(EXPOT = c(db$expo1, db$expo3, db$expo4, db$expo2),
                          AGEW = c(db$age.wmn , db$age2.wmn,
                                   db$age4.wmn, db$age6.wmn))


      auxiliary <- function(age, age.wmn, age2.wmn, exposition1, exposition2){
        expo1 <- rep(0,length(exposition1))
        aux<-grep(age, age.wmn, value=FALSE)
        expo1[aux] <- exposition1[aux]
        aux2<-grep(age, age2.wmn, value=FALSE)
        expo1[aux2] <- exposition2[aux2]

        return(expo1)
      }

      nom <- sprintf("exposition_%s", min(db$age.wmn):max(db$age2.wmn))
      data <- data.frame(matrix(ncol =(max(db$age2.wmn)- min(db$age.wmn))+1, nrow = length(db$age.wmn)))
      colnames(data) <- nom
      for(j in 1:((max(db$age2.wmn)- min(db$age.wmn))+1)){
        data[,j]<- auxiliary(j+min(db$age.wmn)-1,db$age.wmn, db$age2.wmn,db$expo1,db$expo2)
      }

      temp <- rep(0, nrow(data))
      data2 <- cbind(temp, data[ ,-((max(db$age2.wmn)- min(db$age.wmn))+1)])
      data3 <- cbind(temp, data2[ ,-((max(db$age2.wmn)- min(db$age.wmn))+1)])
      data <- data + data2 + data3


      db$expo1 <- NULL
      db$expo2 <- NULL
      db$expo3 <- NULL
      db$expo4 <- NULL


      db <- cbind(db,data)

      estimate_age <- function(m.child, y.child, m.wmn, intvw.age, y.intvw){
        age <- NULL
        for(i in 1:length(m.child)){
          if(is.na(m.child[i]) == F){
            if(m.child[i] > m.wmn[i] | m.child[i] < m.wmn[i]){
              if(m.child[i] > m.wmn[i] ){
                age = c(age, intvw.age[i]-(y.intvw-y.child[i])+1)
              }
              else{
                age = c(age, intvw.age[i]-(y.intvw-y.child[i]))
              }
            }
          else{
            age = c(age, intvw.age[i]-(y.intvw-y.child[i])+stats::rbinom(1,1,0.5))
            }
          }
          else{age = c(age,0)}
        }
        return(age)
      }


      db$age.mother <- estimate_age(db$m.child, db$y.child, db$m.wmn, db$intvw.age, y.intvw)
      db$age.group <- cut(db$age.mother, c(-1, 14, 19, 24, 29, 34, 39, 44, 49, 60))
      levels(db$age.group) <- c('0_14', '15_19', '20_24', '25_29', '30_34', '35_39', '40_44', '45_49', '50_60')

      db$expo15_19 <- apply(db[,paste('exposition_',15:19, sep = '')],1,sum)
      db$expo20_24 <- apply(db[,paste('exposition_',20:24, sep = '')],1,sum)
      db$expo25_29 <- apply(db[,paste('exposition_',25:29, sep = '')],1,sum)
      db$expo30_34 <- apply(db[,paste('exposition_',30:34, sep = '')],1,sum)
      db$expo35_39 <- apply(db[,paste('exposition_',35:39, sep = '')],1,sum)
      db$expo40_44 <- apply(db[,paste('exposition_',40:44, sep = '')],1,sum)
      db$expo45_49 <- apply(db[,paste('exposition_',45:49, sep = '')],1,sum)

      db$y.wmn <- NULL
      db$m.child <- NULL

      DATSN <- cbind(DATSN, AGEM = db$age.mother)

      bandera <- NULL
      if(length(ids) > 2){
        db$ids <- ids
        bandera <- TRUE
        db$strata <- strata
        db$weights <- weights

        detach(data)
        attach(db)
        ds <- survey::svydesign(id = ~ids,
                                strata = ~strata,
                                weights = ~weights,
                                data = db, nest=TRUE)

        DATSN <- cbind(DATSN, WEIG = db$weights, IDT = db$id.wmn)
        IDT <- unique(db$id.wmn)
        clst <- data.frame(IDT, CLUS = round(stats::runif(length(IDT), 1,
                                                   round(length(IDT)/70, 0)), 0))
        rm(IDT)
        DATSN <- merge(DATSN, clst, by = 'IDT')
        rm(clst)
        A <- length(unique(DATSN$CLUS))
        DATSD <- cbind(DATSD, WEIG2 = rep(db$weights, 4),
                       CLUS2 = rep(DATSN$CLUS, 4))


        DATSN <- DATSN[which(DATSN$YCHILD == y.first | DATSN$YCHILD == y.second | DATSN$YCHILD == y.third), -1]
        DATSN <- DATSN[which(DATSN$AGEM >= 15 & DATSN$AGEM <= 49), ]
        numJ <- matrix(15:49, byrow = TRUE)
        DATSD <- DATSD[which(DATSD$AGEW >=15 & DATSD$AGEW <=49), ]
        DATSD <- DATSD[which(DATSD$EXPOT > 0), ]
        denJ <- matrix(15:49, byrow = TRUE)

        for (i in unique(DATSN$CLUS)){
          dbJ <- DATSN[which(!DATSN$CLUS == i), ]
          dbM <- DATSD[which(!DATSD$CLUS2 == i), ]
          num <- stats::aggregate(dbJ$DUM * dbJ$WEIG, list(dbJ$AGEM), sum)$x
          numJ <- cbind(numJ, num)

          den <- stats::aggregate(dbM$EXPOT * dbM$WEIG2, list(dbM$AGEW), sum)$x
          denJ <- cbind(denJ, den)

        }

        tgfa <- apply(numJ[ , -1]/denJ[ , -1], 2, sum)



      } else {
        bandera <- FALSE
        db$strata <- strata
        db$weights <- weights

        detach(data)
        attach(db)
        ds <- survey::svydesign(id = ~1,
                                strata = ~strata,
                                weights = ~weights,
                                data = db, nest=TRUE)

        DATSN <- cbind(DATSN, WEIG = db$weights, IDT = db$id.wmn)
        IDT <- unique(db$id.wmn)
        clst <- data.frame(IDT, CLUS = round(stats::runif(length(IDT), 1,
                                                   round(length(IDT)/70, 0)), 0))
        rm(IDT)
        DATSN <- merge(DATSN, clst, by = 'IDT')
        rm(clst)
        A <- length(unique(DATSN$CLUS))
        DATSD <- cbind(DATSD, WEIG2 = rep(db$weights, 4),
                       CLUS2 = rep(DATSN$CLUS, 4))


        DATSN <- DATSN[which(DATSN$YCHILD == y.first | DATSN$YCHILD == y.second | DATSN$YCHILD == y.third), -1]
        DATSN <- DATSN[which(DATSN$AGEM >= 15 & DATSN$AGEM <= 49), ]
        numJ <- matrix(15:49, byrow = TRUE)
        DATSD <- DATSD[which(DATSD$AGEW >=15 & DATSD$AGEW <=49), ]
        DATSD <- DATSD[which(DATSD$EXPOT > 0), ]
        denJ <- matrix(15:49, byrow = TRUE)

        for (i in unique(DATSN$CLUS)){
          dbJ <- DATSN[which(!DATSN$CLUS == i), ]
          dbM <- DATSD[which(!DATSD$CLUS2 == i), ]
          num <- stats::aggregate(dbJ$DUM * dbJ$WEIG, list(dbJ$AGEM), sum)$x
          numJ <- cbind(numJ, num)

          den <- stats::aggregate(dbM$EXPOT * dbM$WEIG2, list(dbM$AGEW), sum)$x
          denJ <- cbind(denJ, den)

        }

        tgfa <- apply(numJ[ , -1]/denJ[ , -1], 2, sum)

      }


      period <- c(y.first, y.second, y.third)

      value <- list(df = db, ds = ds, period = period, ta = tgfa, a = A, flag = bandera)

      attr(value, 'class') <- 'frts_3yrs'
      value



    } else {
      stop("Library 'survey' must be installed")
    }
  } else {
    stop('There is not some data.frame')

  }


}
