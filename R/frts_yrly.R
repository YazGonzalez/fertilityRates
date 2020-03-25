#' Apply fertility rates methodology for last complete years up to four years
#'
#' Applies a given fertility rates methodology rates
#' case-by-case to a data set and returns another data set
#' with the estimates by observation.
#'
#' @param m.intvw A numeric or a vector which indicates the month in which the woman were interviewed.
#' @param y.intvw A numeric which indicate the year in which the woman were interviewed.
#' @param y.ref A numeric which indicate the reference year. It must be between last four years above interview.
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
#' @return An object of class frts_yrly containing a data.frame, a list and a numeric.
#'
#' @examples
#'
#' ## Information from ENADID 2014, INEGI
#'
#' mg2 <- frts_yrly(m.intvw=ENTREV_M, y.intvw=2014, y.ref=2012, m.wmn=FEC_MUJ_M,
#'  y.wmn=FEC_MUJ_A, age.wmn=EDAD_M, m.child=FEC_HIJ_M, y.child=FEC_HIJ_A,
#'  wmn.dummy=MUJER, id.wmn=ID_1, ids=UPM, strata=ESTRATO, weights = FACTOR,
#'  data = enadid_2014)
#'
#' summary(mg2, level = 0.9)
#'
#' @export
#'

frts_yrly<- function(m.intvw, y.intvw, y.ref, m.wmn, y.wmn, age.wmn,
                      m.child, y.child,
                      wmn.dummy, id.wmn, ids,
                      strata, weights, data){
  if(is.data.frame(data)){
    if (requireNamespace("survey", quietly = TRUE)) {

      attach(data)
      db <- data.frame(m.wmn, y.wmn,
                             m.child, y.child,
                             wmn.dummy, id.wmn)

      db$child.dummy <- ifelse(is.na(y.child)==FALSE, 1, 0)


      intvw.age  <-  ifelse(m.wmn >= m.intvw, age.wmn,  age.wmn-1)
      age.wmn <- NULL

      age.wmn <- intvw.age - (y.intvw-y.ref)
      age2.wmn <- age.wmn + 1 #age2.wmn-(y.intvw-y.ref-1)


      db$age.wmn <- age.wmn
      db$age2.wmn <- age2.wmn
      db$intvw.age <- intvw.age

      db$expo1 <- 0
      db$expo2 <- 0


      for (i in 1:dim(db)[1]) {

        if(db$wmn.dummy[i]==TRUE){
            db$expo1[i] <- ((db$m.wmn[i]-0.5)/12)
            db$expo2[i] <- (1-db$expo1[i])
        }

      }

      # special databases

      DATSN <- data.frame(YCHILD = db$y.child , DUM = db$child.dummy)
      DATSD <- data.frame(EXPOT = c(db$expo1, db$expo2),
                          AGEW = c(db$age.wmn , db$age2.wmn))



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
        data[,j]<- auxiliary(j+min(db$age.wmn)-1,db$age.wmn,db$age2.wmn,db$expo1,db$expo2)
      }


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
              age = c(age, intvw.age[i]-(y.intvw-y.child[i])+ stats::rbinom(1,1,0.5))
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

        DATSN <- cbind(DATSN, CLUS = db$ids, WEIG = db$weights)
        A <- length(unique(DATSN$CLUS))
        DATSD <- cbind(DATSD, CLUS2 = c(db$ids, db$ids),
                       WEIG2 = c(db$weights, db$weights))


        DATSN <- DATSN[which(DATSN$YCHILD==y.ref), -1]
        DATSN <- DATSN[which(DATSN$AGEM >= 15 & DATSN$AGEM <= 49), ]
        numJ <- matrix(15:49, byrow = TRUE)
        DATSD <- DATSD[which(DATSD$AGEW >=15 & DATSD$AGEW <= 49), ]
        DATSD <- DATSD[which(DATSD$EXPOT > 0), ]
        denJ <- matrix(15:49, byrow = TRUE)

        for (i in unique(DATSN$CLUS)){
          dbJ <- DATSN[which(!DATSN$CLUS == i), ]
          num <- stats::aggregate(dbJ$DUM * dbJ$WEIG, list(dbJ$AGEM), sum)$x
          numJ <- cbind(numJ, num)
          dbJ <- DATSD[which(!DATSD$CLUS2 == i), ]
          den <- stats::aggregate(dbJ$EXPOT * dbJ$WEIG2, list(dbJ$AGEW), sum)$x
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
                                                   round(length(IDT)/40, 0)), 0))
        rm(IDT)
        DATSN <- merge(DATSN, clst, by = 'IDT')
        rm(clst)
        A <- length(unique(DATSN$CLUS))
        DATSD <- cbind(DATSD, WEIG2 = c(db$weights, db$weights),
                       CLUS2 = c(DATSN$CLUS, DATSN$CLUS))


        DATSN <- DATSN[which(DATSN$YCHILD==y.ref), -1]
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

      rm(dbJ, DATSN, DATSD)


      value <- list(df = db, ds = ds, year = y.ref, ta = tgfa, a = A, flag = bandera)


      attr(value, 'class') <- 'frts_yrly'
      value



    } else {
      stop("Library 'survey' must be installed")
    }
  } else {
    stop('There is not some data.frame')

  }

}
