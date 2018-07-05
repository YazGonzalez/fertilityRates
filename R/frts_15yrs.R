frts_15yrs<- function(y.intvw, m.wmn,
                     y.wmn, m.child, y.child, children,
                     child.dummy, wmn.dummy, id.wmn,
                     weights, data, ...){
  if(is.data.frame(data)){

      attach(data)
      database <- data.frame(m.wmn, y.wmn,
                             m.child, y.child,
                             children, child.dummy,
                             wmn.dummy, id.wmn, weights)


      age.wmn <- y.intvw-1-y.wmn-1
      age2.wmn <- y.intvw-1-y.wmn
      database$age.wmn <- age.wmn
      database$age2.wmn <- age2.wmn
      database$expo1 <- 0
      database$expo2 <- 0

      for (i in 1:dim(database)[1]) {

        if(database$wmn.dummy[i]==TRUE){
          database$expo1[i] <- ((database$m.wmn[i]-0.5)/12)
          database$expo2[i] <- (1-database$expo1[i])
        }

      }

      database$expo1 <- database$expo1 * database$weights
      database$expo2 <- database$expo2 * database$weights


      auxiliary <- function(age, age.wmn, age2.wmn, exposition1, exposition2){
        aux <- grep(age, age.wmn, value=FALSE)
        s1 <- sum(exposition1[aux])
        aux2 <- grep(age, age2.wmn, value=FALSE)
        s2 <- sum(exposition2[aux2])
        val <- s1+s2
        return(val)
      }

      exposition <- matrix(rep(0,560), nrow = 40, byrow = T )
      temp <- (y.intvw - 14):(y.intvw - 1)
      colnames(exposition) <- as.character(temp)
      temp2 <- 15:54
      rownames(exposition) <- as.character(temp2)


      for (i in c(15:54)) {
        exposition[i-14,14] <- auxiliary(i, database$age.wmn, database$age2.wmn, database$expo1, database$expo2)
      }

      for(i in 40:1){
        for(j in 14:1){
          exposition[i-1,j-1] <- exposition[i,j]
        }
      }

      database$expo1 <- NULL
      database$expo2 <- NULL

      gqe<-as.matrix(rbind(apply(exposition[c(1:5),c(1:14)],2,sum),
                                    apply(exposition[c(6:10),c(1:14)],2,sum),
                                    apply(exposition[c(11:15),c(1:14)],2,sum),
                                    apply(exposition[c(16:20),c(1:14)],2,sum),
                                    apply(exposition[c(21:25),c(1:14)],2,sum),
                                    apply(exposition[c(26:30),c(1:14)],2,sum),
                                    apply(exposition[c(31:35),c(1:14)],2,sum)))


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

      auxiliary_2 <- function(date, age, data){
        aux <- grep(date, data[ ,'y.child'],value=FALSE)
        aux2 <- data[aux, c('weights', 'age.mother')]
        aux3<-grep(age,aux2[,2],value=FALSE)
        aux4<-aux2[aux3,1:2]
        aux5<-rep(aux4[,2],aux4[,1])
        return(length(aux5))
      }


      birth <- matrix(rep(0,560),nrow = 40, byrow = T )
      temp <- (y.intvw - 14):(y.intvw - 1)
      colnames(birth) <- as.character(temp)
      temp2 <- 15:54
      rownames(birth) <- as.character(temp2)


      for (j in (y.intvw - 14):(y.intvw - 1) ) {

        for (i in c(15:54)) {

          birth[i-14, j-(y.intvw - 15)]<- auxiliary_2(j, i, database)
        }
      }

      gqb<-as.matrix(rbind(apply(birth[c(1:5),c(1:14)],2,sum),
                           apply(birth[c(6:10),c(1:14)],2,sum),
                           apply(birth[c(11:15),c(1:14)],2,sum),
                           apply(birth[c(16:20),c(1:14)],2,sum),
                           apply(birth[c(21:25),c(1:14)],2,sum),
                           apply(birth[c(26:30),c(1:14)],2,sum),
                           apply(birth[c(31:35),c(1:14)],2,sum)))


      value <- list(expo = exposition, birth = birth, gqe = gqe, gqb = gqb, y.intvw = y.intvw)

      attr(value, 'class') <- 'frts_15yrs'
      value




} else {
  stop('There is not some data.frame')

}
  }





summary.frts_15yrs <- function(x, ...){

  rgp <- x$gqb/x$gqe

  m1 <-c(mean(rgp[7, 13:14]), mean(rgp[7, 12:13]), mean(rgp[7, 11:12]))
  m2 <-c(mean(rgp[6, 13:14]), mean(rgp[6, 12:13]), mean(rgp[6, 11:12]), mean(rgp[6, 10:11 ]), mean(rgp[6, 9:10]), mean(rgp[6, 8:9]), mean(rgp[6, 7:8]), mean(rgp[6, 6:7]))


  est1=c(rgp[7, 11],rep(sd(m1),10))
  est1=rev(cumsum(est1))
  est2=c(rgp[6, 6],rep(sd(m2),5))
  est2=rev(cumsum(est2))

  rgp[7, 1:11] <- est1
  rgp[6, 1:6] <- est2


  #X=15,20, 25,...,45,50
  #X=50 TGF
  FX <-function(x, year, db){
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
    fx1 <- exp(-exp(vx))*TGFQ(year, db)
    return(fx1)
  }


  disaggregate <- function(auxfx1,auxfx2){
    f1x1 <- auxfx2-auxfx1 #la tasa corresponde al a??o de auxfx1
    return(f1x1)
  }

  rts_dis <- matrix(0, nrow = 35, ncol = 14)

  for(j in (x$y.intvw-14):(x$y.intvw-1)){
    for(i in 15:49){
      rts_dis[i-14,j-(x$y.intvw-15)]<- disaggregate(auxFX(VX2(i, j, rgp), j, rgp), auxFX(VX2(i+1, j, rgp), j, rgp))
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




  value <- list(as_frs = as_frs, t_frs = t_frs, ag_frs = rgp)
  value

}



