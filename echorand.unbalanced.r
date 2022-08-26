# real school data, all schools originally consenting included:
schoolframe48 <- 
  data.frame(schoolnr=1:48, school=c(
    "Trasop","Skøyen","Smedstad","Tårnåsen","Tverlandet skole",
    "Glommasvingen","Bestum","Frol","Sørumsand","Vigernes","Finstad",
    "Grünerløkka","Rustad","Skogn","Volla","Kviltorp", "Mørkvedmarka skole",
    "Lakkegata","Oppsal","Halsen","Finnsnes barneskole","Lilleaker",
    "Bøleråsen","Egge","Bodøsjøen skole","Baksalen Skole","Askimbyen",
    "Silsand barneskole","Frosta","Kulstad skole","Andenes skole","Nordbyen",
    "Skatval","Garderåsen","Meråker","Skistua barneskole","Fjordtun Skole",
    "Bell","Sakshaug skole","Fuglenes Skole","Kabelvåg skole","Sørum","Asak",
    "Risøyhamn skole", "Tydal","Selbustrand","Bleik Montessoriskole","Øverbygda"),
    service=c(
      "Østensjø","Ullern","Ullern","Nordre Follo","Bodø","Sør-Odal","Ullern",
      "Levanger", "Lillestrøm","Lillestrøm","Nordre Follo","Grünerløkka",
      "Østensjø","Levanger","Lillestrøm","Molde","Bodø","Grünerløkka",
      "Østensjø","Stjørdal","Lenvik","Ullern","Nordre Follo","Steinkjer",
      "Bodø","Hammerfest","Indre østfold","Lenvik","Frosta","Vefsn/Mosjøen",
      "Andøy","Molde","Stjørdal", "Lillestrøm","Meråker","Narvik","Hammerfest", 
      "Selbu","Inderøy", "Hammerfest",  "Vågan","Lillestrøm","Lillestrøm",
      "Andøy","Tydal","Selbu","Andøy","Selbu"),
    size=c(724,716,593,586,563,560,556,547,541,530,525,
           518,506,488,470,468,443,431,420,405,388,377,365,
           361,359,341,313,310,301,297,273,273,267,265,262,
           260,250,195,195,191,172,171,160,94,66,64,62,51))
# random order for tied sizes
nsf48 <- dim(schoolframe48)[1]
runif.sf48 <- runif(n=nsf48)
schoolframe48 <- schoolframe48[order(-schoolframe48$size,runif.sf48),]

# data frame with one school deleted
schoolframe47 <- schoolframe48[schoolframe48$school!="Bleik Montessoriskole",]
# random order for tied sizes
nsf47 <- dim(schoolframe47)[1]
runif.sf47 <- runif(n=nsf47)
schoolframe47 <- schoolframe47[order(-schoolframe47$size,runif.sf47),]

# schools per service, original frame
length(unique(schoolframe48$service)) # 23 services
sort(table(schoolframe48$service))
table(table(schoolframe48$service)) # 10 one-school services, 5 two-
# school, 6 three-schools, 1 four-school and 1 six-school service

# schools per service, one school deleted
length(unique(schoolframe47$service)) # 23 services
sort(table(schoolframe47$service))
table(table(schoolframe47$service)) # 10 one-school services, 6 two-
# school, 5 three-schools, 1 four-school and 1 six-school service

# randomization for a possibly unbalanced design, where the number of 
# schools is not a multiple of the number of conditions, this may occur 
# even when a balanced design is aimed for, if one or more schools 
# retract their participation. This was the case for schoolframe47 
# above, where one school was lost
echorand.unbalanced <- 
  function(sframe, intprint=0, condrank=8:1,type=1, nrestr=2,
           cond=c("lhf","lhn","llf","lln","shf","shn","slf","sln")) {
    # type=1, more schools for the least demanding conditions
    # type=2, more schools for a random sample of conditions
    # nrestr, number of restricted interventions, highest rank
    cond <- cond[order(-condrank)] # rank order conditions,
    ncond <- length(cond)          # ... highest rank first
    nschool <- dim(sframe)[1]
    sframe$origsort <- 1:nschool
    # determine number of schools per condition
    schoolspercond.low <- floor(nschool/ncond) # lowest number of schools 
    # per condition, some conditions with one more
    ncond.high <- nschool - ncond*schoolspercond.low # number of conditions
    ncond.low <- ncond - ncond.high
    schoolspercond <- rep(schoolspercond.low, ncond)
    if (ncond.high>0) {
      condnr <- 1:ncond
      condnr.high <- (ncond-ncond.high+1):ncond
      if (type==2) condnr.high <- sample(x=condnr, size=ncond.high)
      schoolspercond[condnr.high] <- schoolspercond.low + 1
    } # end if unbalanced
    if (intprint==1) {
      print("schools per condition")
      schoolspercond.print <- schoolspercond
      names(schoolspercond.print) <- cond
      print(schoolspercond.print)
      }
    # stage 1, first random services for the nrestr first conditions,
    # then one school within each of htese services
    sframe$condition <- "unselected"
    tabserv <- table(sframe$service)
    serv <- names(tabserv)
    nserv <- length(serv) # number of services
    nsps <- as.numeric(tabserv) # number of schools per service  
    # determine services/schools within condition 1 and 2
    nserv.restr <- sum(schoolspercond[1:nrestr])
    serv.restr <- sample(serv, size=nserv.restr, prob=nsps)
    cond.serv.restr <- rep(cond[1:nrestr], schoolspercond[1:nrestr])
    if (intprint==1) {
      print("stage 1, services for restricted conditions")
      serv.restr.print <- serv.restr
      names(serv.restr.print) <- cond.serv.restr
      print(serv.restr.print)
    }
    for (snr in 1:nserv.restr) {
      se <- serv.restr[snr]
      schools.act <- sframe$schoolnr[sframe$service==se]
      if (length(schools.act)==1) schooltake <- schools.act
      if (length(schools.act)>1)schooltake <- sample(schools.act, size=1)
      sframe$condition[sframe$schoolnr==schooltake] <- cond.serv.restr[snr]
    } # end select schools for condition 1
    if (intprint==1) {
      print("stage 1, schools allocated to restricted conditions")
      sframe.restricted.print <- sframe[sframe$condition!="unselected",]
      sframe.restricted.print <- sframe.restricted.print[
        order(sframe.restricted.print$condition, -sframe.restricted.print$size),
        c("schoolnr","school","service","size","condition")]
      print(sframe.restricted.print)
      }
    # stage 2, allocate condition to schools not included in stage 1
    sframe.stage2 <- sframe[sframe$condition=="unselected",]
    n.stage2 <- dim(sframe.stage2)[1]
    sframe.stage2$origsort <- 1:n.stage2
    sframe.stage2$runif.stage2 <- runif(n=n.stage2)
    sframe.stage2 <- sframe.stage2[
      order(sframe.stage2$size, sframe.stage2$runif.stage2),]
    cond.stage2 <- cond[(nrestr+1):ncond]
    ncond.stage2 <- length(cond.stage2)
    schoolspercond.stage2 <- schoolspercond[(nrestr+1):ncond]
    schoolspercond.stage2.unique <- sort(unique(schoolspercond.stage2))
    nschoolspercond.stage2.unique <- length(schoolspercond.stage2.unique)
    if (nschoolspercond.stage2.unique==1) {
      schoolspercond.stage2.unique <- schoolspercond.stage2.unique[1]
      sizegr <- rep(1:schoolspercond.stage2.unique, 
                    rep(ncond.stage2,schoolspercond.stage2.unique))
      for (sgr in 1:schoolspercond.stage2.unique) 
        sframe.stage2$condition[sizegr==sgr] <- sample(cond.stage2)
      sframe.stage2$sizegr <- sizegr
      if (intprint==1) {
        print("stage 2, the case with balanced allocation within the unrestricted conditions")
        print("schools allocated to unrestricted conditions, with size groups displayed")
        print(sframe.stage2[,c("schoolnr","school","service","size","sizegr","condition")])
        }
    } else if (nschoolspercond.stage2.unique==2) {
      schoolspercond.stage2.max <- max(schoolspercond.stage2.unique)
      cond.low <- cond.stage2[schoolspercond.stage2 <
                                schoolspercond.stage2.max]
      sizegr <- rep(1:schoolspercond.stage2.max, 
                    rep(ncond.stage2,schoolspercond.stage2.max))
      ncond.init <- ncond.stage2*schoolspercond.stage2.max
      cond.init <- rep("unselected", ncond.init)
      for (sgr in 1:schoolspercond.stage2.max) 
        cond.init[sizegr==sgr] <- sample(cond.stage2)
      delete.cond <- rep(0,ncond.init)
      cond.init.nr <- 1:ncond.init
      for (condl in cond.low) {
        cond.init.nr1 <- cond.init.nr[cond.init==condl]
        delete.cond[sample(cond.init.nr1, size=1)] <- 1
      } # end for the less frequent conditions
      sframe.stage2$sizegr <- sizegr[delete.cond==0]
      sframe.stage2$condition <- cond.init[delete.cond==0]
      if (intprint==1) {
        print("stage 2, the case with unbalanced allocation within the unrestricted conditions")
        if (length(cond.stage2)>1) print("stage 2 conditions")
        print(cond.stage2)
        if (length(cond.low)==1) print(paste0("intervention with less schools, ",
                                              cond.low, ". original allocation, as if balanced"))
        if (length(cond.low)>1) print(paste0("intervention with one school less, ",
                                             cond.low, ". original allocation, as if balanced"))
        cond.init.print <- matrix(cond.init, byrow=TRUE,
                                  nrow=schoolspercond.stage2.max)
        colnames(cond.init.print) <- paste0("..", 1:dim(cond.init.print)[1])
        rownames(cond.init.print) <- paste0("size group ", 1:dim(cond.init.print)[1])
        print(cond.init.print)
        delete.cond.print <- matrix(delete.cond, byrow=TRUE,
                                    nrow=schoolspercond.stage2.max)
        rownames(delete.cond.print) <- paste0("size group ", 1:dim(cond.init.print)[1])
        colnames(delete.cond.print) <- paste0("..", 1:dim(cond.init.print)[1])
        if (length(cond.low)==1) print("deletion to account for unbalanced allocation")
        if (length(cond.low)>1) print("deletions to account for unbalanced allocation")
        print(delete.cond.print)
        print("schools allocated to unrestricted conditions, with size groups displayed")
        print(sframe.stage2[,c("schoolnr","school","service","size","sizegr","condition")])
      }
    } else {
      errl <- paste0("unexpeced ",nschoolspercond.stage2.unique)
      stop(err1)
    } # end school frame stage 2
    # sort back, merge, sort back:
    sframe.stage2 <- sframe.stage2[order(sframe.stage2$origsort),]
    sframe$condition[sframe$condition=="unselected"] <-  
      sframe.stage2$condition
    sframe <- sframe[order(sframe$origsort),
                     c("schoolnr","school","service","size","condition")]
    return(sframe)
  } # end function echorand.unbalanced
e1 <- echorand.unbalanced(sframe=schoolframe47, intprint=1)
e1 # 47 schools, the first cond with 5 schools, the other with 6
e2 <- echorand.unbalanced(sframe=schoolframe47, intprint=1, type=2)
e2 # 47 schools, a random cond with 5 schools, the others with 6
e3 <- echorand.unbalanced(sframe=schoolframe47[1:42,], intprint=1)
e3 # 42 schools, the first six conds with 5 schools, the last two with 6
e4 <- echorand.unbalanced(sframe=schoolframe47[1:42,], intprint=1, type=2)
e4 # 42 schools, 6 random conds with 5 schools, the other two with 6
