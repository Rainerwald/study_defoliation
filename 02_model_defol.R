# 
# read phaeno data
# loop plots
# chart phasis
# save rda
# 

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","data.table","sp","corrplot","mgcv","RPostgreSQL","dotenv","trend","nls2","mgcv","ggplot2","mgcViz")
for(ii in 1:length(ll)){aa <-ll[ii];if(!aa%in%rownames(installed.packages()))install.packages(aa, dependencies = TRUE); library(aa, character.only = TRUE)}

# GLOBALS G ------------------------------------------------------------------
G <-list() ### list workfiles and facts
aa <-getActiveDocumentContext()$path
bb <-unlist(str_split(aa,"\\/"))
### name
G$n_script <-str_sub(bb[length(bb)],1,-3) 
G$n_project <-bb[length(bb)-1]
### time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); # G$year <-2023;
### dir
G$d_home <-dirname(aa); message(G$d_home);
G$d_in <-file.path(G$d_home,"input");  list.files(G$d_in)
cc <-unlist(str_split(G$d_in,G$n_project))[1];
G$d_in1 <-file.path(cc,"icpf_mm/output/rda"); list.files(G$d_in1);
G$d_out <-file.path(G$d_home,"output"); if(!dir.exists(G$d_out)){dir.create(G$d_out)};
G$d_out1 <-file.path(G$d_out,"rda"); if(!dir.exists(G$d_out1)){dir.create(G$d_out1)};
G$d_out2 <-file.path(G$d_out,G$n_script); if(!dir.exists(G$d_out2)){dir.create(G$d_out2)};

### end
print(G)


# ENVIRONMENT -------------------------------------------------------
load_dot_env(file =file.path(G$d_home,".env"))
E <-list();
E[["sys_env"]] <-Sys.getenv(); 
E[["session"]] <-sessionInfo();
E[["options"]] <-options();

# LOAD data ---------------------------------------------------------------------
list.files(G$d_in1);
load(file.path(G$d_in1,"08_aggregate_AGG.rda"));

# CONNECT PG -------------------------------------------------------------
C <-list();

### CONNECT
aa <-E[["sys_env"]]
C$host <-aa[names(aa)%in%"FUK_PG_HOST"]; C$port <-aa[names(aa)%in%"FUK_PG_PORT"];
C$user <-aa[names(aa)%in%"FUK_PG_USER"]; C$pw <-aa[names(aa)%in%"FUK_PG_PW"]; 
C$db <-aa[names(aa)%in%"FUK_PG_DB"]; 
C$pg <- dbConnect(PostgreSQL(),host=C$host,user=C$user,password=C$pw,port=C$port,dbname=C$db);

### pg/icp_download
C$s1 <-"icp_download";
qq <-paste("SELECT * FROM information_schema.tables WHERE table_schema ='", C$s1, "';", sep="");
aa <- dbSendQuery(C$pg, statement=qq); 
bb <- fetch(aa, -1); tt <-bb$table_name; tt <-tt[order(tt)]; cbind(tt);
dbGetQuery(C$pg,paste("SET search_path TO",C$s1)); 
cc_trc <-dbReadTable(C$pg, "cc_trc"); 

### pg/icp_download
C$s1 <-"icp_dictionaries";
qq <-paste("SELECT * FROM information_schema.tables WHERE table_schema ='", C$s1, "';", sep="");
aa <- dbSendQuery(C$pg, statement=qq); 
bb <- fetch(aa, -1); tt <-bb$table_name; tt <-tt[order(tt)]; cbind(tt);
dbGetQuery(C$pg,paste("SET search_path TO",C$s1)); 
d_tree_spec <-dbReadTable(C$pg, "d_tree_spec"); 
d_defoliation <-dbReadTable(C$pg, "d_defoliation"); 


# meteo l2m ---------------------------------------------------------------
ll <-names(AGG);
ii <-2; l2m <-list();
for (ii in 1:length(ll)) 
  {
    aa <-AGG[[ll[ii]]];
    if(ii==1)
    {
      jj <-2;
      for(jj in 2:ncol(aa))
      {
        bb <-aa[,c(1,jj)]; 
        colnames(bb)[1] <-"year";
        colnames(bb)[2] <-ll[ii];
        l2m[[colnames(aa)[jj]]] <-bb;
      }
      next;
    }
    ### merge
    jj <-2;
    for(jj in 2:ncol(aa))
    {
      bb <-aa[,c(1,jj)]; 
      colnames(bb)[1] <-"year";
      colnames(bb)[2] <-ll[ii];
      cc <-l2m[[colnames(aa)[jj]]];
      dd <-merge(cc,bb,by="year",all=T);
      l2m[[colnames(aa)[jj]]] <-dd;
    }
  }

# LOOP gam_mean_defoliation ----------------------------------------------------------------
ll <-levels(as.factor(cc_trc$code_plot))
ii <-1;
for(ii in 1:length(ll))
{
  ### data
  {
    aa <-cc_trc[cc_trc$code_plot%in%ll[ii],];
    aa <-aa[aa$code_removal<=9,]; aa <-aa[is.na(aa$code_defoliation)==F,]; aa <-aa[aa$code_defoliation>=0,]; 
    a1 <-tapply(aa$code_defoliation,aa$survey_year,mean);
    cc <-data.frame(year=as.integer(names(a1)),defol=a1);
    bb <-l2m[[ll[ii]]]; # meteo
    if(is.null(nrow(bb))){next}
    b1 <-bb[,c("year","YEAR-pr_MEANSUM","YEAR-pet_MEANSUM","0409-pr_MEANSUM","0409-pet_MEANSUM")]; # colnames(bb)
    b1$"YEAR-cwb_MEANSUM" <-b1$`YEAR-pr_MEANSUM`-b1$`YEAR-pet_MEANSUM`;
    b1$"0409-cwb_MEANSUM" <-b1$`0409-pr_MEANSUM`-b1$`0409-pet_MEANSUM`;
    b1$year <-as.integer(b1$year);
    cc <-merge(cc,b1,by="year",all=F);
    b2 <-b1; b2$year <-b2$year-1; colnames(b2) <-paste(colnames(b1),"t1",sep="_");
    colnames(b2)[1] <-colnames(b1)[1];
    cc <-merge(cc,b2,by="year",all=F);
    colnames(cc) <-str_replace_all(colnames(cc),"MEANSUM","");
    colnames(cc) <-str_replace_all(colnames(cc),"YEAR","yr");
    colnames(cc) <-str_replace_all(colnames(cc),"0409","gs");
    colnames(cc) <-str_replace_all(colnames(cc),"__","_");
  }
  ### corr-matrix
  {
    G$d_temp <-file.path(G$d_out2,"corrplot"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)}
    graphics.off(); 
    out <-paste("corrplot",ll[ii],".png",sep="_");
    png(file.path(G$d_temp,out), units="mm", width=160, height=100, res=1000);
    cc_cor <-cor(cc[,-1])
    corrplot(cc_cor, method="color")
    graphics.off(); 
  }
  ### loop model
  G$d_temp <-file.path(G$d_out2,"gam_mean_defoliation"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)}
  dd <-sort(abs(cc_cor[,1]),decreasing = T);
  mm <-names(dd)[2:6];
  jj <-1; xx <-data.frame(matrix(NA,0,0))
  for(jj in 1:length(mm))
  {
    cc$y <-cc[,"defol"]; cc$x <-cc[,mm[jj]];
    ### nam
    nam <-paste0("gam_n",mm[jj]);
    nam_main <-paste0("defoliation ~",mm[jj])
    nam_xlab <-expression(paste(mm[jj]))
    nam_ylab <-expression(paste("mean defoliation [%]"))
    ### limit
    xmax <-ceiling(max(cc$x)); xmin <-ceiling(min(cc$x));
    ymax <-ceiling(max(cc$y)); ymin <-ceiling(min(cc$y));
    ### gam
    {
      # https://stackoverflow.com/questions/67077306/plotting-output-of-gam-model
      ff <-gam(y~s(x,bs="cs"),method = "REML",dat=cc); # plot(ff)
      # ff <-gam(y~s(x,bs="bs"),dat=cc); plot(ff)
      # ff <-lm(y~splines::bs(x, 3),dat=cc); # plot(ff)
      gg <-summary(ff); gg$r.sq;
      preds <-predict(ff,se.fit=T)
      my_data <- data.frame(mu=preds$fit, 
                            low =(preds$fit - 1.96 * preds$se.fit), 
                            high = (preds$fit + 1.96 * preds$se.fit),
                            x=cc$x, y=cc$y
      )
    }
    ### plot model
    {
      graphics.off(); 
      out <-paste("x1",nam,ll[ii],".png",sep="_");
      png(file.path(G$d_temp,out), units="mm", width=160, height=100, res=1000);
      par(mar=c(3.5,3.5,2,1),mgp=c(2,.75,0),lab=c(nrow(aa)/5,12,7));
      ### Viz
      b <-getViz(ff);
      ### plot
      o <- plot( sm(b, 1) ) # 1. model effect
      o <- o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
        l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
        l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()  +
        geom_text(aes(mean(b$model$x),max(b$model$y)*0.5), 
                  label="y ~ s(x, bs = cs)",
                  family="sans",parse=F,size = 5)+ 
        labs(title=paste0("GAM Model: ",nam_main))
      print(o); 
      ### save
      graphics.off(); 
    }
    ### plot fit 
    {
      graphics.off(); 
      out <-paste("y",nam,ll[ii],".png",sep="_");
      png(file.path(G$d_temp,out), units="mm", width=160, height=100, res=1000);par(mar=c(3.5,3.5,2,1),mgp=c(2,.75,0),lab=c(nrow(aa)/5,12,7));
      xtext <-xmax-100; ytext <-ymax;
      pp <-ggplot()+
        geom_line(data = my_data, aes(x=x, y=mu), linewidth=1, col="blue")+
        geom_smooth(data=my_data,aes(ymin = low, ymax = high, x=x, y = mu), 
                    stat = "identity", col="green")+
        geom_point(data=my_data,aes(x=x, y=y), col="black") +
        # geom_smooth(method = gam, formula = y ~ splines::bs(x, 3), se = T) +
        labs(title=paste0("GAM Fit: ",nam_main)) + xlab(nam_xlab) + ylab(nam_ylab) +
        geom_text(aes(xtext,ytext), label=paste0("R-sq.(adj) = ",round(gg$r.sq,3)),
                  family="sans",parse=F,size = 5) +
        geom_text(aes(xtext,ytext-ytext*0.1), label=paste0("dev.expl = ",round(gg$dev.expl,3)),
                  family="sans",parse=F,size = 5);
      print(pp);
      ### save
      graphics.off(); 
    } 
    ### table
    {
      xx[jj,"var"] <-mm[jj]
      xx[jj,"corr"] <-dd[mm[jj]]
      xx[jj,"r.sq"] <-gg$r.sq
      xx[jj,"dev.expl"] <-gg$dev.expl
    }
  }
  ### save table
  G$d_temp <-file.path(G$d_out2,"gam_mean_defoliation_table"); if(!dir.exists(G$d_temp)){dir.create(G$d_temp)};
  out <-paste("gam_table",ll[ii],".csv",sep="_");
  write.table(xx,file=file.path(G$d_temp,out),sep=";",dec=".",
              col.names = T,row.names = F)
}



# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")
