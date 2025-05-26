# 
# read phaeno data
# loop plots
# chart phasis
# save rda
# 

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","data.table","sp","mgcv","RPostgreSQL","dotenv","alluvial")
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
G$d_in1 <-file.path(G$d_home,"output/rda"); list.files(G$d_in1);
G$d_out <-file.path(G$d_home,"output"); if(!dir.exists(G$d_out)){dir.create(G$d_out)};
G$d_out1 <-file.path(G$d_out,"rda"); if(!dir.exists(G$d_out1)){dir.create(G$d_out1)};
G$d_out2 <-file.path(G$d_out,G$n_script); if(!dir.exists(G$d_out2)){dir.create(G$d_out2)};
G$d_out3 <-file.path(G$d_out2,"alluvial"); if(!dir.exists(G$d_out3)){dir.create(G$d_out3)};

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
# load(file.path(G$d_in1,"07_model_day_Mm.rda")); list.files(G$d_in1);

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


# LOOP plot ------------------------------------------------------------------
ll <-levels(as.factor(cc_trc$code_plot))
ii <-6;
for(ii in 1:length(ll))
{
  aa <-cc_trc[cc_trc$code_plot%in%ll[ii],];
  aa <-aa[aa$code_removal<=9,]; # levels(as.factor(aa$code_removal))
  aa <-aa[is.na(aa$code_defoliation)==F,]; 
  aa <-aa[aa$code_defoliation>=0,]; 
  ### boxplot
  {
    nam <-paste("defoliation - ",ll[ii])
    ### y
    aa$y <-aa$code_defoliation 
    y_nam <-"defoliation [%]"; 
    y_min <-0; y_max <-100; 
    ### x
    aa$x <-aa$survey_year; 
    x_nam <-"year";
    ### window
    out <-paste(G$d_out2,paste("defoliation",ll[ii],".png",sep="_"),sep="/");
    png(out, width=300,height=100,units="mm",res=300,pointsize=5); 
    par(cex=1.5,cex.lab=2,cex.axis=2,pch=16, mar=c(2,5,1,1)); # par()
    ### base
    ee <-boxplot(aa$y~aa$x,ylim=c(y_min,y_max),ylab=y_nam,xlab=x_nam,main=nam);
    grid();
    boxplot(aa$y~aa$x,col="orange3",add=T);
    ### save
    graphics.off();
  }
  ### def.class
  {
    summary(aa$code_defoliation)
    ### classes
    aa[aa$code_defoliation<=10,"def.class"] <-0;
    aa[aa$code_defoliation>10 & aa$code_defoliation<=25,"def.class"] <-1;
    aa[aa$code_defoliation>25 & aa$code_defoliation<=60,"def.class"] <-2;
    aa[aa$code_defoliation>60 & aa$code_defoliation<=99,"def.class"] <-3;
    aa[aa$code_defoliation%in%100,"def.class"] <-4;
    ### propportion
    pp <-levels(as.factor(aa$survey_year));
    jj <-1; cc <-data.frame(matrix(NA,0,0))
    for(jj in 1:length(pp))
    {
      bb <-aa[aa$survey_year%in%pp[jj],];
      cc[jj,"year"] <-as.integer(pp[jj]);
      cc[jj,"cc_0"] <-nrow(bb[bb$def.class%in%0,])/nrow(bb)*100
      cc[jj,"cc_1"] <-nrow(bb[bb$def.class%in%1,])/nrow(bb)*100
      cc[jj,"cc_2"] <-nrow(bb[bb$def.class%in%2,])/nrow(bb)*100
      cc[jj,"cc_3"] <-nrow(bb[bb$def.class%in%3,])/nrow(bb)*100
      cc[jj,"cc_4"] <-nrow(bb[bb$def.class%in%4,])/nrow(bb)*100
    }
    cc[,"cc_2-4"] <-cc$cc_2+cc$cc_3+cc$cc_4
  }
  ### plot
  {    
    nam <-paste("defoliation classes - ",ll[ii])
    ### y
    y_nam <-"frequency [%]"; 
    y_min <-0; y_max <-100; 
    ### x
    x_nam <-"year";
    ### window
    out <-paste(G$d_out2,paste("defoliation_class",ll[ii],".png",sep="_"),sep="/");
    png(out, width=300,height=100,units="mm",res=300,pointsize=5); 
    par(cex=1.5,cex.lab=2,cex.axis=2,pch=16, mar=c(2,5,1,1)); # par()
    ### base
    plot(cc$cc_0~cc$year,type="l",main=nam,
         ylim=c(y_min,y_max),ylab=y_nam,xlab=x_nam,pch=16,xaxt="n");
    axis(1,at=cc$year,labels=cc$year)
    grid();
    ### legend
    legend("topleft",
           legend = c("def.class 0 (defoliation = 0-10 %)",
                      "def.class 1 (defoliation = 11-25 %)",
                      "def.class 2-4 (defoliation = 26-100 %)"),
           col=c("green3","orange","red2"),pch=c("-","-","-"),lwd = c(2,2,2),cex=2)
    ### lines
    lines(cc$cc_0~cc$year,col="green3",lwd=2)
    lines(cc$cc_1~cc$year,col="orange",lwd=2)
    lines(cc$cc_2-4~cc$year,col="red2",lwd=2)
    ### save
    graphics.off();
  }
  ### alluvial 
  {
    pp <-levels(as.factor(aa$survey_year)); pp <-as.integer(pp[-1]);
    jj <-1;
    for(jj in 1:length(pp))
    {
      dd <-aa[aa$survey_year%in%c(pp[jj]-1),];
      ee <-aa[aa$survey_year%in%c(pp[jj]),];
      mm <-c("cc_0","cc_1","cc_2-4");
      kk <-1; ww <-data.frame(matrix(NA,0,0));
      for(kk in 1:length(mm))
      {
        if(mm[kk]=="cc_0"){uu <-c(0)};
        if(mm[kk]=="cc_1"){uu <-c(1)};
        if(mm[kk]=="cc_2-4"){uu <-c(2:4)};
        ff <-dd[dd$def.class%in%uu,];
        ww[nrow(ww)+1,"t0"] <-mm[kk];
        ww[nrow(ww),"t1"] <-mm[1];
        gg <-ee[ee$def.class%in%c(0),];
        hh <-gg[gg$tree_number%in%ff$tree_number,];
        ww[nrow(ww),"freq"] <-nrow(hh);
        ww[nrow(ww)+1,"t0"] <-mm[kk];
        ww[nrow(ww),"t1"] <-mm[2];
        gg <-ee[ee$def.class%in%c(1),];
        hh <-gg[gg$tree_number%in%ff$tree_number,];
        ww[nrow(ww),"freq"] <-nrow(hh);
        ww[nrow(ww)+1,"t0"] <-mm[kk];
        ww[nrow(ww),"t1"] <-mm[3];
        gg <-ee[ee$def.class%in%c(2:4),];
        hh <-gg[gg$tree_number%in%ff$tree_number,];
        ww[nrow(ww),"freq"] <-nrow(hh);
      }
      ### window
      out <-paste(G$d_out3,paste("alluvial",ll[ii],pp[jj],".png",sep="_"),sep="/");
      png(out, width=100,height=100,units="mm",res=300,pointsize=5); 
      par(mar=c(0,0,10,0));
      ### plot
      alluvial(ww[,1:2], freq=ww$freq, xw=0.0, alpha=0.8,cex=2,
               gap.width=0.1, border="white",
               layer = ww$t1%in%c("cc_0","cc_1","cc_2-4"),
               col=ifelse(ww$t1=="cc_0", "green3", ifelse(ww$t1=="cc_1", "orange", "red3")));
      mtext(pp[jj],outer = F, line=0,side=3,cex=3,col="blue3");
      ### save
      graphics.off();
    }
  }
  ### end ii
}




# CLEAN ---------------------------------------------------
rm(list = ls());  gc()
cat("\014")
