library("odbc")

con <- dbConnect(odbc::odbc(),
                 Driver   = "FreeTDS",
                 Server   = "",
                 Database = "",
                 UID      = "",
                 PWD      = "",
                 Port     = ####)

Reservas <- dbGetQuery(con, "SELECT * FROM RSG_RESERVA")

# Indicar el periodo que se quiere analizar.
# Los primeros cuatro digitos hacen referencia al año, 
# mientas que los últimos dos son respecto al mes.


  Periodo<-201901
  Peri<-factor(Periodo)
  Archivo<-as.character(paste("Periodo_",Periodo,".csv", sep = ""))
  
  creditos<-subset(Reservas,Reservas[,12]==as.character.factor(Peri))   
  # View(creditos) 
  
  Nombres_tab<-c(" Categoría",
                 "Exposición al Incumplimiento (EI)", 
                 "Porcentaje de participación (EI)", 
                 "Porcentaje de participación acumulada (EI)",
                 "Reservas (R)",
                 "Porcentaje de participación (R)", 
                 "Porcentaje de participación acumulada (R)",
                 "Probabilidad al Incumplimiento Ponderada (PI)",
                 "Severidad de la Pérdida Ponderada (SP)",
                 "Porcentaje de Reservas",
                 "Tabla",
                 "Periodo")
  
  # --------
  # Tabla por estatus AA
  # --------
  
  AA_Referencias<-c("Vigente", "Vencida")  
  AA_Base_Ref<-creditos[,19]               
  AA_Base_Ref_Fact<-factor(AA_Base_Ref,levels = AA_Referencias) 
  
  AA_ExI<-creditos[,4]     
  AA_Res<-creditos[,7]     
  AA_PIP<-creditos[,16]    
  AA_SPP<-creditos[,17]    
  
  x1<-tapply(AA_ExI,AA_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(AA_Referencias)){xp[i]<-NA} 
  for(i in 1:length(AA_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(AA_ExI,AA_Base_Ref_Fact,sum)/sum(AA_ExI)
  xp<-x2
  for(i in 1:length(AA_Referencias)){xp[i]<-NA} 
  for(i in 1:length(AA_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(AA_Res,AA_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(AA_Referencias)){xp[i]<-NA} 
  for(i in 1:length(AA_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(AA_Res,AA_Base_Ref_Fact,sum)/sum(AA_Res)
  xp<-x5
  for(i in 1:length(AA_Referencias)){xp[i]<-NA} 
  for(i in 1:length(AA_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(AA_PIP,AA_Base_Ref_Fact,sum)/tapply(AA_ExI,AA_Base_Ref_Fact,sum)
  x8<-tapply(AA_SPP,AA_Base_Ref_Fact,sum)/tapply(AA_ExI,AA_Base_Ref_Fact,sum)
  x9<-tapply(AA_Res,AA_Base_Ref_Fact,sum)/tapply(AA_ExI,AA_Base_Ref_Fact,sum)
  x10<-rep("Estatus",length(AA_Referencias))
  x11<-rep(Periodo,length(AA_Referencias))
  x0<-AA_Referencias
  
  xp<-x7
  for(i in 1:length(AA_Referencias)){xp[i]<-NA} 
  for(i in 1:length(AA_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(AA_Referencias)){xp[i]<-NA} 
  for(i in 1:length(AA_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(AA_Referencias)){xp[i]<-NA} 
  for(i in 1:length(AA_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  AA_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(AA_Tabla_Inf)<-c(Nombres_tab)
  
  AA_Tabla_Inf<-AA_Tabla_Inf[rev(order(AA_Tabla_Inf[,2])),]
  
  AA_Tabla_Inf[,4]<-cumsum(AA_Tabla_Inf[,3])
  AA_Tabla_Inf[,7]<-cumsum(AA_Tabla_Inf[,6])
  
  # View(AA_Tabla_Inf)
  
  # --------
  # Tabla por categoria del prestamo BB
  # --------
  
  BB_Referencias<-c("Directos", "IFB", "IFE")
  BB_Base_Ref<-creditos[,18]
  BB_Base_Ref_Fact<-factor(BB_Base_Ref,levels = BB_Referencias)
  
  BB_ExI<-creditos[,4]
  BB_Res<-creditos[,7]
  BB_PIP<-creditos[,16]
  BB_SPP<-creditos[,17]
  
  x1<-tapply(BB_ExI,BB_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(BB_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BB_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(BB_ExI,BB_Base_Ref_Fact,sum)/sum(BB_ExI)
  xp<-x2
  for(i in 1:length(BB_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BB_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(BB_Res,BB_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(BB_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BB_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(BB_Res,BB_Base_Ref_Fact,sum)/sum(BB_Res)
  xp<-x5
  for(i in 1:length(BB_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BB_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(BB_PIP,BB_Base_Ref_Fact,sum)/tapply(BB_ExI,BB_Base_Ref_Fact,sum)
  x8<-tapply(BB_SPP,BB_Base_Ref_Fact,sum)/tapply(BB_ExI,BB_Base_Ref_Fact,sum)
  x9<-tapply(BB_Res,BB_Base_Ref_Fact,sum)/tapply(BB_ExI,BB_Base_Ref_Fact,sum)
  x10<-rep("Categoria_Prestamo",length(BB_Referencias))
  x11<-rep(Periodo,length(BB_Referencias))
  x0<-BB_Referencias
  
  xp<-x7
  for(i in 1:length(BB_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BB_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(BB_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BB_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(BB_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BB_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  BB_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(BB_Tabla_Inf)<-c(Nombres_tab)
  
  BB_Tabla_Inf<-BB_Tabla_Inf[rev(order(BB_Tabla_Inf[,2])),]
  
  BB_Tabla_Inf[,4]<-cumsum(BB_Tabla_Inf[,3])
  BB_Tabla_Inf[,7]<-cumsum(BB_Tabla_Inf[,6])
  
  # View(BB_Tabla_Inf)
  
  # --------
  # Tabla por categoria del prestamo con cartera vencida BX
  # --------
  
  BX_Referencias<-c("Directos", "IFB", "IFE", "Vencida")
  BX_Base_Ref<-factor(creditos[,18],BX_Referencias)
  xprueba<-as.factor(creditos[,19])
  elem_vig_ven<-factor("Vencida",c("Vencida","Vigente"))
  for(i in 1:length(BX_Base_Ref)) if(identical(xprueba[i],elem_vig_ven[1])) BX_Base_Ref[i]<-elem_vig_ven[1]
   
  BX_Base_Ref_Fact<-factor(BX_Base_Ref,levels = BX_Referencias)
  
  BX_ExI<-creditos[,4]
  BX_Res<-creditos[,7]
  BX_PIP<-creditos[,16]
  BX_SPP<-creditos[,17]
  
  x1<-tapply(BX_ExI,BX_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(BX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BX_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(BX_ExI,BX_Base_Ref_Fact,sum)/sum(BX_ExI)
  xp<-x2
  for(i in 1:length(BX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BX_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(BX_Res,BX_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(BX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BX_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(BX_Res,BX_Base_Ref_Fact,sum)/sum(BX_Res)
  xp<-x5
  for(i in 1:length(BX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BX_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(BX_PIP,BX_Base_Ref_Fact,sum)/tapply(BX_ExI,BX_Base_Ref_Fact,sum)
  x8<-tapply(BX_SPP,BX_Base_Ref_Fact,sum)/tapply(BX_ExI,BX_Base_Ref_Fact,sum)
  x9<-tapply(BX_Res,BX_Base_Ref_Fact,sum)/tapply(BX_ExI,BX_Base_Ref_Fact,sum)
  x10<-rep("Categoria_Prestamo_V",length(BX_Referencias))
  x11<-rep(Periodo,length(BX_Referencias))
  x0<-BX_Referencias
  
  xp<-x7
  for(i in 1:length(BX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BX_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(BX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BX_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(BX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(BX_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  BX_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(BX_Tabla_Inf)<-c(Nombres_tab)
  
  BX_Tabla_Inf<-BX_Tabla_Inf[rev(order(BX_Tabla_Inf[,2])),]
  
  BX_Tabla_Inf[,4]<-cumsum(BX_Tabla_Inf[,3])
  BX_Tabla_Inf[,7]<-cumsum(BX_Tabla_Inf[,6])
  
  # View(BX_Tabla_Inf)
  
  # --------
  # Tabla por grupo de actividad CC
  # --------
  
  CC_Referencias<-c("Grupo 1", "Grupo 2", "Grupo 3A", "Grupo 3B", "Grupo 4")
  CC_Base_Ref<-creditos[,14]
  CC_Base_Ref_Fact<-factor(CC_Base_Ref,levels = CC_Referencias)
  
  CC_ExI<-creditos[,4]
  CC_Res<-creditos[,7]
  CC_PIP<-creditos[,16]
  CC_SPP<-creditos[,17]
  
  x1<-tapply(CC_ExI,CC_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(CC_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CC_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(CC_ExI,CC_Base_Ref_Fact,sum)/sum(CC_ExI)
  xp<-x2
  for(i in 1:length(CC_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CC_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(CC_Res,CC_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(CC_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CC_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(CC_Res,CC_Base_Ref_Fact,sum)/sum(CC_Res)
  xp<-x5
  for(i in 1:length(CC_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CC_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(CC_PIP,CC_Base_Ref_Fact,sum)/tapply(CC_ExI,CC_Base_Ref_Fact,sum)
  x8<-tapply(CC_SPP,CC_Base_Ref_Fact,sum)/tapply(CC_ExI,CC_Base_Ref_Fact,sum)
  x9<-tapply(CC_Res,CC_Base_Ref_Fact,sum)/tapply(CC_ExI,CC_Base_Ref_Fact,sum)
  x10<-rep("Grupo_Actividad",length(CC_Referencias))
  x11<-rep(Periodo,length(CC_Referencias))
  x0<-CC_Referencias
  
  xp<-x7
  for(i in 1:length(CC_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CC_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(CC_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CC_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(CC_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CC_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  CC_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(CC_Tabla_Inf)<-c(Nombres_tab)
  
  CC_Tabla_Inf<-CC_Tabla_Inf[rev(order(CC_Tabla_Inf[,2])),]
  
  CC_Tabla_Inf[,4]<-cumsum(CC_Tabla_Inf[,3])
  CC_Tabla_Inf[,7]<-cumsum(CC_Tabla_Inf[,6])
  
  # View(CC_Tabla_Inf)
  
  # --------
  # Tabla por grupo de actividad con cartera vencida CX
  # --------
  
  CX_Referencias<-c("Grupo 1", "Grupo 2", "Grupo 3A", "Grupo 3B", "Grupo 4", "Vencida")
  CX_Base_Ref<-factor(creditos[,14],CX_Referencias)
  xprueba<-as.factor(creditos[,19])
  elem_vig_ven<-factor("Vencida",c("Vencida","Vigente"))
  for(i in 1:length(CX_Base_Ref)) if(identical(xprueba[i],elem_vig_ven[1])) CX_Base_Ref[i]<-elem_vig_ven[1]
  
  CX_Base_Ref_Fact<-factor(CX_Base_Ref,levels = CX_Referencias)
  
  CX_ExI<-creditos[,4]
  CX_Res<-creditos[,7]
  CX_PIP<-creditos[,16]
  CX_SPP<-creditos[,17]
  
  x1<-tapply(CX_ExI,CX_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(CX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CX_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(CX_ExI,CX_Base_Ref_Fact,sum)/sum(CX_ExI)
  xp<-x2
  for(i in 1:length(CX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CX_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(CX_Res,CX_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(CX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CX_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(CX_Res,CX_Base_Ref_Fact,sum)/sum(CX_Res)
  xp<-x5
  for(i in 1:length(CX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CX_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(CX_PIP,CX_Base_Ref_Fact,sum)/tapply(CX_ExI,CX_Base_Ref_Fact,sum)
  x8<-tapply(CX_SPP,CX_Base_Ref_Fact,sum)/tapply(CX_ExI,CX_Base_Ref_Fact,sum)
  x9<-tapply(CX_Res,CX_Base_Ref_Fact,sum)/tapply(CX_ExI,CX_Base_Ref_Fact,sum)
  x10<-rep("Grupo_Actividad_V",length(CX_Referencias))
  x11<-rep(Periodo,length(CX_Referencias))
  x0<-CX_Referencias
  
  xp<-x7
  for(i in 1:length(CX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CX_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(CX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CX_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(CX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(CX_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  CX_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(CX_Tabla_Inf)<-c(Nombres_tab)
  
  CX_Tabla_Inf<-CX_Tabla_Inf[rev(order(CX_Tabla_Inf[,2])),]
  
  CX_Tabla_Inf[,4]<-cumsum(CX_Tabla_Inf[,3])
  CX_Tabla_Inf[,7]<-cumsum(CX_Tabla_Inf[,6])
  
  # View(CX_Tabla_Inf)
  
  # --------
  # Tabla por canal de distribución DD
  # --------
  
  DD_Referencias<-c("Directos", "SOFOM ENR", "UC", "SOFOM ER", "BANCOS")
  DD_Base_Ref<-creditos[,15]
  DD_Base_Ref_Fact<-factor(DD_Base_Ref,levels = DD_Referencias)
  
  DD_ExI<-creditos[,4]
  DD_Res<-creditos[,7]
  DD_PIP<-creditos[,16]
  DD_SPP<-creditos[,17]
  
  x1<-tapply(DD_ExI,DD_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(DD_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DD_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(DD_ExI,DD_Base_Ref_Fact,sum)/sum(DD_ExI)
  xp<-x2
  for(i in 1:length(DD_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DD_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(DD_Res,DD_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(DD_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DD_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(DD_Res,DD_Base_Ref_Fact,sum)/sum(DD_Res)
  xp<-x5
  for(i in 1:length(DD_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DD_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(DD_PIP,DD_Base_Ref_Fact,sum)/tapply(DD_ExI,DD_Base_Ref_Fact,sum)
  x8<-tapply(DD_SPP,DD_Base_Ref_Fact,sum)/tapply(DD_ExI,DD_Base_Ref_Fact,sum)
  x9<-tapply(DD_Res,DD_Base_Ref_Fact,sum)/tapply(DD_ExI,DD_Base_Ref_Fact,sum)
  x10<-rep("Canal_Distribucion",length(DD_Referencias))
  x11<-rep(Periodo,length(DD_Referencias))
  x0<-DD_Referencias
  
  xp<-x7
  for(i in 1:length(DD_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DD_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(DD_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DD_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(DD_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DD_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  DD_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(DD_Tabla_Inf)<-c(Nombres_tab)
  
  DD_Tabla_Inf<-DD_Tabla_Inf[rev(order(DD_Tabla_Inf[,2])),]
  
  DD_Tabla_Inf[,4]<-cumsum(DD_Tabla_Inf[,3])
  DD_Tabla_Inf[,7]<-cumsum(DD_Tabla_Inf[,6])
  
  # View(DD_Tabla_Inf)
  
  # --------
  # Tabla por canal de distribución con cartera vencida DX
  # --------
  
  DX_Referencias<-c("Directos", "SOFOM ENR", "UC", "SOFOM ER", "BANCOS", "Vencida")
  DX_Base_Ref<-factor(creditos[,15],DX_Referencias)
  xprueba<-as.factor(creditos[,19])
  elem_vig_ven<-factor("Vencida",c("Vencida","Vigente"))
  for(i in 1:length(DX_Base_Ref)) if(identical(xprueba[i],elem_vig_ven[1])) DX_Base_Ref[i]<-elem_vig_ven[1]
  
  DX_Base_Ref_Fact<-factor(DX_Base_Ref,levels = DX_Referencias)
  
  DX_ExI<-creditos[,4]
  DX_Res<-creditos[,7]
  DX_PIP<-creditos[,16]
  DX_SPP<-creditos[,17]
  
  x1<-tapply(DX_ExI,DX_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(DX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DX_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(DX_ExI,DX_Base_Ref_Fact,sum)/sum(DX_ExI)
  xp<-x2
  for(i in 1:length(DX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DX_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(DX_Res,DX_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(DX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DX_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(DX_Res,DX_Base_Ref_Fact,sum)/sum(DX_Res)
  xp<-x5
  for(i in 1:length(DX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DX_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(DX_PIP,DX_Base_Ref_Fact,sum)/tapply(DX_ExI,DX_Base_Ref_Fact,sum)
  x8<-tapply(DX_SPP,DX_Base_Ref_Fact,sum)/tapply(DX_ExI,DX_Base_Ref_Fact,sum)
  x9<-tapply(DX_Res,DX_Base_Ref_Fact,sum)/tapply(DX_ExI,DX_Base_Ref_Fact,sum)
  x10<-rep("Canal_Distribucion_V",length(DX_Referencias))
  x11<-rep(Periodo,length(DX_Referencias))
  x0<-DX_Referencias
  
  xp<-x7
  for(i in 1:length(DX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DX_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(DX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DX_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(DX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DX_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  DX_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(DX_Tabla_Inf)<-c(Nombres_tab)
  
  DX_Tabla_Inf<-DX_Tabla_Inf[rev(order(DX_Tabla_Inf[,2])),]
  
  DX_Tabla_Inf[,4]<-cumsum(DX_Tabla_Inf[,3])
  DX_Tabla_Inf[,7]<-cumsum(DX_Tabla_Inf[,6])
  
  # View(DX_Tabla_Inf)
  
  # --------
  # Tabla por canal de distribución con  SOFOM DY
  # --------
  
  DY_Referencias<-c("Directos","SOFOM", "SOFOM ENR", "UC", "SOFOM ER", "BANCOS")
  DY_Base_Ref<-factor(creditos[,15],DY_Referencias)
  xprueba<-creditos[,19]
  elem_er_enr<-factor(c("SOFOM ENR", "SOFOM ER"),DY_Referencias)
  elem_sofom<-factor("SOFOM",DY_Referencias)
  for(i in 1:length(DY_Base_Ref)) if(identical(DY_Base_Ref[i],elem_er_enr[1])|identical(DY_Base_Ref[i],elem_er_enr[2])) DY_Base_Ref[i]<-elem_sofom[1]
  DY_Referencias<-c("Directos", "SOFOM", "UC", "BANCOS")
  DY_Base_Ref_Fact<-factor(DY_Base_Ref,levels = DY_Referencias)
  
  DY_ExI<-creditos[,4]
  DY_Res<-creditos[,7]
  DY_PIP<-creditos[,16]
  DY_SPP<-creditos[,17]
  
  x1<-tapply(DY_ExI,DY_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(DY_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DY_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(DY_ExI,DY_Base_Ref_Fact,sum)/sum(DY_ExI)
  xp<-x2
  for(i in 1:length(DY_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DY_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(DY_Res,DY_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(DY_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DY_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(DY_Res,DY_Base_Ref_Fact,sum)/sum(DY_Res)
  xp<-x5
  for(i in 1:length(DY_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DY_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(DY_PIP,DY_Base_Ref_Fact,sum)/tapply(DY_ExI,DY_Base_Ref_Fact,sum)
  x8<-tapply(DY_SPP,DY_Base_Ref_Fact,sum)/tapply(DY_ExI,DY_Base_Ref_Fact,sum)
  x9<-tapply(DY_Res,DY_Base_Ref_Fact,sum)/tapply(DY_ExI,DY_Base_Ref_Fact,sum)
  x10<-rep("Canal_Distribucion_B",length(DY_Referencias))
  x11<-rep(Periodo,length(DY_Referencias))
  x0<-DY_Referencias
  
  xp<-x7
  for(i in 1:length(DY_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DY_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(DY_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DY_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(DY_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DY_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  DY_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(DY_Tabla_Inf)<-c(Nombres_tab)
  
  DY_Tabla_Inf<-DY_Tabla_Inf[rev(order(DY_Tabla_Inf[,2])),]
  
  DY_Tabla_Inf[,4]<-cumsum(DY_Tabla_Inf[,3])
  DY_Tabla_Inf[,7]<-cumsum(DY_Tabla_Inf[,6])
  
  # View(DY_Tabla_Inf)
  
  # --------
  # Tabla por canal de distribución con  SOFOM y cartera vencida DZ
  # --------
  
  DZ_Referencias<-c("Directos","SOFOM", "SOFOM ENR", "UC", "SOFOM ER", "BANCOS", "Vencida")
  DZ_Base_Ref<-factor(creditos[,15],DZ_Referencias)
  xprueba<-as.factor(creditos[,19])
  elem_vig_ven<-factor("Vencida",c("Vencida","Vigente"))
  elem_er_enr<-factor(c("SOFOM ENR", "SOFOM ER"),DZ_Referencias)
  elem_sofom<-factor("SOFOM",DZ_Referencias)
  for(i in 1:length(DZ_Base_Ref)) if(identical(DZ_Base_Ref[i],elem_er_enr[1])|identical(DZ_Base_Ref[i],elem_er_enr[2])) DZ_Base_Ref[i]<-elem_sofom[1]
  for(i in 1:length(DZ_Base_Ref)) if(identical(xprueba[i],elem_vig_ven[1])) DZ_Base_Ref[i]<-elem_vig_ven[1]
  DZ_Referencias<-c("Directos", "SOFOM", "UC", "BANCOS", "Vencida")
  DZ_Base_Ref_Fact<-factor(DZ_Base_Ref,levels = DZ_Referencias)
  
  DZ_ExI<-creditos[,4]
  DZ_Res<-creditos[,7]
  DZ_PIP<-creditos[,16]
  DZ_SPP<-creditos[,17]
  
  x1<-tapply(DZ_ExI,DZ_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(DZ_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DZ_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(DZ_ExI,DZ_Base_Ref_Fact,sum)/sum(DZ_ExI)
  xp<-x2
  for(i in 1:length(DZ_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DZ_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(DZ_Res,DZ_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(DZ_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DZ_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(DZ_Res,DZ_Base_Ref_Fact,sum)/sum(DZ_Res)
  xp<-x5
  for(i in 1:length(DZ_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DZ_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(DZ_PIP,DZ_Base_Ref_Fact,sum)/tapply(DZ_ExI,DZ_Base_Ref_Fact,sum)
  x8<-tapply(DZ_SPP,DZ_Base_Ref_Fact,sum)/tapply(DZ_ExI,DZ_Base_Ref_Fact,sum)
  x9<-tapply(DZ_Res,DZ_Base_Ref_Fact,sum)/tapply(DZ_ExI,DZ_Base_Ref_Fact,sum)
  x10<-rep("Canal_Distribucion_BV",length(DZ_Referencias))
  x11<-rep(Periodo,length(DZ_Referencias))
  x0<-DZ_Referencias
  
  xp<-x7
  for(i in 1:length(DZ_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DZ_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(DZ_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DZ_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(DZ_Referencias)){xp[i]<-NA} 
  for(i in 1:length(DZ_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  DZ_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(DZ_Tabla_Inf)<-c(Nombres_tab)
  
  DZ_Tabla_Inf<-DZ_Tabla_Inf[rev(order(DZ_Tabla_Inf[,2])),]
  
  DZ_Tabla_Inf[,4]<-cumsum(DZ_Tabla_Inf[,3])
  DZ_Tabla_Inf[,7]<-cumsum(DZ_Tabla_Inf[,6])
  
  # View(DZ_Tabla_Inf)
  
  # --------
  # Tabla por grado de riesgo EE
  # --------
  
  EE_Referencias<-c("A-1", "A-2", "B-1", "B-2", "B-3", "C-1", "C-2", "D", "E")
  EE_Base_Ref<-creditos[,8]
  EE_Base_Ref_Fact<-factor(EE_Base_Ref,levels = EE_Referencias)
  
  EE_ExI<-creditos[,4]
  EE_Res<-creditos[,7]
  EE_PIP<-creditos[,16]
  EE_SPP<-creditos[,17]
  
  x1<-tapply(EE_ExI,EE_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(EE_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EE_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(EE_ExI,EE_Base_Ref_Fact,sum)/sum(EE_ExI)
  xp<-x2
  for(i in 1:length(EE_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EE_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(EE_Res,EE_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(EE_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EE_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(EE_Res,EE_Base_Ref_Fact,sum)/sum(EE_Res)
  xp<-x5
  for(i in 1:length(EE_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EE_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(EE_PIP,EE_Base_Ref_Fact,sum)/tapply(EE_ExI,EE_Base_Ref_Fact,sum)
  x8<-tapply(EE_SPP,EE_Base_Ref_Fact,sum)/tapply(EE_ExI,EE_Base_Ref_Fact,sum)
  x9<-tapply(EE_Res,EE_Base_Ref_Fact,sum)/tapply(EE_ExI,EE_Base_Ref_Fact,sum)
  x10<-rep("Grado_Riesgo",length(EE_Referencias))
  x11<-rep(Periodo,length(EE_Referencias))
  x0<-EE_Referencias
  
  xp<-x7
  for(i in 1:length(EE_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EE_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(EE_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EE_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(EE_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EE_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  EE_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(EE_Tabla_Inf)<-c(Nombres_tab)
  
  EE_Tabla_Inf<-EE_Tabla_Inf[rev(order(EE_Tabla_Inf[,2])),]
  
  EE_Tabla_Inf[,4]<-cumsum(EE_Tabla_Inf[,3])
  EE_Tabla_Inf[,7]<-cumsum(EE_Tabla_Inf[,6])
  
  # View(EE_Tabla_Inf)
  
  # --------
  # Tabla por grado de riesgo con cartera vencida EX
  # --------
  
  EX_Referencias<-c("A-1", "A-2", "B-1", "B-2", "B-3", "C-1", "C-2", "D", "E", "Vencida")
  EX_Base_Ref<-factor(creditos[,8],EX_Referencias)
  xprueba<-as.factor(creditos[,19])
  elem_vig_ven<-factor("Vencida",c("Vencida","Vigente"))
  for(i in 1:length(EX_Base_Ref)) if(identical(xprueba[i],elem_vig_ven[1])) EX_Base_Ref[i]<-elem_vig_ven[1]
  
  EX_Base_Ref_Fact<-factor(EX_Base_Ref,levels = EX_Referencias)
  
  EX_ExI<-creditos[,4]
  EX_Res<-creditos[,7]
  EX_PIP<-creditos[,16]
  EX_SPP<-creditos[,17]
  
  x1<-tapply(EX_ExI,EX_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(EX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EX_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(EX_ExI,EX_Base_Ref_Fact,sum)/sum(EX_ExI)
  xp<-x2
  for(i in 1:length(EX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EX_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(EX_Res,EX_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(EX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EX_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(EX_Res,EX_Base_Ref_Fact,sum)/sum(EX_Res)
  xp<-x5
  for(i in 1:length(EX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EX_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(EX_PIP,EX_Base_Ref_Fact,sum)/tapply(EX_ExI,EX_Base_Ref_Fact,sum)
  x8<-tapply(EX_SPP,EX_Base_Ref_Fact,sum)/tapply(EX_ExI,EX_Base_Ref_Fact,sum)
  x9<-tapply(EX_Res,EX_Base_Ref_Fact,sum)/tapply(EX_ExI,EX_Base_Ref_Fact,sum)
  x10<-rep("Grado_Riesgo_V",length(EX_Referencias))
  x11<-rep(Periodo,length(EX_Referencias))
  x0<-EX_Referencias
  
  xp<-x7
  for(i in 1:length(EX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EX_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(EX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EX_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(EX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(EX_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  EX_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(EX_Tabla_Inf)<-c(Nombres_tab)
  
  EX_Tabla_Inf<-EX_Tabla_Inf[rev(order(EX_Tabla_Inf[,2])),]
  
  EX_Tabla_Inf[,4]<-cumsum(EX_Tabla_Inf[,3])
  EX_Tabla_Inf[,7]<-cumsum(EX_Tabla_Inf[,6])
  
  # View(EX_Tabla_Inf)
  
  # --------
  # Tabla por gerencia regional FF
  # --------
  
  FF_Referencias<-c(sort(unique(creditos[,21])))
  FF_Base_Ref<-creditos[,21]
  FF_Base_Ref_Fact<-factor(FF_Base_Ref,levels = FF_Referencias)
  
  FF_ExI<-creditos[,4]
  FF_Res<-creditos[,7]
  FF_PIP<-creditos[,16]
  FF_SPP<-creditos[,17]
  
  x1<-tapply(FF_ExI,FF_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(FF_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FF_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(FF_ExI,FF_Base_Ref_Fact,sum)/sum(FF_ExI)
  xp<-x2
  for(i in 1:length(FF_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FF_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(FF_Res,FF_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(FF_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FF_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(FF_Res,FF_Base_Ref_Fact,sum)/sum(FF_Res)
  xp<-x5
  for(i in 1:length(FF_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FF_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(FF_PIP,FF_Base_Ref_Fact,sum)/tapply(FF_ExI,FF_Base_Ref_Fact,sum)
  x8<-tapply(FF_SPP,FF_Base_Ref_Fact,sum)/tapply(FF_ExI,FF_Base_Ref_Fact,sum)
  x9<-tapply(FF_Res,FF_Base_Ref_Fact,sum)/tapply(FF_ExI,FF_Base_Ref_Fact,sum)
  x10<-rep("Gerencia_Regional",length(FF_Referencias))
  x11<-rep(Periodo,length(FF_Referencias))
  x0<-FF_Referencias
  
  xp<-x7
  for(i in 1:length(FF_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FF_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(FF_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FF_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(FF_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FF_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  FF_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(FF_Tabla_Inf)<-c(Nombres_tab)
  
  FF_Tabla_Inf<-FF_Tabla_Inf[rev(order(FF_Tabla_Inf[,2])),]
  
  FF_Tabla_Inf[,4]<-cumsum(FF_Tabla_Inf[,3])
  FF_Tabla_Inf[,7]<-cumsum(FF_Tabla_Inf[,6])
  
  # View(FF_Tabla_Inf)
  
  # --------
  # Tabla por gerencia regional con cartera vencida FX
  # --------
  
  FX_Referencias<-c(sort(unique(creditos[,21])), "Vencida")
  FX_Base_Ref<-factor(creditos[,21],FX_Referencias)
  xprueba<-as.factor(creditos[,19])
  elem_vig_ven<-factor("Vencida",c("Vencida","Vigente"))
  for(i in 1:length(FX_Base_Ref)) if(identical(xprueba[i],elem_vig_ven[1])) FX_Base_Ref[i]<-elem_vig_ven[1]
  
  FX_Base_Ref_Fact<-factor(FX_Base_Ref,levels = FX_Referencias)
  
  FX_ExI<-creditos[,4]
  FX_Res<-creditos[,7]
  FX_PIP<-creditos[,16]
  FX_SPP<-creditos[,17]
  
  x1<-tapply(FX_ExI,FX_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(FX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FX_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(FX_ExI,FX_Base_Ref_Fact,sum)/sum(FX_ExI)
  xp<-x2
  for(i in 1:length(FX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FX_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(FX_Res,FX_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(FX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FX_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(FX_Res,FX_Base_Ref_Fact,sum)/sum(FX_Res)
  xp<-x5
  for(i in 1:length(FX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FX_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(FX_PIP,FX_Base_Ref_Fact,sum)/tapply(FX_ExI,FX_Base_Ref_Fact,sum)
  x8<-tapply(FX_SPP,FX_Base_Ref_Fact,sum)/tapply(FX_ExI,FX_Base_Ref_Fact,sum)
  x9<-tapply(FX_Res,FX_Base_Ref_Fact,sum)/tapply(FX_ExI,FX_Base_Ref_Fact,sum)
  x10<-rep("Gerencia_Regional_V",length(FX_Referencias))
  x11<-rep(Periodo,length(FX_Referencias))
  x0<-FX_Referencias
  
  xp<-x7
  for(i in 1:length(FX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FX_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(FX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FX_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(FX_Referencias)){xp[i]<-NA} 
  for(i in 1:length(FX_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  FX_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(FX_Tabla_Inf)<-c(Nombres_tab)
  
  FX_Tabla_Inf<-FX_Tabla_Inf[rev(order(FX_Tabla_Inf[,2])),]
  
  FX_Tabla_Inf[,4]<-cumsum(FX_Tabla_Inf[,3])
  FX_Tabla_Inf[,7]<-cumsum(FX_Tabla_Inf[,6])
  
  # View(FX_Tabla_Inf)
  
  
  # ---------------------------------------------------------------------------
  # Tabla del acreditados por periodo GG
  # ---------------------------------------------------------------------------
  
  GG_Referencias<-c(unique(creditos[,2])) 
  GG_Base_Ref<-creditos[,2]               
  GG_Base_Ref_Fact<-factor(GG_Base_Ref,levels = GG_Referencias) 
  
  GG_ExI<-creditos[,4]     
  GG_Res<-creditos[,7]     
  GG_PIP<-creditos[,16]    
  GG_SPP<-creditos[,17]    
  
  x1<-tapply(GG_ExI,GG_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(GG_Referencias)){xp[i]<-NA} 
  for(i in 1:length(GG_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(GG_ExI,GG_Base_Ref_Fact,sum)/sum(GG_ExI)
  xp<-x2
  for(i in 1:length(GG_Referencias)){xp[i]<-NA} 
  for(i in 1:length(GG_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(GG_Res,GG_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(GG_Referencias)){xp[i]<-NA} 
  for(i in 1:length(GG_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(GG_Res,GG_Base_Ref_Fact,sum)/sum(GG_Res)
  xp<-x5
  for(i in 1:length(GG_Referencias)){xp[i]<-NA} 
  for(i in 1:length(GG_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(GG_PIP,GG_Base_Ref_Fact,sum)/tapply(GG_ExI,GG_Base_Ref_Fact,sum)
  x8<-tapply(GG_SPP,GG_Base_Ref_Fact,sum)/tapply(GG_ExI,GG_Base_Ref_Fact,sum)
  x9<-tapply(GG_Res,GG_Base_Ref_Fact,sum)/tapply(GG_ExI,GG_Base_Ref_Fact,sum)
  x10<-rep("Acreditados",length(GG_Referencias))
  x11<-rep(Periodo,length(GG_Referencias))
  x0<-GG_Referencias
  
  xp<-x7
  for(i in 1:length(GG_Referencias)){xp[i]<-NA} 
  for(i in 1:length(GG_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(GG_Referencias)){xp[i]<-NA} 
  for(i in 1:length(GG_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(GG_Referencias)){xp[i]<-NA} 
  for(i in 1:length(GG_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  GG_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(GG_Tabla_Inf)<-c(Nombres_tab)
  
  # View(GG_Tabla_Inf)
  
  # ---------------------------------------------------------------------------
  # Tabla de los diez principales acreditados por Exposición al Incumplimiento HH
  # ---------------------------------------------------------------------------
  
  HH_Referencias<-c(unique(creditos[,2])) 
  HH_Referencias<-GG_Tabla_Inf[rev(order(GG_Tabla_Inf[,2])),1]
  HH_Base_Ref<-creditos[,2]               
  HH_Base_Ref_Fact<-factor(HH_Base_Ref,levels = HH_Referencias) 
  
  HH_ExI<-creditos[,4]     
  HH_Res<-creditos[,7]     
  HH_PIP<-creditos[,16]    
  HH_SPP<-creditos[,17]    
  
  x1<-tapply(HH_ExI,HH_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(HH_Referencias)){xp[i]<-NA} 
  for(i in 1:length(HH_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(HH_ExI,HH_Base_Ref_Fact,sum)/sum(HH_ExI)
  xp<-x2
  for(i in 1:length(HH_Referencias)){xp[i]<-NA} 
  for(i in 1:length(HH_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(HH_Res,HH_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(HH_Referencias)){xp[i]<-NA} 
  for(i in 1:length(HH_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(HH_Res,HH_Base_Ref_Fact,sum)/sum(HH_Res)
  xp<-x5
  for(i in 1:length(HH_Referencias)){xp[i]<-NA} 
  for(i in 1:length(HH_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(HH_PIP,HH_Base_Ref_Fact,sum)/tapply(HH_ExI,HH_Base_Ref_Fact,sum)
  x8<-tapply(HH_SPP,HH_Base_Ref_Fact,sum)/tapply(HH_ExI,HH_Base_Ref_Fact,sum)
  x9<-tapply(HH_Res,HH_Base_Ref_Fact,sum)/tapply(HH_ExI,HH_Base_Ref_Fact,sum)
  x10<-rep("Principales_10_EI",length(HH_Referencias))
  x11<-rep(Periodo,length(HH_Referencias))
  x0<-HH_Referencias
  
  xp<-x7
  for(i in 1:length(HH_Referencias)){xp[i]<-NA} 
  for(i in 1:length(HH_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(HH_Referencias)){xp[i]<-NA} 
  for(i in 1:length(HH_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(HH_Referencias)){xp[i]<-NA} 
  for(i in 1:length(HH_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  HH_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(HH_Tabla_Inf)<-c(Nombres_tab)
  
  HH_Tabla_Inf<-subset(HH_Tabla_Inf,HH_Tabla_Inf[,8]<1)
  HH_Tabla_Inf<-head(HH_Tabla_Inf,n=10)
  HH_Tabla_Inf[,4]<-cumsum(HH_Tabla_Inf[,3])
  HH_Tabla_Inf[,7]<-cumsum(HH_Tabla_Inf[,6])
  
  # View(HH_Tabla_Inf)
  
  
  # ---------------------------------------------------------------------------
  # Tabla de los diez principales acreditados por Reservas II
  # ---------------------------------------------------------------------------
  
  II_Referencias<-c(unique(creditos[,2])) 
  II_Referencias<-GG_Tabla_Inf[rev(order(GG_Tabla_Inf[,6])),1]
  II_Base_Ref<-creditos[,2]               
  II_Base_Ref_Fact<-factor(II_Base_Ref,levels = II_Referencias) 
  
  II_ExI<-creditos[,4]     
  II_Res<-creditos[,7]     
  II_PIP<-creditos[,16]    
  II_SPP<-creditos[,17]    
  
  x1<-tapply(II_ExI,II_Base_Ref_Fact,sum)
  xp<-x1
  for(i in 1:length(II_Referencias)){xp[i]<-NA} 
  for(i in 1:length(II_Referencias))  if(identical(x1[i],xp[i]))   x1[i]<-0
  
  x2<-tapply(II_ExI,II_Base_Ref_Fact,sum)/sum(II_ExI)
  xp<-x2
  for(i in 1:length(II_Referencias)){xp[i]<-NA} 
  for(i in 1:length(II_Referencias))  if(identical(x2[i],xp[i]))   x2[i]<-0
  
  x3<-cumsum(x2)
  
  x4<-tapply(II_Res,II_Base_Ref_Fact,sum)
  xp<-x4
  for(i in 1:length(II_Referencias)){xp[i]<-NA} 
  for(i in 1:length(II_Referencias))  if(identical(x4[i],xp[i]))   x4[i]<-0
  
  x5<-tapply(II_Res,II_Base_Ref_Fact,sum)/sum(II_Res)
  xp<-x5
  for(i in 1:length(II_Referencias)){xp[i]<-NA} 
  for(i in 1:length(II_Referencias))  if(identical(x5[i],xp[i]))   x5[i]<-0
  
  x6<-cumsum(x5)
  
  x7<-tapply(II_PIP,II_Base_Ref_Fact,sum)/tapply(II_ExI,II_Base_Ref_Fact,sum)
  x8<-tapply(II_SPP,II_Base_Ref_Fact,sum)/tapply(II_ExI,II_Base_Ref_Fact,sum)
  x9<-tapply(II_Res,II_Base_Ref_Fact,sum)/tapply(II_ExI,II_Base_Ref_Fact,sum)
  x10<-rep("Principales_10_R",length(II_Referencias))
  x11<-rep(Periodo,length(II_Referencias))
  x0<-II_Referencias
  
  xp<-x7
  for(i in 1:length(II_Referencias)){xp[i]<-NA} 
  for(i in 1:length(II_Referencias))  if(identical(x7[i],xp[i]))   x7[i]<-0
  xp<-x8
  for(i in 1:length(II_Referencias)){xp[i]<-NA} 
  for(i in 1:length(II_Referencias))  if(identical(x8[i],xp[i]))   x8[i]<-0
  xp<-x9
  for(i in 1:length(II_Referencias)){xp[i]<-NA} 
  for(i in 1:length(II_Referencias))  if(identical(x9[i],xp[i]))   x9[i]<-0
  
  II_Tabla_Inf<-cbind.data.frame(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  
  names(II_Tabla_Inf)<-c(Nombres_tab)
  
  II_Tabla_Inf<-subset(II_Tabla_Inf,II_Tabla_Inf[,8]<1)
  II_Tabla_Inf<-head(II_Tabla_Inf,n=10)
  II_Tabla_Inf[,4]<-cumsum(II_Tabla_Inf[,3])
  II_Tabla_Inf[,7]<-cumsum(II_Tabla_Inf[,6])
  
  # View(II_Tabla_Inf)
  
  
  
  
  
  
  
  
  
  # --------
  # Incorparación de todas la tablas en una sola base y guardar en un archivo csv
  # --------
  ZZ_Tabla_Inf<-rbind.data.frame(AA_Tabla_Inf,
                                 BB_Tabla_Inf,
                                 BX_Tabla_Inf,
                                 CC_Tabla_Inf,
                                 CX_Tabla_Inf,
                                 DD_Tabla_Inf,
                                 DX_Tabla_Inf,
                                 DY_Tabla_Inf,
                                 DZ_Tabla_Inf,
                                 EE_Tabla_Inf,
                                 EX_Tabla_Inf,
                                 FF_Tabla_Inf,
                                 FX_Tabla_Inf,
                                 GG_Tabla_Inf,
                                 HH_Tabla_Inf,
                                 II_Tabla_Inf,make.row.names = F)
  
  # View(ZZ_Tabla_Inf)
  
  # View(AA_Tabla_Inf)
  # View(BB_Tabla_Inf)
  # View(BX_Tabla_Inf)
  # View(CC_Tabla_Inf)
  # View(CX_Tabla_Inf)
  # View(DD_Tabla_Inf)
  # View(DX_Tabla_Inf)
  # View(DY_Tabla_Inf)
  # View(DZ_Tabla_Inf)
  # View(EE_Tabla_Inf)
  # View(EX_Tabla_Inf)
  # View(FF_Tabla_Inf)
  # View(FX_Tabla_Inf)
  # View(GG_Tabla_Inf)
  # View(HH_Tabla_Inf)
  # View(II_Tabla_Inf)
  
  return_tabla_inf <- function(){
    ZZ_Tabla_Inf
  }
