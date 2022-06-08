library(tidyverse)
library(readxl)
library(finalfit)
library(kableExtra)
library(ggpubr)
library(rgdal)
library(plotly)
library(lubridate)
library(DT)
library(shiny)
library(shinydashboard)
library(devtools)

# Server: ####
shinyServer(
  function(input, output) {
    base_franquias=read_xlsx("Dados/base_franquias.xlsx")
    demograficas=read_xlsx("Dados/demograficas.xlsx")
    distancia=read_csv2("Dados/distancia.csv")
    base_franquias=base_franquias %>% 
      drop_na()
    
    # Clientes por região: ####
    
    query=base_franquias %>% 
      group_by(cidade,referencia) %>% 
      summarise(clientes_ativos=sum(clientes_ativos)) %>% 
      left_join(demograficas) %>% 
      group_by(regiao,referencia) %>% 
      summarise(clientes_ativos=sum(clientes_ativos),
                pop=sum(soma_pop_total)) %>% 
      drop_na()
    p1=query %>% 
      mutate(prop=round(clientes_ativos*100000/pop)) %>% 
      ggplot(aes(referencia,prop,col=regiao))+
      geom_line(size=1)+
      labs(x="Data",y="Clientes ativos/100.000 hab.",title="Proporção de clientes ativos")+
      theme_bw()
    p2=query %>% 
      ggplot(aes(referencia,clientes_ativos,col=regiao))+
      geom_line(size=1)+
      labs(x="Data",y="Clientes ativos",title="Número absoluto de clientes ativos")+
      theme_bw()
    plot=ggarrange(p1,p2,nrow = 2)
    
    output$clientes_regiao=renderPlot(plot)
    
    # Mapa: ####
    
    latlon=readOGR(dsn="Dados",layer="BR_Localidades_2010_v1",verbose=F)
    latlon=as.tibble(latlon) %>% 
      mutate(cod_ibge=as.numeric(CD_GEOCODM)) %>% 
      select(cod_ibge,LAT,LONG) %>% 
      group_by(cod_ibge) %>% 
      summarise(LAT=mean(LAT,na.rm=T),
                LONG=mean(LONG,na.rm=T))
    
    query=base_franquias %>% 
      left_join(demograficas) %>% 
      left_join(latlon) %>% 
      drop_na()
    media=list(lon=mean(query$LONG),lat=mean(query$LAT))
    datas=unique(query$referencia)
    i=1
    steps=list()
    for(i in 1:length(datas)){
      dat=ymd(datas[i])
      steps[[i]]=list(args=list("transforms[0].value",dat),
                      label=dat,
                      value=as.character(i),
                      method="restyle")
    }
    
    p1=query %>% 
      plot_ly(transforms=list(
        list(
          type="filter",
          target=~referencia,
          operation="=",
          value=datas[1]
        )
      ),
      lat=~LAT,
      lon=~LONG,
      hovertext=~paste(cidade,"\n",
                       round(clientes_ativos*100000/soma_pop_total),
                       "Clientes/100.000hab."),
      marker=list(size=~clientes_ativos*50/soma_pop_total,
                  color=~clientes_ativos*50/soma_pop_total),
      type="scattermapbox")
    
    p1=p1 %>% 
      layout(title="Evolução da quantidade de clientes/100.000 hab. por cidade",
             sliders=list(list(active=0,
                               currentvalue=list(prefix="Data Referência: "),
                               steps=steps)),
             mapbox=list(
               style="white-bg",
               sourcetype="raster",
               zoom=2.8,
               center=media,
               layers = list(list(
                 below = 'traces',
                 sourcetype = "raster",
                 source = list("https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer/tile/{z}/{y}/{x}")),
                 list(
                   sourcetype = "raster",
                   source = list("https://geo.weather.gc.ca/geomet/?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&BBOX={bbox-epsg-3857}&CRS=EPSG:3857&WIDTH=1000&HEIGHT=1000&LAYERS=RADAR_1KM_RDBR&TILED=true&FORMAT=image/png"))))
      )
    output$mapa=renderPlotly(p1)
    
    # Franquias: ####
    
    query=base_franquias %>% 
      group_by(franquia,referencia) %>% 
      summarise(clientes=sum(clientes_ativos)) %>% 
      mutate(crescimento=round((clientes/lag(clientes)-1)*100,2),
             acumulado=round((clientes/clientes[clientes>0][1]-1)*100))
    f1=query %>% 
      summarise("Crescimento (%)"=last(acumulado),
                "Clientes (última referência)"=last(clientes)) %>% 
      arrange(desc(`Crescimento (%)`)) %>% 
      head(10) %>% 
      datatable(caption="Franquias que mais cresceram.",
                option=list(dom="t"))
    
    f2=query %>% 
      summarise("Crescimento (%)"=last(acumulado),
                "Clientes (última referência)"=last(clientes)) %>% 
      arrange(`Crescimento (%)`) %>% 
      head(10) %>% 
      datatable(caption="Franquias que menos cresceram.",
                option=list(dom="t"))
    
    f3=query %>% 
      summarise("Crescimento (%)"=last(acumulado),
                "Clientes (última referência)"=last(clientes)) %>% 
      arrange(desc(`Clientes (última referência)`)) %>% 
      head(10) %>% 
      datatable(caption="Maiores franquias em número de clientes.",
                option=list(dom="t"))
    
    output$franquias1=renderDataTable(f1)
    output$franquias2=renderDataTable(f2)
    output$franquias3=renderDataTable(f3)
    
    # desafio: ####
    
    d1=distancia %>% 
      group_by(vendedor,data_visita) %>% 
      mutate(data_visita=as.Date(data_visita,format="%d/%m/%Y"),
             lat=lat*pi/180,
             lon=lon*pi/180,
             deltaphi=lat-lag(lat),
             deltalambda=lon-lag(lon),
             a=sin(deltaphi/2)^2+cos(lag(lat))*cos(lat)*sin(deltalambda)^2,
             c=2*atan2(sqrt(a),sqrt(1-a)),
             dist=round(6371e3*c,2)) %>% 
      summarise(dist=sum(dist,na.rm=T)) %>% 
      group_by(vendedor) %>% 
      summarise("Distância"=round(mean(dist),2)) %>% 
      arrange(desc(`Distância`)) %>% 
      datatable(caption="Distância média percorrida por vendedor por dia em metros.")
    
    output$desafio=renderDataTable(d1)
    
    # Download relatório: ####
    
    withProgress(message="Gerando Relatório",value=1,{
      output$download <-downloadHandler(
        filename = "Descritiva.html",
        content = function(file) {
          tempReport <- file.path(tempdir(), "Descritiva.Rmd")
          file.copy("Descritiva.Rmd", tempReport, overwrite = TRUE)
          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            knit_root_dir = getwd(),
                            envir = new.env(parent = globalenv())
          )
        }
      )
    })
      
  }
)
