---
title: "Exploratory Analysis"
author: "KD"
date: "27/10/2023"
output:
  html_document:
    keep_md: true
---

Užkrauname reikalingas bibliotekas


```r
library(tidyverse)
library(knitr)
```

Generuojant ataskaitą galima failo neskaityti kiekvieną kartą iš naujo - cache=TRUE. Nenorint klaidų/informacinių pranešimų pridedame message=FALSE ir warning=FALSE.


```r
df <- read_csv("../../../project/1-data/1-sample_data.csv")
```

Duomenų failo dimensijos:



```r
dim(df)
```

```
## [1] 1000000       9
```
# Kintamųjų apžvalga


(dėl gražesnio spaudinimo, naudojame funkciją kable() ir išdaliname kintamuosius į kelias eilutes)



```r
summary(df)
```

```
##        id                y       amount_current_loan     term          
##  Min.   :      1   Min.   :0.0   Min.   : 10802      Length:1000000    
##  1st Qu.: 250001   1st Qu.:0.0   1st Qu.:174394      Class :character  
##  Median : 500000   Median :0.5   Median :269676      Mode  :character  
##  Mean   : 500000   Mean   :0.5   Mean   :316659                        
##  3rd Qu.: 750000   3rd Qu.:1.0   3rd Qu.:435160                        
##  Max.   :1000000   Max.   :1.0   Max.   :789250                        
##                                                                        
##  credit_score       loan_purpose       yearly_income       home_ownership    
##  Length:1000000     Length:1000000     Min.   :    76627   Length:1000000    
##  Class :character   Class :character   1st Qu.:   825797   Class :character  
##  Mode  :character   Mode  :character   Median :  1148550   Mode  :character  
##                                        Mean   :  1344805                     
##                                        3rd Qu.:  1605899                     
##                                        Max.   :165557393                     
##                                        NA's   :219439                        
##   bankruptcies   
##  Min.   :0.0000  
##  1st Qu.:0.0000  
##  Median :0.0000  
##  Mean   :0.1192  
##  3rd Qu.:0.0000  
##  Max.   :7.0000  
##  NA's   :1805
```

Galutinėje ataskaitoje galime neįtraukti R kodo, naudojant echo=FALSE parametrą. 


|   |      id        |      y     |amount_current_loan |    term         |credit_score     |loan_purpose     |yearly_income     |home_ownership   | bankruptcies  |
|:--|:---------------|:-----------|:-------------------|:----------------|:----------------|:----------------|:-----------------|:----------------|:--------------|
|   |Min.   :      1 |Min.   :0.0 |Min.   : 10802      |Length:1000000   |Length:1000000   |Length:1000000   |Min.   :    76627 |Length:1000000   |Min.   :0.0000 |
|   |1st Qu.: 250001 |1st Qu.:0.0 |1st Qu.:174394      |Class :character |Class :character |Class :character |1st Qu.:   825797 |Class :character |1st Qu.:0.0000 |
|   |Median : 500000 |Median :0.5 |Median :269676      |Mode  :character |Mode  :character |Mode  :character |Median :  1148550 |Mode  :character |Median :0.0000 |
|   |Mean   : 500000 |Mean   :0.5 |Mean   :316659      |NA               |NA               |NA               |Mean   :  1344805 |NA               |Mean   :0.1192 |
|   |3rd Qu.: 750000 |3rd Qu.:1.0 |3rd Qu.:435160      |NA               |NA               |NA               |3rd Qu.:  1605899 |NA               |3rd Qu.:0.0000 |
|   |Max.   :1000000 |Max.   :1.0 |Max.   :789250      |NA               |NA               |NA               |Max.   :165557393 |NA               |Max.   :7.0000 |
|   |NA              |NA          |NA                  |NA               |NA               |NA               |NA's   :219439    |NA               |NA's   :1805   |


# TO DO

Apžvelgti NA reikšmes, y pasiskirstymą, character tipo kintamuosius panagrinėti detaliau.



```r
df$loan_purpose <- as.factor(df$loan_purpose)
df$y <- as.factor(df$y)
```



```r
summary(df$loan_purpose) %>%
  kable()
```



|                     |      x|
|:--------------------|------:|
|business_loan        |  17756|
|buy_a_car            |  11855|
|buy_house            |   6897|
|debt_consolidation   | 785428|
|educational_expenses |    992|
|home_improvements    |  57517|
|major_purchase       |   3727|
|medical_bills        |  11521|
|moving               |   1548|
|other                |  91481|
|renewable_energy     |    109|
|small_business       |   3242|
|take_a_trip          |   5632|
|vacation             |   1166|
|wedding              |   1129|


Arba:


```r
df %>%
  group_by(loan_purpose) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  kable()
```



|loan_purpose         |      n|
|:--------------------|------:|
|debt_consolidation   | 785428|
|other                |  91481|
|home_improvements    |  57517|
|business_loan        |  17756|
|buy_a_car            |  11855|
|medical_bills        |  11521|
|buy_house            |   6897|
|take_a_trip          |   5632|
|major_purchase       |   3727|
|small_business       |   3242|
|moving               |   1548|
|vacation             |   1166|
|wedding              |   1129|
|educational_expenses |    992|
|renewable_energy     |    109|

Pasirinkus kintamuosius juos vizualizuokite


```r
df %>%
  group_by(y, loan_purpose) %>%
  summarise(n = n()) %>%
  ggplot(aes(fill=y, y=n, x=loan_purpose)) + 
  geom_bar(position="dodge", stat="identity") + 
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  theme_dark()
```

![](report_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Daugiausiai banktotų imant paskolą šiems tikslams:


```r
df %>%
  filter(y == 1) %>%
  group_by(loan_purpose) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  kable()
```



|loan_purpose       |      n|
|:------------------|------:|
|debt_consolidation | 391875|
|other              |  44888|
|home_improvements  |  27274|
|business_loan      |  10356|
|medical_bills      |   6286|
|buy_a_car          |   5810|
|buy_house          |   3652|
|take_a_trip        |   2870|
|small_business     |   2152|
|major_purchase     |   2120|


# Papildomi pasiūlymai interaktyvumui pagerinti

Interaktyvios lentelės su datatable (DT)


```r
library(DT)
df %>%
  group_by(y, loan_purpose) %>%
  summarise(n = n()) %>%
  datatable()
```

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-4eb2ca18c19e4d2df11f" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-4eb2ca18c19e4d2df11f">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"],["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1"],["business_loan","buy_a_car","buy_house","debt_consolidation","educational_expenses","home_improvements","major_purchase","medical_bills","moving","other","renewable_energy","small_business","take_a_trip","vacation","wedding","business_loan","buy_a_car","buy_house","debt_consolidation","educational_expenses","home_improvements","major_purchase","medical_bills","moving","other","renewable_energy","small_business","take_a_trip","vacation","wedding"],[7400,6045,3245,393553,474,30243,1607,5235,664,46593,42,1090,2762,513,534,10356,5810,3652,391875,518,27274,2120,6286,884,44888,67,2152,2870,653,595]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>y<\/th>\n      <th>loan_purpose<\/th>\n      <th>n<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":3},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

Interaktyvūs grafikai su plotly


```r
library(plotly)
df %>%
  group_by(y, credit_score) %>%
  summarise(n = n()) %>%
  plot_ly(x = ~credit_score, y = ~n, name = ~y, type = "bar")
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-1b6629a895d92c757c8b" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-1b6629a895d92c757c8b">{"x":{"visdat":{"191d6fadfc97":["function () ","plotlyVisDat"]},"cur_data":"191d6fadfc97","attrs":{"191d6fadfc97":{"x":{},"y":{},"name":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"credit_score","type":"category","categoryorder":"array","categoryarray":["fair","good","very_good"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"n"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["fair","good","very_good"],"y":[26261,276478,91270],"name":"0","type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["fair","good","very_good"],"y":[33518,216691,41449],"name":"1","type":"bar","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```


