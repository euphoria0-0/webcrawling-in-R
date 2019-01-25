# URL connection ----
rm(list = ls()) # 11/05/02:05
i = 1 
pn.num = 1:2
f1.layer.list = vector('list', length = length(pn.num)) 
for ( i in 1:length(pn.num))
{ 
  cat(i,'\n') 
  p = url(paste("https://www1.president.go.kr/petitions?page=" 
                , pn.num[i], sep=''), open = 'r') 
  ch.vec = c() 
  idx = 1 
  while(TRUE) 
  { 
    tmp = readLines(p,n=1, warn = FALSE, encoding = 'UTF-8') 
    if (length(tmp) == 0 ) break 
    ch.vec[idx] = tmp
    idx = idx + 1
  }
  close(p) 
  f1.layer.list[[i]] = ch.vec 
}

# link sampling ----
idx = 1 
f1.layer.url = c() 
for ( i in 1:length(pn.num)) 
{ 
  tmp = f1.layer.list[[i]] 
  j = 1 
  for ( j in 1:length(tmp)) 
  { 
    tmp1= tmp[[j]] 
    v = grep("https://www1.president.go.kr/petitions/[0-9]{6}", tmp1 ) 
    #if (length(v)>0 ) break 
    if (length(v) > 0) 
    { 
      a = regexpr('https://', tmp1 ) 
      b = regexpr('class=', tmp1 )
      f1.layer.url[idx] = substr(tmp1,a,b-3) 
      idx = idx + 1 
    }
  }
}
rm=NULL ; rm2=NULL
for(i in 1:length(f1.layer.url)) {
  if(f1.layer.url[i]=="") rm = c(rm, i)
  if(grepl('best',f1.layer.url[i])) rm2 = c(rm2, i)
}
rm2
url0 = f1.layer.url[-c(rm,rm2)]

# link resampling & page download ----
tmp3 = list();tmp2=list();doc = list()
for(num in 1:length(url0)) {
  p=url(url0[num],open='r')
  ch.vec=c()
  idx=1
  while(TRUE)
  {
    tmp=readLines(p,n=1,warn=FALSE,encoding='UTF-8')
    if(length(tmp)==0) break
    ch.vec[idx]=tmp
    idx=idx+1
  }
  close(p)
  
  # text resampling ----
  rec = FALSE
  tmp1=list()
  idx = 1
  for ( i in 1:length(ch.vec))
  {
    xx = ch.vec[i]
    if( grepl('<title>', xx) ) {
      doc[[i]][[1]] = substr(xx, regexpr('<title>',xx)+7, regexpr('</title>', xx)-1)
    }
    if( grepl('참여인원', xx)) {
      doc[[i]][[2]] = substr(xx, regexpr('counter',xx)+3, regexpr('</span>', xx)-1)
    }
    if( grepl('카테고리', xx) ) {
      doc[[i]][[3]] = substr(xx, regexpr('</p>',xx)+4, regexpr('</li>', xx)-1)
      doc[[i]][[4]] = substr(xx, regexpr('</p>',xx)+4, regexpr('</li>', xx)-1)
    }
    v = grep('청원 본문', xx )
    if (length(v) > 0 ) rec <- !rec
    if (rec) {
      tmp1[idx] = ch.vec[[i]]
      idx = idx + 1
    }
  }
  kk = NULL
  for (i in 1:length(tmp1)) {
    if(grepl('<div class=\"View_write\" style=\"word-break:break-all\">',tmp1[i])) {
      jj = i
      for (j in i:length(tmp1)) {
        if(grepl('</div>$',tmp1[j])) kk = c(kk, j)
      }
    }
  }

  tmp2 = list(tmp1[(jj+1):(kk[1]-1)])
  
  for ( i in 1:length(tmp2[[1]]))
  {
    tmp2[[1]][[i]] = gsub('\\t','',tmp2[[1]][[i]])
    tmp2[[1]][[i]] = gsub('<br/>','',tmp2[[1]][[i]])
  }
  tmp3[[num]] = unlist(tmp2)
}
tmp3[[2]]
doc[1]

library(stringr)
library(KoNLP)
library(rJava)
library(tm)
library(wordcloud)
hwpCps = Corpus(VectorSource(tmp3))




hwpNoun = tm_map(hwpCps, extractNoun)
inspect(hwpNoun[1:5])
hwpTdm = TermDocumentMatrix(hwpCps)
dim(hwpTdm)
findFreqTerms(hwpTdm, lowfreq = 30)
wordcloud(hwpNoun, min.freq = 30, ordered.colors = T)

