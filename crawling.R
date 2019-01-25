# URL connection ----
rm(list = ls())
clist = vector('list', length = 1) 
p = url("https://www.mybudget.go.kr/natnFnnc/myIdeaList", open = 'r')
ch.vec = c() 
idx = 1 
while(TRUE) { 
  tmp = readLines(p,n=1, warn = FALSE, encoding = 'UTF-8') 
  if (length(tmp) == 0 ) break 
  ch.vec[idx] = tmp
  idx = idx + 1
}
close(p) 

# link sampling ----
idx = 1 
url = c() ; tmp1=c()

for (j in 1:length(ch.vec)) 
{ 
  tmp1= ch.vec[j] 
  v = grepl("/natnFnnc/myIdea?", tmp1) 
  if (v) { 
    a = regexpr('/natnFnnc/myIdea?', tmp1 ) 
    b = regexpr('Kind=', tmp1 )
    url[idx] = paste0('https://www.mybudget.go.kr',substr(tmp1,a,b+4))
    idx = idx + 1 
  }
}
url = url[8:109]

A = list() ; B = list()
for(num in 1:length(url)) {
  p=url(url[num],open='r')
  lnk=c()
  idx=1
  while(TRUE)
  {
    tmp = readLines(p,n=1,warn=FALSE,encoding='UTF-8')
    if(length(tmp)==0) break
    lnk[idx]=tmp
    idx=idx+1
  }
  close(p)
  
  # text resampling ----
  rec = FALSE ; rec2 = FALSE
  tmp0 = list() ; tmp2 = list()
  idx = 5 ; idx2 = 1 ; idx3 = 1 ; aa = c()
  for ( i in 1:length(lnk))
  {
    if ( grepl('<!-- 타이틀 -->', lnk[i]) ) {
      tmp0[1] = substr(lnk[i+4], regexpr('</span>',lnk[i+4])+8,nchar(lnk[i+4]))
      tmp0[2] = substr(lnk[i+12],regexpr('</b>',lnk[i+12])+5,regexpr('</span>',lnk[i+12])-1)
      tmp0[3] = substr(lnk[i+15],regexpr('</b>',lnk[i+15])+5,regexpr('</span>',lnk[i+15])-1)
      tmp0[4] = substr(lnk[i+20],regexpr('Bold',lnk[i+20])+8,regexpr('명',lnk[i+20])-5)
    }
    if ( grepl('</i> 사업내용</h4>', lnk[i]) ) {rec <- !rec ; j = i+2}
    if (rec) {
      tmp0[idx] = gsub("  "," ",lnk[j])
      idx = idx + 1; j=j+1
      if ( grepl('<!--a href=', lnk[j])  ) rec <- !rec
    }
    if(grepl('\\[제안자\\]', lnk[i])) {
      gg = substr(lnk[i],regexpr('">',lnk[i])+3,regexpr('</span>',lnk[i])-1)
      gg = gsub("<span>", " ",gg)
      tmp0[idx] = gg
      idx = idx + 1
    }
    if ( grepl('댓글 입력 창', lnk[i]) ) rec2 <- !rec2
    if ( rec2 ) {
      aa = substr(lnk[i], regexpr('창">',lnk[i])+3, nchar(lnk[i]))
      tmp2[idx2] = gsub('</textarea>', '', aa)
      idx2 = idx2 + 1
      if( grepl('</textarea>', lnk[i]) ) { rec2 <- !rec2 }
    }
  }
  A[num] = list(tmp0)
  B[num] = list(tmp2)
}
