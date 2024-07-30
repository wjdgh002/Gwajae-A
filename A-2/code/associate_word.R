#R 프로그램 내 자료 삭제
rm(list=ls())
############################

# 로딩 경로, 저장 경로 지정
load_dir='C:/Users/wjdgh/OneDrive/바탕 화면/과제 A/A-2/excel/'
save_dir='C:/Users/wjdgh/OneDrive/바탕 화면/과제 A/A-2/picture/'
############################

#필요 패키지 로드
library(dplyr)
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)
library(stringr)
############################

# 단어 분리 함수
associate_word=function(word,support=0.01,confidence=0.2){
  word_list=strsplit(word," ")
  word_transaction=as(word_list,"transactions")
  summary(word_transaction)
  
  return(apriori(word_transaction,parameter = list(support=support,confidence=confidence,minlen=2)))
}

# 연관 분석 후 그림 생성 함수
associate_plot=function(word,method="lift",top){
  apri=associate_word(word=word)
  top=min(length(apri@lhs@data@i),top)  
  apri1=head(sort(apri, by=method), top)
  
  ig=as_data_frame(associations2igraph(apri1),what='both')

  i=nrow(ig$edges)-top
  start=ig$edges[1:i,]
  
  names(start)=c('from','toto')
  end=ig$edges[(i+1):nrow(ig$edges),]
  names(end)=c('toto','to')
  tmp=ig$vertices
  end$cnt=na.omit(tmp[,'count'])
  
  edge=unique(merge(start,end)[,2:4])
  tmp1=edge[,c(1,3)] ; names(tmp1)=c('index','value')
  tmp2=edge[,c(2,3)] ; names(tmp2)=c('index','value')
  size_fix=data.frame(rbind(tmp1,tmp2) %>% group_by(index) %>% summarise(value=sum(value)))
  
  nodes=merge(ig$vertices[is.na(ig$vertices$support),c('index','label')],size_fix)
  
  names(nodes)=c('id','label','value')
  nodes$title=ifelse(nodes$label=="",nodes$id,nodes$label)
  edges=unique(edge[,1:2])
  
  visNetwork(nodes, edges) %>%visOptions(highlightNearest = TRUE)
}
############################

# 자료 로드
posts=read.csv(paste0(load_dir,'posts.csv'),header=F)
names(posts)=c('date','view','recommend','comment_num','main','post','comments')

#게시글 제목, 내용, 댓글을 조합하여 변수 생성
posts$main_post=paste(posts$main,posts$post)
posts$post_comments=paste(posts$post,posts$comments)
posts$main_comments=paste(posts$main,posts$comments)
posts$all=paste(posts$main,posts$post,posts$comments)

# 상품 의미의 '패스'만 존재할 수 있도록 '패스', '수비'가 함께 존재하는 문장 제거
tf=(str_detect(posts$all,"패스")+str_detect(posts$all,"수비"))<2
posts=posts[tf,]

# 상품과 연관있는 단어가 최소 1개 이상 포함하는 문장만 추출
tf=(str_detect(posts$all,"과금")+str_detect(posts$all,"패스")+str_detect(posts$all,"상자")+str_detect(posts$all,"딸깍팩")+
    str_detect(posts$all,"열쇠")+str_detect(posts$all,"멤버쉽")+str_detect(posts$all,"BP")+str_detect(posts$all,"FC")+
    str_detect(posts$all,"빠칭코")+str_detect(posts$all,"추천")+str_detect(posts$all,"대")+str_detect(posts$all,"중")+
    str_detect(posts$all,"소")+str_detect(posts$all,"금고")+str_detect(posts$all,"MDL")+str_detect(posts$all,"위너스")+
    str_detect(posts$all,"추천"))>0
posts=posts[tf,]


# 연관 분석 후 그림 생성
associate_plot(posts$all,top=60,method="support")
