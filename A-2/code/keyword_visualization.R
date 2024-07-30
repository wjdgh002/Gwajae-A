#R 프로그램 내 자료 삭제
rm(list=ls())
############################

# 로딩 경로, 저장 경로 지정
load_dir='C:/Users/wjdgh/OneDrive/바탕 화면/과제 A/A-2/excel/'
save_dir='C:/Users/wjdgh/OneDrive/바탕 화면/과제 A/A-2/picture/'
############################

#필요 패키지 로드
library(ggplot2)
library(dplyr)
############################

# 그림 형식 지정
theme=theme(axis.title.x = element_text(size = 45, face = "bold"),
        axis.title.y = element_text(size = 45, face = "bold"),
        axis.text.x=element_text(size=40,face="bold"),
        axis.text.y=element_text(size=40,face="bold"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5),legend.position='none')

names=c("main","post","comment")
title=c("게시글 제목","게시글 내용","댓글")

for (i in 1:3){
  # 데이터 로드
  eval(parse(text=paste0(names[i],"_count=read.csv(paste0(load_dir,'",names[i],"_count.csv'),header=F)")))

  # 키워드 빈도수 별 정렬 후 그림 생성
  eval(parse(text=paste0(names[i],"_count$V1=factor(",names[i],"_count$V1,levels=",names[i],"_count$V1)")))
  eval(parse(text=paste0(names[i],"_count_bar=ggplot(",names[i],"_count,aes(x=V1,y=V2,fill=V1))+geom_col()+coord_flip()+labs(title = '",title[i]," 키워드 빈도수', x = '키워드', y = '빈도수')+theme")))

  #그림 저장
  png(paste0(save_dir,names[i],'_count_bar.png'),width=480*3,height=480*2)
  eval(parse(text=paste0("print(",names[i],"_count_bar)")))
  dev.off()
}
