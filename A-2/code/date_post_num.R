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
            axis.text.x=element_text(size=40,face="bold",angle=45,hjust=1),
            axis.text.y=element_text(size=40,face="bold"),
            plot.title = element_text(size = 50, face = "bold", hjust = 0.5),legend.position='none')

# 데이터 로드
gwa=read.csv(paste0(load_dir,'gwa.csv'),header=F)
names(gwa)=c('date','view','recommend','comment_num','main','post','comments')

# 날짜별 게시글 수 추출
gwa_date=data.frame(gwa[,c(1,5:7)] %>% group_by(date) %>% summarise(post_num=n(),
                                                                    main=sum(main),
                                                                    post=sum(post),
                                                                    comments=sum(comments)))

# 날짜별 게시글 수 그림 생성
date_plot=ggplot(gwa_date,aes(x=date,y=post_num))+geom_line(linewidth=2)+labs(title='날짜별 게시글 수',x='날짜',y='게시글 수')+  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month")+theme

# 저장
png(paste0(save_dir,'date_gwa_line.png'),width=480*3,height=480*2)
print(date_plot)
dev.off()
