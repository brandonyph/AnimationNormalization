library(ggplot2)
library(nycflights13)

df <- flights[1:1000,]
data <- flights[1:1000,]

p<- ggplot(df, aes(x=arr_delay,y=distance))+ geom_jitter()
p 

df_f <- df[,c("dest","distance")]
df_f$distance <- log10(df_f$distance+1)
df_f_log<-as.data.frame(df_f)

p2<- ggplot(df_f_log, aes(x=arr_delay,y=distance))+ geom_jitter()
p2 


library(gganimate)

animate_y_log10 <- function(data=df,x_axis="air_time",y_axis="distance"){
  df <- c()
  df_tar <- c()
  df_calc <- data.frame()
  
  df2 <- data.frame()

  
  df$x <- data[,c(x_axis)]
  df$y <- data[,c(y_axis)]
  df$c <- rep(0,nrow(df$x))
    
  df_tar$y <- log10(data[,c(y_axis)]+1)
  
  diff_between_frame <- 10^(log10(df$y/df_tar$y)/20)
  df <- as.data.frame(df)
  
  df_calc <- cbind(df[,1],
                   df[,2]/(diff_between_frame),
                   rep(x=1,nrow(df)))
  
  colnames(df_calc) <- colnames(df)
  df2 <- rbind(df,df_calc)
  
  for(i in 2:20){
      df_calc <- cbind(df[,1],
                       df[,2]/(diff_between_frame^i),
                       rep(x=i,nrow(df)))
      colnames(df_calc) <- colnames(df)
      df2 <- rbind(df2,df_calc)
    }
  
 
  df2$c <- as.factor(df2$c)
  
  colnames(df2) <- c("X1","X2","Steps")
  
  p <- ggplot(df2, aes(x=X1,y=X2)) + 
        geom_jitter() +
        scale_y_continuous()+
        labs(
            x = x_axis,
            y = y_axis,
            title = "{closest_state}"
            )+
        transition_states(Steps, transition_length = 5, state_length =1) +
    view_follow(fixed_x = TRUE)
        
  return(p)
}

p <- animate_y_log10(data=df,x_axis="air_time",y_axis="distance")

animate(p, fps=24)
