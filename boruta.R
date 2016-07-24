
library(Boruta)


df <- threes %>% 
     left_join(feats, 
               by = c('gameid' = 'gameid',
                      'event.id' = 'event.id')) %>% 
     .[complete.cases(.),]

model_df <- df %>% select(-c(lastnameshooter, 
                             teamName,
                             id,
                             EVENTNUM,
                             EVENTMSGACTIONTYPE,
                             event.id,
                             PLAYER1_ID,
                             gameid,
                             team_off,
                             team_def))


B <- Boruta(as.factor(EVENTMSGTYPE) ~ .,
            data = model_df,
            doTrace = 2,
            num.threads = 4 )

par(mar = c(9, 3, 1, 1))
plot(B, las = 2, xlab = '')