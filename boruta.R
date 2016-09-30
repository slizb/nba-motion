
library(Boruta)
library(ranger)
library(tidyr)
library(viridis)

set.seed(321)

B <- Boruta(Class ~ .,
            data = up_train,
            doTrace = 2,
            num.threads = 4 )
            #respect.unordered.factors = T)

par(mar = c(11.5, 3, 1, 1))
plot(B, las = 2, xlab = '')

var_labels <- c(distanceTraveled = 'Teamwork',     
                xDistanceTraveled = 'Teamwork',
                yDistanceTraveled = 'Teamwork',
                xSpread = 'Teamwork',                
                ySpread = 'Teamwork',                
                elapsedTime = 'Other',           
                timeInPaint = 'Other',             
                medDefenderDist = 'Defense',        
                maxDefenderDist = 'Defense',        
                offenseHeight = 'Matchup',          
                defenseHeight = 'Matchup',          
                offenseWeight = 'Matchup',         
                defenseWeight = 'Matchup',          
                defenderDistAtShot = 'Defense',    
                onBallHeightMismatch = 'Matchup',
                onBallWeightMismatch = 'Matchup',    
                onBallExperienceMismatch = 'Matchup',
                onBallPositionMatchup = 'Matchup',   
                numberPasses = 'Teamwork',           
                numberBallDefenders = 'Teamwork',
                teamMovement = 'Teamwork',            
                offenseChull = 'Teamwork',
                defenseChull = 'Defense',           
                chullDiff = 'Teamwork',              
                PLAYER1_ID = 'Other',             
                team_id = 'Other',              
                stephenCurry = 'Other',       
                shadowMax = 'Random',              
                shadowMean = 'Random',             
                shadowMin = 'Random') %>% 
     data.frame() 
var_labels$key <- rownames(var_labels)
colnames(var_labels)[1] <- 'Type'

var_order <- B$ImpHistory %>% 
     data.frame() %>% 
     gather() %>% 
     group_by(key) %>% 
     summarize(medval = median(value)) %>% 
     arrange(medval) %>% 
     .$key

df_b <- B$ImpHistory %>% 
     data.frame() %>% 
     gather() %>% 
     left_join(var_labels, by = 'key') %>% 
     mutate(key = factor(key, levels = var_order, ordered = T))

ggplot(df_b, aes(x = key, y = value, fill = Type)) +
     geom_boxplot() +
     theme_bw() +
     theme(text = element_text(size = 20),
           axis.text.x = element_text(angle = 45, hjust = 1)) +
     scale_fill_viridis(discrete = T, option = 'plasma') +
     labs(x = '',
          y = 'Importance')
     



