IPUMS_cc$HispStrokeFlag2 <- IPUMS_cc$HispStrokeFlag
IPUMS_cc$HispStrokeFlag2 <- ifelse(IPUMS_cc$HispStrokeFlag2==1, "Hispanic", "Non-Hispanic White")

out <- svyby(formula = ~WebUse, 
             by = ~HispStrokeFlag2, 
             design = svy2, 
             FUN = svymean, 
             na.rm = TRUE, 
             keep.names = FALSE)
#sig diff
WebUse_Plot <- ggplot(data =out , mapping = aes(x=HispStrokeFlag2, y=WebUse, fill = HispStrokeFlag2)) +
  geom_bar(stat="identity") + 
  labs(y="% who report web use") + scale_fill_manual(values=c("#CC0000", "#2D2926"), name = "Poststroke Subset") + 
  xlab("") +theme_minimal()+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))

WebUse_Plot
#####

#citizenship
out2 <- svyby(formula = ~Cit, 
             by = ~HispStrokeFlag2, 
             design = svy2, 
             FUN = svymean, 
             na.rm = TRUE, 
             keep.names = FALSE)
#sig diff
Cit_plot <- ggplot(data =out2 , mapping = aes(x=HispStrokeFlag2, y=Cit, fill = HispStrokeFlag2)) +
  geom_bar(stat="identity") + 
  labs(y="% citizen") + scale_fill_manual(values=c("#CC0000", "#2D2926"), name = "Poststroke Subset") + 
  xlab("") +theme_minimal() +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))



Cit_plot

out3 <- svyby(formula = ~PovertyAdj, 
             by = ~HispStrokeFlag2, 
             design = svy2, 
             FUN = svymean, 
             na.rm = TRUE, 
             keep.names = FALSE)
out3
#sig diff
poverty_plot <- ggplot(data =out3 , mapping = aes(x=HispStrokeFlag2, y=`PovertyAdj<100%`, fill = HispStrokeFlag2)) +
  geom_bar(stat="identity") + 
  labs(y="% below the poverty line") +scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_manual(values=c("#CC0000", "#2D2926"), name = "Poststroke Subset") + xlab("") +theme_minimal()

###

out4 <- svyby(formula = ~LANY_Bin, 
              by = ~HispStrokeFlag2, 
              design = svy2, 
              FUN = svymean, 
              na.rm = TRUE, 
              keep.names = FALSE)

#sig diff
lany_plot <- ggplot(data =out4 , mapping = aes(x=HispStrokeFlag2, y=LANY_Bin, fill = HispStrokeFlag2)) +
  geom_bar(stat="identity") + 
  labs(y="% who report any activity limitation") +scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_manual(values=c("#CC0000", "#2D2926"), name = "Poststroke Subset") + xlab("") +theme_minimal()
lany_plot

#p = .682

####
out5 <- svyby(formula = ~ComDiff, 
              by = ~HispStrokeFlag2, 
              design = svy2, 
              FUN = svymean, 
              na.rm = TRUE, 
              keep.names = FALSE)

#sig diff
com_plot <- ggplot(data =out5 , mapping = aes(x=HispStrokeFlag2, y=ComDiff, fill = HispStrokeFlag2)) +
  geom_bar(stat="identity") + 
  labs(y="% who report difficulty communicating") +scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_manual(values=c("#CC0000", "#2D2926"), name = "Poststroke Subset") + xlab("") +theme_minimal()

com_plot

#p=.161
###
out6 <- svyby(formula = ~Education, 
              by = ~HispStrokeFlag2, 
              design = svy2, 
              FUN = svymean, 
              na.rm = TRUE, 
              keep.names = FALSE)

edu_plot <- ggplot(data =out6 , mapping = aes(x=HispStrokeFlag2, y=Education, fill = HispStrokeFlag2)) +
  geom_bar(stat="identity") + 
  labs(y="% who report a HS degree/GED or greater") + scale_fill_manual(values=c("#CC0000", "#2D2926"), name = "Poststroke Subset") + 
  xlab("") +theme_minimal()+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))

edu_plot
