# Load data
TOM <- read.csv("all_subj_data_long_and_uniform_TOM_pwa.csv")
redcap <- read.csv("final_PCA_data_241003.csv")
computerized <- read.csv("all_subj_data_apr2025onward.csv")

library(dplyr)
###TOM - This code calculates average overall accuracy for the Theory of Mind task
TOM$accuracy <- ifelse(TOM$accuracy == TRUE, 1, 0)

TOM_long_RUK__FILLER <- subset(TOM, experiment == "ToM" & condition %in% c("long_RUK__FILLER"))
TOM_long_RUK__FILLER_avg_accuracy <- aggregate(accuracy ~ ID, data = TOM_long_RUK__FILLER, FUN = mean)
names(TOM_long_RUK__FILLER_avg_accuracy)[names(TOM_long_RUK__FILLER_avg_accuracy) == "accuracy"] <- "long_RUK__FILLER"

TOM_long_RUK__MC <- subset(TOM, experiment == "ToM" & condition %in% c("long_RUK__MC"))
TOM_long_RUK__MC_avg_accuracy <- aggregate(accuracy ~ ID, data = TOM_long_RUK__MC, FUN = mean)
names(TOM_long_RUK__MC_avg_accuracy)[names(TOM_long_RUK__MC_avg_accuracy) == "accuracy"] <- "long_RUK__MC"

TOM_long_RUK__TB <- subset(TOM, experiment == "ToM" & condition %in% c("long_RUK__TB"))
TOM_long_RUK__TB_avg_accuracy <- aggregate(accuracy ~ ID, data = TOM_long_RUK__TB, FUN = mean)
names(TOM_long_RUK__TB_avg_accuracy)[names(TOM_long_RUK__TB_avg_accuracy) == "accuracy"] <- "long_RUK__TB"

TOM_long_RUK__FB <- subset(TOM, experiment == "ToM" & condition %in% c("long_RUK__FB"))
TOM_long_RUK__FB_avg_accuracy <- aggregate(accuracy ~ ID, data = TOM_long_RUK__FB, FUN = mean)
names(TOM_long_RUK__FB_avg_accuracy)[names(TOM_long_RUK__FB_avg_accuracy) == "accuracy"] <- "long_RUK__FB"

TOM_RK__FILLER <- subset(TOM, experiment == "ToM" & condition %in% c("RK__FILLER"))
TOM_RK__FILLER_avg_accuracy <- aggregate(accuracy ~ ID, data = TOM_RK__FILLER, FUN = mean)
names(TOM_RK__FILLER_avg_accuracy)[names(TOM_RK__FILLER_avg_accuracy) == "accuracy"] <- "RK__FILLER"

TOM_RK__TB_MC <- subset(TOM, experiment == "ToM" & condition %in% c("RK__TB_MC"))
TOM_RK__TB_MC_avg_accuracy <- aggregate(accuracy ~ ID, data = TOM_RK__TB_MC, FUN = mean)
names(TOM_RK__TB_MC_avg_accuracy)[names(TOM_RK__TB_MC_avg_accuracy) == "accuracy"] <- "RK__TB_MC"

TOM_RK__FB <- subset(TOM, experiment == "ToM" & condition %in% c("RK__FB"))
TOM_RK__FB_avg_accuracy <- aggregate(accuracy ~ ID, data = TOM_RK__FB, FUN = mean)
names(TOM_RK__FB_avg_accuracy)[names(TOM_RK__FB_avg_accuracy) == "accuracy"] <- "RK__FB"

TOM_ALL <- full_join(TOM_long_RUK__FILLER_avg_accuracy, TOM_long_RUK__MC_avg_accuracy, by = "ID")
TOM_ALL <- full_join(TOM_ALL, TOM_long_RUK__TB_avg_accuracy, by = "ID")
TOM_ALL <- full_join(TOM_ALL, TOM_long_RUK__FB_avg_accuracy, by = "ID")
TOM_ALL <- full_join(TOM_ALL, TOM_RK__FILLER_avg_accuracy, by = "ID")
TOM_ALL <- full_join(TOM_ALL, TOM_RK__TB_MC_avg_accuracy, by = "ID")
TOM_ALL <- full_join(TOM_ALL, TOM_RK__FB_avg_accuracy, by = "ID")

colnames(TOM_ALL)[colnames(TOM_ALL) == "long_RUK__FILLER"] <- "TOM_long_RUK__FILLER"
colnames(TOM_ALL)[colnames(TOM_ALL) == "long_RUK__MC"] <- "TOM_long_RUK__MC"
colnames(TOM_ALL)[colnames(TOM_ALL) == "long_RUK__TB"] <- "TOM_long_RUK__TB"
colnames(TOM_ALL)[colnames(TOM_ALL) == "long_RUK__FB"] <- "TOM_long_RUK__FB"
colnames(TOM_ALL)[colnames(TOM_ALL) == "RK__FILLER"] <- "TOM_RK__FILLER"
colnames(TOM_ALL)[colnames(TOM_ALL) == "RK__TB"] <- "TOM_RK__TB"
colnames(TOM_ALL)[colnames(TOM_ALL) == "RK__TB_MC"] <- "TOM_RK__TB_MC"
colnames(TOM_ALL)[colnames(TOM_ALL) == "RK__FB"] <- "TOM_RK__FB"


###Redcap 
redcap <- subset(redcap, select = -redcap_event_name)
redcap <- subset(redcap, select = -syntax_total)

###Computerized
computerized$accuracy <- ifelse(computerized$accuracy == "True", 1, 0)

#colorshape
colorshape_cong <- subset(computerized, experiment == "colorshape" & condition %in% c("congruent"))
colorshape_cong_avg_accuracy <- aggregate(accuracy ~ ID, data = colorshape_cong, FUN = mean)
names(colorshape_cong_avg_accuracy)[names(colorshape_cong_avg_accuracy) == "accuracy"] <- "congruent"

colorshape_incong <- subset(computerized, experiment == "colorshape" & condition %in% c("incongruent"))
colorshape_incong_avg_accuracy <- aggregate(accuracy ~ ID, data = colorshape_incong, FUN = mean)
names(colorshape_incong_avg_accuracy)[names(colorshape_incong_avg_accuracy) == "accuracy"] <- "incongruent"

colorshape <- merge(colorshape_cong_avg_accuracy, colorshape_incong_avg_accuracy, by = "ID")

colnames(colorshape)[colnames(colorshape) == "congruent"] <- "colorshape_congruent"
colnames(colorshape)[colnames(colorshape) == "incongruent"] <- "colorshape_incongruent"

#fish_flanker
fish_flanker_cong <- subset(computerized, experiment == "fish_flanker" & condition %in% c("congruent"))
fish_flanker_cong_avg_accuracy <- aggregate(accuracy ~ ID, data = fish_flanker_cong, FUN = mean)
names(fish_flanker_cong_avg_accuracy)[names(fish_flanker_cong_avg_accuracy) == "accuracy"] <- "congruent"

fish_flanker_incong <- subset(computerized, experiment == "fish_flanker" & condition %in% c("incongruent"))
fish_flanker_incong_avg_accuracy <- aggregate(accuracy ~ ID, data = fish_flanker_incong, FUN = mean)
names(fish_flanker_incong_avg_accuracy)[names(fish_flanker_incong_avg_accuracy) == "accuracy"] <- "incongruent"

fish_flanker <- merge(fish_flanker_cong_avg_accuracy, fish_flanker_incong_avg_accuracy, by = "ID")

colnames(fish_flanker)[colnames(fish_flanker) == "congruent"] <- "fish_flanker_congruent"
colnames(fish_flanker)[colnames(fish_flanker) == "incongruent"] <- "fish_flanker_incongruent"

#geometric_inclusion
geometric_inclusion_included <- subset(computerized, experiment == "geometric_inclusion" & condition %in% c("matching", "shape included"))
geometric_inclusion_included_avg_accuracy <- aggregate(accuracy ~ ID, data = geometric_inclusion_included, FUN = mean)
names(geometric_inclusion_included_avg_accuracy)[names(geometric_inclusion_included_avg_accuracy) == "accuracy"] <- "included"

geometric_inclusion_notincluded <- subset(computerized, experiment == "geometric_inclusion" & condition %in% c("not matching", "shape not included","no shape included"))
geometric_inclusion_notincluded_avg_accuracy <- aggregate(accuracy ~ ID, data = geometric_inclusion_notincluded, FUN = mean)
names(geometric_inclusion_notincluded_avg_accuracy)[names(geometric_inclusion_notincluded_avg_accuracy) == "accuracy"] <- "notincluded"

geometric_inclusion <- merge(geometric_inclusion_included_avg_accuracy, geometric_inclusion_notincluded_avg_accuracy, by = "ID")

colnames(geometric_inclusion)[colnames(geometric_inclusion) == "included"] <- "geometric_inclusion_included"
colnames(geometric_inclusion)[colnames(geometric_inclusion) == "notincluded"] <- "geometric_inclusion_notincluded"

#geometric_matching
geometric_matching_matching <- subset(computerized, experiment == "geometric_matching" & condition %in% c("matching"))
geometric_matching_matching_avg_accuracy <- aggregate(accuracy ~ ID, data = geometric_matching_matching, FUN = mean)
names(geometric_matching_matching_avg_accuracy)[names(geometric_matching_matching_avg_accuracy) == "accuracy"] <- "matching"

geometric_matching_notmatching <- subset(computerized, experiment == "geometric_matching" & condition %in% c("not matching"))
geometric_matching_notmatching_avg_accuracy <- aggregate(accuracy ~ ID, data = geometric_matching_notmatching, FUN = mean)
names(geometric_matching_notmatching_avg_accuracy)[names(geometric_matching_notmatching_avg_accuracy) == "accuracy"] <- "notmatching"

geometric_matching <- merge(geometric_matching_matching_avg_accuracy, geometric_matching_notmatching_avg_accuracy, by = "ID")

colnames(geometric_matching)[colnames(geometric_matching) == "matching"] <- "geometric_matching_matching"
colnames(geometric_matching)[colnames(geometric_matching) == "notmatching"] <- "geometric_matching_notmatching"

#low level audition pure
lowlevel_pure1 <- subset(computerized, experiment == "lowlevel_pure" & condition %in% c("1.0"))
lowlevel_pure1_avg_accuracy <- aggregate(accuracy ~ ID, data = lowlevel_pure1, FUN = mean)
names(lowlevel_pure1_avg_accuracy)[names(lowlevel_pure1_avg_accuracy) == "accuracy"] <- "1"

lowlevel_pure2 <- subset(computerized, experiment == "lowlevel_pure" & condition %in% c("2.0"))
lowlevel_pure2_avg_accuracy <- aggregate(accuracy ~ ID, data = lowlevel_pure2, FUN = mean)
names(lowlevel_pure2_avg_accuracy)[names(lowlevel_pure2_avg_accuracy) == "accuracy"] <- "2"

lowlevel_pure3 <- subset(computerized, experiment == "lowlevel_pure" & condition %in% c("3.0"))
lowlevel_pure3_avg_accuracy <- aggregate(accuracy ~ ID, data = lowlevel_pure3, FUN = mean)
names(lowlevel_pure3_avg_accuracy)[names(lowlevel_pure3_avg_accuracy) == "accuracy"] <- "3"

lowlevel_pure4 <- subset(computerized, experiment == "lowlevel_pure" & condition %in% c("4.0"))
lowlevel_pure4_avg_accuracy <- aggregate(accuracy ~ ID, data = lowlevel_pure4, FUN = mean)
names(lowlevel_pure4_avg_accuracy)[names(lowlevel_pure4_avg_accuracy) == "accuracy"] <- "4"

lowlevel_pure5 <- subset(computerized, experiment == "lowlevel_pure" & condition %in% c("5.0"))
lowlevel_pure5_avg_accuracy <- aggregate(accuracy ~ ID, data = lowlevel_pure5, FUN = mean)
names(lowlevel_pure5_avg_accuracy)[names(lowlevel_pure5_avg_accuracy) == "accuracy"] <- "5"

lowlevel_pure <- merge(lowlevel_pure1_avg_accuracy, lowlevel_pure2_avg_accuracy, by = "ID")
lowlevel_pure <- merge(lowlevel_pure, lowlevel_pure3_avg_accuracy, by = "ID")
lowlevel_pure <- merge(lowlevel_pure, lowlevel_pure4_avg_accuracy, by = "ID")
lowlevel_pure <- full_join(lowlevel_pure, lowlevel_pure5_avg_accuracy, by = "ID")

colnames(lowlevel_pure)[colnames(lowlevel_pure) == "1"] <- "lowlevel_pure1"
colnames(lowlevel_pure)[colnames(lowlevel_pure) == "2"] <- "lowlevel_pure2"
colnames(lowlevel_pure)[colnames(lowlevel_pure) == "3"] <- "lowlevel_pure3"
colnames(lowlevel_pure)[colnames(lowlevel_pure) == "4"] <- "lowlevel_pure4"
colnames(lowlevel_pure)[colnames(lowlevel_pure) == "5"] <- "lowlevel_pure5"

#low level audition complex
lowlevel_complex1 <- subset(computerized, experiment == "lowlevel_complex" & condition %in% c("1.0"))
lowlevel_complex1_avg_accuracy <- aggregate(accuracy ~ ID, data = lowlevel_complex1, FUN = mean)
names(lowlevel_complex1_avg_accuracy)[names(lowlevel_complex1_avg_accuracy) == "accuracy"] <- "1"

lowlevel_complex2 <- subset(computerized, experiment == "lowlevel_complex" & condition %in% c("2.0"))
lowlevel_complex2_avg_accuracy <- aggregate(accuracy ~ ID, data = lowlevel_complex2, FUN = mean)
names(lowlevel_complex2_avg_accuracy)[names(lowlevel_complex2_avg_accuracy) == "accuracy"] <- "2"

lowlevel_complex3 <- subset(computerized, experiment == "lowlevel_complex" & condition %in% c("3.0"))
lowlevel_complex3_avg_accuracy <- aggregate(accuracy ~ ID, data = lowlevel_complex3, FUN = mean)
names(lowlevel_complex3_avg_accuracy)[names(lowlevel_complex3_avg_accuracy) == "accuracy"] <- "3"

lowlevel_complex4 <- subset(computerized, experiment == "lowlevel_complex" & condition %in% c("4.0"))
lowlevel_complex4_avg_accuracy <- aggregate(accuracy ~ ID, data = lowlevel_complex4, FUN = mean)
names(lowlevel_complex4_avg_accuracy)[names(lowlevel_complex4_avg_accuracy) == "accuracy"] <- "4"

lowlevel_complex5 <- subset(computerized, experiment == "lowlevel_complex" & condition %in% c("5.0"))
lowlevel_complex5_avg_accuracy <- aggregate(accuracy ~ ID, data = lowlevel_complex5, FUN = mean)
names(lowlevel_complex5_avg_accuracy)[names(lowlevel_complex5_avg_accuracy) == "accuracy"] <- "5"

lowlevel_complex <- merge(lowlevel_complex1_avg_accuracy, lowlevel_complex2_avg_accuracy, by = "ID")
lowlevel_complex <- merge(lowlevel_complex, lowlevel_complex3_avg_accuracy, by = "ID")
lowlevel_complex <- merge(lowlevel_complex, lowlevel_complex4_avg_accuracy, by = "ID")
lowlevel_complex <- full_join(lowlevel_complex, lowlevel_complex5_avg_accuracy, by = "ID")

colnames(lowlevel_complex)[colnames(lowlevel_complex) == "1"] <- "lowlevel_complex1"
colnames(lowlevel_complex)[colnames(lowlevel_complex) == "2"] <- "lowlevel_complex2"
colnames(lowlevel_complex)[colnames(lowlevel_complex) == "3"] <- "lowlevel_complex3"
colnames(lowlevel_complex)[colnames(lowlevel_complex) == "4"] <- "lowlevel_complex4"
colnames(lowlevel_complex)[colnames(lowlevel_complex) == "5"] <- "lowlevel_complex5"

# MEC_emot_comprehension
MEC_emot_comprehension_sad <- subset(computerized, experiment == "MEC_emot_comprehension" & condition %in% c("sad"))
MEC_emot_comprehension_sad_avg_accuracy <- aggregate(accuracy ~ ID, data = MEC_emot_comprehension_sad, FUN = mean)
names(MEC_emot_comprehension_sad_avg_accuracy)[names(MEC_emot_comprehension_sad_avg_accuracy) == "accuracy"] <- "sad"

MEC_emot_comprehension_angry <- subset(computerized, experiment == "MEC_emot_comprehension" & condition %in% c("angry"))
MEC_emot_comprehension_angry_avg_accuracy <- aggregate(accuracy ~ ID, data = MEC_emot_comprehension_angry, FUN = mean)
names(MEC_emot_comprehension_angry_avg_accuracy)[names(MEC_emot_comprehension_angry_avg_accuracy) == "accuracy"] <- "angry"

MEC_emot_comprehension_happy <- subset(computerized, experiment == "MEC_emot_comprehension" & condition %in% c("happy"))
MEC_emot_comprehension_happy_avg_accuracy <- aggregate(accuracy ~ ID, data = MEC_emot_comprehension_happy, FUN = mean)
names(MEC_emot_comprehension_happy_avg_accuracy)[names(MEC_emot_comprehension_happy_avg_accuracy) == "accuracy"] <- "happy"

MEC_emot_comprehension <- merge(MEC_emot_comprehension_sad_avg_accuracy, MEC_emot_comprehension_angry_avg_accuracy, by = "ID")
MEC_emot_comprehension <- merge(MEC_emot_comprehension, MEC_emot_comprehension_happy_avg_accuracy, by = "ID")

colnames(MEC_emot_comprehension)[colnames(MEC_emot_comprehension) == "sad"] <- "MEC_emot_comprehension_sad"
colnames(MEC_emot_comprehension)[colnames(MEC_emot_comprehension) == "angry"] <- "MEC_emot_comprehension_angry"
colnames(MEC_emot_comprehension)[colnames(MEC_emot_comprehension) == "happy"] <- "MEC_emot_comprehension_happy"

#MEC_ling_comprehension
MEC_ling_comprehension_Question <- subset(computerized, experiment == "MEC_ling_comprehension" & condition %in% c("Question"))
MEC_ling_comprehension_Question_avg_accuracy <- aggregate(accuracy ~ ID, data = MEC_ling_comprehension_Question, FUN = mean)
names(MEC_ling_comprehension_Question_avg_accuracy)[names(MEC_ling_comprehension_Question_avg_accuracy) == "accuracy"] <- "Question"

MEC_ling_comprehension_Order <- subset(computerized, experiment == "MEC_ling_comprehension" & condition %in% c("Order"))
MEC_ling_comprehension_Order_avg_accuracy <- aggregate(accuracy ~ ID, data = MEC_ling_comprehension_Order, FUN = mean)
names(MEC_ling_comprehension_Order_avg_accuracy)[names(MEC_ling_comprehension_Order_avg_accuracy) == "accuracy"] <- "Order"

MEC_ling_comprehension_Statement <- subset(computerized, experiment == "MEC_ling_comprehension" & condition %in% c("Statement"))
MEC_ling_comprehension_Statement_avg_accuracy <- aggregate(accuracy ~ ID, data = MEC_ling_comprehension_Statement, FUN = mean)
names(MEC_ling_comprehension_Statement_avg_accuracy)[names(MEC_ling_comprehension_Statement_avg_accuracy) == "accuracy"] <- "Statement"

MEC_ling_comprehension <- merge(MEC_ling_comprehension_Question_avg_accuracy, MEC_ling_comprehension_Order_avg_accuracy, by = "ID")
MEC_ling_comprehension <- merge(MEC_ling_comprehension, MEC_ling_comprehension_Statement_avg_accuracy, by = "ID")

colnames(MEC_ling_comprehension)[colnames(MEC_ling_comprehension) == "Question"] <- "MEC_ling_comprehension_Question"
colnames(MEC_ling_comprehension)[colnames(MEC_ling_comprehension) == "Order"] <- "MEC_ling_comprehension_Order"
colnames(MEC_ling_comprehension)[colnames(MEC_ling_comprehension) == "Statement"] <- "MEC_ling_comprehension_Statement"

#meta-syntax
metasyntax_grammatical <- subset(computerized, experiment == "meta-syntax" & condition %in% c("grammatical"))
metasyntax_grammatical_avg_accuracy <- aggregate(accuracy ~ ID, data = metasyntax_grammatical, FUN = mean)
names(metasyntax_grammatical_avg_accuracy)[names(metasyntax_grammatical_avg_accuracy) == "accuracy"] <- "grammatical"

metasyntax_ungrammatical <- subset(computerized, experiment == "meta-syntax" & condition %in% c("ungrammatical"))
metasyntax_ungrammatical_avg_accuracy <- aggregate(accuracy ~ ID, data = metasyntax_ungrammatical, FUN = mean)
names(metasyntax_ungrammatical_avg_accuracy)[names(metasyntax_ungrammatical_avg_accuracy) == "accuracy"] <- "ungrammatical"

metasyntax <- merge(metasyntax_grammatical_avg_accuracy, metasyntax_ungrammatical_avg_accuracy, by = "ID")

colnames(metasyntax)[colnames(metasyntax) == "grammatical"] <- "metasyntax_grammatical"
colnames(metasyntax)[colnames(metasyntax) == "ungrammatical"] <- "metasyntax_ungrammatical"

#music 
music_good <- subset(computerized, experiment == "music" & condition %in% c("good"))
music_good_avg_accuracy <- aggregate(accuracy ~ ID, data = music_good, FUN = mean)
names(music_good_avg_accuracy)[names(music_good_avg_accuracy) == "accuracy"] <- "good"

music_sour <- subset(computerized, experiment == "music" & condition %in% c("sour"))
music_sour_avg_accuracy <- aggregate(accuracy ~ ID, data = music_sour, FUN = mean)
names(music_sour_avg_accuracy)[names(music_sour_avg_accuracy) == "accuracy"] <- "sour"

music <- merge(music_good_avg_accuracy, music_sour_avg_accuracy, by = "ID")

colnames(music)[colnames(music) == "good"] <- "music_good"
colnames(music)[colnames(music) == "sour"] <- "music_sour"

#digit_comp
digit_comp  <- subset(computerized, experiment == "digit_comp")
digit_comp_avg_accuracy  <- aggregate(accuracy ~ ID, data = digit_comp, FUN = mean)
names(digit_comp_avg_accuracy)[names(digit_comp_avg_accuracy) == "accuracy"] <- "digit_comp"

#reading_numbers
reading_numbers <- subset(computerized, experiment == "reading_numbers")
reading_numbers_avg_accuracy  <- aggregate(accuracy ~ ID, data = reading_numbers, FUN = mean)
names(reading_numbers_avg_accuracy)[names(reading_numbers_avg_accuracy) == "accuracy"] <- "reading_numbers"

#writing_numbers
writing_numbers <- subset(computerized, experiment == "writing_numbers")
writing_numbers_avg_accuracy  <- aggregate(accuracy ~ ID, data = writing_numbers, FUN = mean)
names(writing_numbers_avg_accuracy)[names(writing_numbers_avg_accuracy) == "accuracy"] <- "writing_numbers"

#mental_calculation
mental_calculation_multiplication <- subset(computerized, experiment == "mental_calculation" & condition %in% c("multiplication"))
mental_calculation_multiplication_avg_accuracy <- aggregate(accuracy ~ ID, data = mental_calculation_multiplication, FUN = mean)
names(mental_calculation_multiplication_avg_accuracy)[names(mental_calculation_multiplication_avg_accuracy) == "accuracy"] <- "multiplication"

mental_calculation_addition <- subset(computerized, experiment == "mental_calculation" & condition %in% c("addition"))
mental_calculation_addition_avg_accuracy <- aggregate(accuracy ~ ID, data = mental_calculation_addition, FUN = mean)
names(mental_calculation_addition_avg_accuracy)[names(mental_calculation_addition_avg_accuracy) == "accuracy"] <- "addition"

mental_calculation_subtraction <- subset(computerized, experiment == "mental_calculation" & condition %in% c("subtraction"))
mental_calculation_subtraction_avg_accuracy <- aggregate(accuracy ~ ID, data = mental_calculation_subtraction, FUN = mean)
names(mental_calculation_subtraction_avg_accuracy)[names(mental_calculation_subtraction_avg_accuracy) == "accuracy"] <- "subtraction"

mental_calculation <- full_join(mental_calculation_addition_avg_accuracy, mental_calculation_subtraction_avg_accuracy, by = "ID")
mental_calculation <- full_join(mental_calculation, mental_calculation_multiplication_avg_accuracy, by = "ID")

colnames(mental_calculation)[colnames(mental_calculation) == "multiplication"] <- "mental_calculation_multiplication"
colnames(mental_calculation)[colnames(mental_calculation) == "addition"] <- "mental_calculation_addition"
colnames(mental_calculation)[colnames(mental_calculation) == "subtraction"] <- "mental_calculation_subtraction"

#rules_and_principles
rules_and_principles_rules <- subset(computerized, experiment == "rules_and_principles" & condition %in% c("rules", "addition_identity", "subtraction_identity", "multiplication_identity","multiplication_by_zero"))
rules_and_principles_rules_avg_accuracy  <- aggregate(accuracy ~ ID, data = rules_and_principles_rules, FUN = mean)
names(rules_and_principles_rules_avg_accuracy)[names(rules_and_principles_rules_avg_accuracy) == "accuracy"] <- "rules"

rules_and_principles_principles <- subset(computerized, experiment == "rules_and_principles" & condition %in% c("principles","addition_commutation", "addition_algebra", "powers_of_ten","addition_unit","multiplication_commutation","multiplication_inverse","multiplication_expanded","multiplication_unit"))
rules_and_principles_principles_avg_accuracy  <- aggregate(accuracy ~ ID, data = rules_and_principles_principles, FUN = mean)
names(rules_and_principles_principles_avg_accuracy)[names(rules_and_principles_principles_avg_accuracy) == "accuracy"] <- "principles"

rules_and_principles <- full_join(rules_and_principles_rules_avg_accuracy, rules_and_principles_principles_avg_accuracy, by = "ID")

colnames(rules_and_principles)[colnames(rules_and_principles) == "rules"] <- "rules_and_principles_rules"
colnames(rules_and_principles)[colnames(rules_and_principles) == "principles"] <- "rules_and_principles_principles"

#written_operations
written_operations_multiplication <- subset(computerized, experiment == "written_operations" & condition %in% c("multiplication", "multiplication level 1", "multiplication level 2"))
written_operations_multiplication_avg_accuracy <- aggregate(accuracy ~ ID, data = written_operations_multiplication, FUN = mean)
names(written_operations_multiplication_avg_accuracy)[names(written_operations_multiplication_avg_accuracy) == "accuracy"] <- "multiplication"

written_operations_addition <- subset(computerized, experiment == "written_operations" & condition %in% c("addition", "addition level 1", "addition level 2"))
written_operations_addition_avg_accuracy <- aggregate(accuracy ~ ID, data = written_operations_addition, FUN = mean)
names(written_operations_addition_avg_accuracy)[names(written_operations_addition_avg_accuracy) == "accuracy"] <- "addition"

written_operations_subtraction <- subset(computerized, experiment == "written_operations" & condition %in% c("subtraction", "subtraction level 1", "subtraction level 2"))
written_operations_subtraction_avg_accuracy <- aggregate(accuracy ~ ID, data = written_operations_subtraction, FUN = mean)
names(written_operations_subtraction_avg_accuracy)[names(written_operations_subtraction_avg_accuracy) == "accuracy"] <- "subtraction"

written_operations <- full_join(written_operations_multiplication_avg_accuracy,written_operations_addition_avg_accuracy, by = "ID")
written_operations <- full_join(written_operations,written_operations_subtraction_avg_accuracy, by = "ID")

colnames(written_operations)[colnames(written_operations) == "multiplication"] <- "written_operations_multiplication"
colnames(written_operations)[colnames(written_operations) == "addition"] <- "written_operations_addition"
colnames(written_operations)[colnames(written_operations) == "subtraction"] <- "written_operations_subtraction"

#PEPSC_comp
PEPSC_comp_animal <- subset(computerized, experiment == "PEPS-C_comp" & condition %in% c("animal"))
PEPSC_comp_animal_avg_accuracy <- aggregate(accuracy ~ ID, data = PEPSC_comp_animal, FUN = mean)
names(PEPSC_comp_animal_avg_accuracy)[names(PEPSC_comp_animal_avg_accuracy) == "accuracy"] <- "animal"

PEPSC_comp_color <- subset(computerized, experiment == "PEPS-C_comp" & condition %in% c("color"))
PEPSC_comp_color_avg_accuracy <- aggregate(accuracy ~ ID, data = PEPSC_comp_color, FUN = mean)
names(PEPSC_comp_color_avg_accuracy)[names(PEPSC_comp_color_avg_accuracy) == "accuracy"] <- "color"

PEPSC_comp <- merge(PEPSC_comp_animal_avg_accuracy, PEPSC_comp_color_avg_accuracy, by = "ID")

colnames(PEPSC_comp)[colnames(PEPSC_comp) == "animal"] <- "PEPSC_comp_animal"
colnames(PEPSC_comp)[colnames(PEPSC_comp) == "color"] <- "PEPSC_comp_color"

#syntax_vis
syntax_vis_active <- subset(computerized, experiment == "syntax_vis" & condition %in% c("active"))
syntax_vis_active_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_vis_active, FUN = mean)
names(syntax_vis_active_avg_accuracy)[names(syntax_vis_active_avg_accuracy) == "accuracy"] <- "active"

syntax_vis_filler <- subset(computerized, experiment == "syntax_vis" & condition %in% c("filler"))
syntax_vis_filler_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_vis_filler, FUN = mean)
names(syntax_vis_filler_avg_accuracy)[names(syntax_vis_filler_avg_accuracy) == "accuracy"] <- "filler"

syntax_vis_SE_clefts <- subset(computerized, experiment == "syntax_vis" & condition %in% c("SE_clefts"))
syntax_vis_SE_clefts_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_vis_SE_clefts, FUN = mean)
names(syntax_vis_SE_clefts_avg_accuracy)[names(syntax_vis_SE_clefts_avg_accuracy) == "accuracy"] <- "SE_clefts"

syntax_vis_SRC <- subset(computerized, experiment == "syntax_vis" & condition %in% c("SRC"))
syntax_vis_SRC_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_vis_SRC, FUN = mean)
names(syntax_vis_SRC_avg_accuracy)[names(syntax_vis_SRC_avg_accuracy) == "accuracy"] <- "SRC"

syntax_vis_passive <- subset(computerized, experiment == "syntax_vis" & condition %in% c("passive"))
syntax_vis_passive_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_vis_passive, FUN = mean)
names(syntax_vis_passive_avg_accuracy)[names(syntax_vis_passive_avg_accuracy) == "accuracy"] <- "passive"

syntax_vis_ORC <- subset(computerized, experiment == "syntax_vis" & condition %in% c("ORC"))
syntax_vis_ORC_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_vis_ORC, FUN = mean)
names(syntax_vis_ORC_avg_accuracy)[names(syntax_vis_ORC_avg_accuracy) == "accuracy"] <- "ORC"

syntax_vis_OE_clefts <- subset(computerized, experiment == "syntax_vis" & condition %in% c("OE_clefts"))
syntax_vis_OE_clefts_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_vis_OE_clefts, FUN = mean)
names(syntax_vis_OE_clefts_avg_accuracy)[names(syntax_vis_OE_clefts_avg_accuracy) == "accuracy"] <- "OE_clefts"

syntax_vis <- merge(syntax_vis_active_avg_accuracy, syntax_vis_filler_avg_accuracy, by = "ID")
syntax_vis <- merge(syntax_vis, syntax_vis_SE_clefts_avg_accuracy, by = "ID")
syntax_vis <- merge(syntax_vis, syntax_vis_SRC_avg_accuracy, by = "ID")
syntax_vis <- merge(syntax_vis, syntax_vis_passive_avg_accuracy, by = "ID")
syntax_vis <- merge(syntax_vis, syntax_vis_OE_clefts_avg_accuracy, by = "ID")
syntax_vis <- merge(syntax_vis, syntax_vis_ORC_avg_accuracy, by = "ID")

colnames(syntax_vis)[colnames(syntax_vis) == "active"] <- "syntax_vis_active"
colnames(syntax_vis)[colnames(syntax_vis) == "filler"] <- "syntax_vis_filler"
colnames(syntax_vis)[colnames(syntax_vis) == "SE_clefts"] <- "syntax_vis_SE_clefts"
colnames(syntax_vis)[colnames(syntax_vis) == "SRC"] <- "syntax_vis_SRC"
colnames(syntax_vis)[colnames(syntax_vis) == "passive"] <- "syntax_vis_passive"
colnames(syntax_vis)[colnames(syntax_vis) == "OE_clefts"] <- "syntax_vis_OE_clefts"
colnames(syntax_vis)[colnames(syntax_vis) == "ORC"] <- "syntax_vis_ORC"

#syntax_aud
syntax_aud_active <- subset(computerized, experiment == "syntax_aud" & condition %in% c("active"))
syntax_aud_active_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_aud_active, FUN = mean)
names(syntax_aud_active_avg_accuracy)[names(syntax_aud_active_avg_accuracy) == "accuracy"] <- "active"

syntax_aud_filler <- subset(computerized, experiment == "syntax_aud" & condition %in% c("filler"))
syntax_aud_filler_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_aud_filler, FUN = mean)
names(syntax_aud_filler_avg_accuracy)[names(syntax_aud_filler_avg_accuracy) == "accuracy"] <- "filler"

syntax_aud_SE_clefts <- subset(computerized, experiment == "syntax_aud" & condition %in% c("SE_clefts"))
syntax_aud_SE_clefts_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_aud_SE_clefts, FUN = mean)
names(syntax_aud_SE_clefts_avg_accuracy)[names(syntax_aud_SE_clefts_avg_accuracy) == "accuracy"] <- "SE_clefts"

syntax_aud_SRC <- subset(computerized, experiment == "syntax_aud" & condition %in% c("SRC"))
syntax_aud_SRC_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_aud_SRC, FUN = mean)
names(syntax_aud_SRC_avg_accuracy)[names(syntax_aud_SRC_avg_accuracy) == "accuracy"] <- "SRC"

syntax_aud_passive <- subset(computerized, experiment == "syntax_aud" & condition %in% c("passive"))
syntax_aud_passive_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_aud_passive, FUN = mean)
names(syntax_aud_passive_avg_accuracy)[names(syntax_aud_passive_avg_accuracy) == "accuracy"] <- "passive"

syntax_aud_ORC <- subset(computerized, experiment == "syntax_aud" & condition %in% c("ORC"))
syntax_aud_ORC_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_aud_ORC, FUN = mean)
names(syntax_aud_ORC_avg_accuracy)[names(syntax_aud_ORC_avg_accuracy) == "accuracy"] <- "ORC"

syntax_aud_OE_clefts <- subset(computerized, experiment == "syntax_aud" & condition %in% c("OE_clefts"))
syntax_aud_OE_clefts_avg_accuracy <- aggregate(accuracy ~ ID, data = syntax_aud_OE_clefts, FUN = mean)
names(syntax_aud_OE_clefts_avg_accuracy)[names(syntax_aud_OE_clefts_avg_accuracy) == "accuracy"] <- "OE_clefts"

syntax_aud <- merge(syntax_aud_active_avg_accuracy, syntax_aud_filler_avg_accuracy, by = "ID")
syntax_aud <- merge(syntax_aud, syntax_aud_SE_clefts_avg_accuracy, by = "ID")
syntax_aud <- merge(syntax_aud, syntax_aud_SRC_avg_accuracy, by = "ID")
syntax_aud <- merge(syntax_aud, syntax_aud_passive_avg_accuracy, by = "ID")
syntax_aud <- merge(syntax_aud, syntax_aud_OE_clefts_avg_accuracy, by = "ID")
syntax_aud <- merge(syntax_aud, syntax_aud_ORC_avg_accuracy, by = "ID")

colnames(syntax_aud)[colnames(syntax_aud) == "active"] <- "syntax_aud_active"
colnames(syntax_aud)[colnames(syntax_aud) == "filler"] <- "syntax_aud_filler"
colnames(syntax_aud)[colnames(syntax_aud) == "SE_clefts"] <- "syntax_aud_SE_clefts"
colnames(syntax_aud)[colnames(syntax_aud) == "SRC"] <- "syntax_aud_SRC"
colnames(syntax_aud)[colnames(syntax_aud) == "passive"] <- "syntax_aud_passive"
colnames(syntax_aud)[colnames(syntax_aud) == "OE_clefts"] <- "syntax_aud_OE_clefts"
colnames(syntax_aud)[colnames(syntax_aud) == "ORC"] <- "syntax_aud_ORC"

#Merge everything together
library(purrr)

colnames(redcap)[colnames(redcap) == "record_id"] <- "ID"

master <- reduce(list(colorshape,digit_comp_avg_accuracy,fish_flanker,geometric_inclusion,
                      geometric_matching,lowlevel_complex,lowlevel_pure,
                      MEC_emot_comprehension,MEC_ling_comprehension,
                      mental_calculation,metasyntax,music,PEPSC_comp,
                      reading_numbers_avg_accuracy,rules_and_principles,
                      syntax_aud,syntax_vis,TOM_ALL,writing_numbers_avg_accuracy,
                      written_operations,redcap), function(x, y) merge(x, y, by = "ID", all = TRUE))


master <- subset(master, ID %in% c("BUMA032","BUMA020","BUMA078","BUMA220","BUMA164","BUMA204",
                                   "BUMA126","BUMA257","BUMA180","BUMA228","BUBA133","BUMA258",
                                   "BUMA130","BUBA150","BUMA273","BUBA149","BUMA275","BUMA280",
                                   "BUMA278","BUMA283","BUMA286","BUMA173","BUMA294","BUMA289",
                                   "BUMA296","BUMA197","BUMA303","BUMA306","BUMA277","BUMA302",
                                   "BUBA155","BUBA192","BUMA307","BUBA193","BUMA305","BUBA114",
                                   "BUMA290","BUMA207","BUMA320","BUMA317","BUMA321"))

wab <- read.csv("wabaq.csv")
wab <- wab[, !names(wab) %in% c("X", "X.1")]
                                         
master <- merge(master, wab, by = "ID")

write.csv(master, "master0416.csv", row.names = FALSE)