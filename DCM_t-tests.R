library(tidyverse)
attach(t.tests_for_behavioral_DCM)
wilc(SpaSem ~ Group)

t.test(EngSem ~ Group)

t.test(SpaSemRT ~ Group)

t.test(EngSemRT ~ Group)

t.test(Size ~ Group)

t.test(SizeRT ~ Group)