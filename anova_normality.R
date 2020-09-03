library('lme4')

dat_example = data.frame(
    subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    btwn_X = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    btwn_Y = c(1, 2, 1, 2, 2, 1, 1, 1, 2, 1),
    measure_x1_yA = c(36.2, 45.2, 41, 24.6, 30.5, 28.2, 40.9, 45.1, 31, 16.9),
    measure_x2_yA = c(-14.1, 58.5,-25.5, 42.2,-13, 4.4, 55.5,-28.5, 25.6,-37.1),
    measure_x1_yB = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85),
    measure_x2_yB = c(8.024,-14.162, 3.1,-2.1,-1.5, 0.91, 11.53, 18.37, 0.3,-0.59),
    measure_x1_yC = c(27.4, -17.6, -32.7, 0.4, 37.2, 1.7, 18.2, 8.9, 1.9, 0.4),
    measure_x2_yC = c(7.7,-0.8, 2.2, 14.1, 22.1,-47.7,-4.8, 8.6, 6.2, 18.2)
)
dat_example$subject = as.factor(as.character(dat_example$subject))
dat_example$btwn_X = as.factor(as.character(dat_example$btwn_X))
dat_example$btwn_Y = as.factor(as.character(dat_example$btwn_Y))

vars = c(
    'measure_x1_yA',
    'measure_x2_yA',
    'measure_x1_yB',
    'measure_x2_yB',
    'measure_x1_yC',
    'measure_x2_yC'
)
dat_l = stats::reshape(
    dat_example,
    direction = 'long',
    varying = vars,
    idvar = 'subject',
    timevar = "within_factor",
    v.names = "values",
    times = vars
)
dat_l$wthn_X = sapply(strsplit(dat_l$within_factor, split = '_', fixed =
                                   TRUE), `[`, 2)
dat_l$wthn_Y = sapply(strsplit(dat_l$within_factor, split = '_', fixed =
                                   TRUE), `[`, 3)
dat_l$wthn_X = as.factor(as.character(dat_l$wthn_X))
dat_l$wthn_Y = as.factor(as.character(dat_l$wthn_Y))


# models ####

aov_B = aov(values ~ btwn_X, data=dat_l)
lmer_B = lmer(values ~ btwn_X+ (1|subject), data=dat_l)
aov_BB = aov(values ~ btwn_X*btwn_Y, data=dat_l)
lmer_BB = lmer(values ~ btwn_X*btwn_Y+ (1|subject), data=dat_l)
aov_W = aov(values ~ wthn_X + Error(subject/wthn_X), data=dat_l)
lmer_W = lmer(values ~ wthn_X + (1|subject), data=dat_l)
aov_WW = aov(values ~ wthn_X * wthn_Y + Error(subject / (wthn_X * wthn_Y)), data = dat_l)
lmer_WW = lmer(values ~ wthn_X * wthn_Y + (1 | subject) +
                         (1 | wthn_X:subject) + (1 | wthn_Y:subject), data = dat_l)
aov_BW = aov(values ~ btwn_X * wthn_X + Error(subject / wthn_X), data = dat_l)
lmer_BW = lmer(values ~ btwn_X * wthn_X + (1 | subject), data = dat_l)
aov_BBW = aov(values ~ btwn_X * btwn_Y * wthn_X + Error(subject / wthn_X),
              data = dat_l)
lmer_BBW = lmer(values ~ btwn_X * btwn_Y * wthn_X + (1 | subject), data = dat_l)
aov_BWW = aov(values ~ btwn_X * wthn_X * wthn_Y +
                  Error(subject / (wthn_X * wthn_Y)), data =
                  dat_l)
lmer_BWW = lmer(values ~ btwn_X * wthn_X * wthn_Y +
                    (1 | subject) + (1 | wthn_X:subject) + (1 | wthn_Y:subject),
                data = dat_l)
aov_BBWW = aov(values ~ btwn_X * btwn_Y * wthn_X * wthn_Y +
                   Error(subject / (wthn_X * wthn_Y)), data = dat_l)
lmer_BBWW = lmer(values ~ btwn_X * btwn_Y * wthn_X * wthn_Y +
                     (1 | subject) + (1 | wthn_X:subject) +
                     (1 | wthn_Y:subject), data = dat_l)

# results ####

lmer_B
lmer_BB
lmer_W
lmer_WW
lmer_BW
lmer_BBW
lmer_BWW
lmer_BBWW

aov_B
aov_BB
aov_W
aov_WW
aov_BW
aov_BBW
aov_BWW
aov_BBWW


summary(aov_WW)
anova(lmer_WW)
summary(aov_BW)
anova(lmer_BW)
summary(aov_BBW)
anova(lmer_BBW)
summary(aov_BWW)
anova(lmer_BWW)
summary(aov_BBWW)
anova(lmer_BBWW)

# plots

plot(aov_WW)
plot(lmer_WW)
plot(aov_BW)
plot(lmer_BW)
plot(aov_BBW)
plot(lmer_BBW)
plot(aov_BWW)
plot(lmer_BWW)
plot(aov_BBWW)
plot(lmer_BBWW)

# residuals ####

resid(lmer_B)
resid(lmer_BB)
resid(lmer_W)
resid(lmer_WW)
resid(lmer_BW)
resid(lmer_BBW)
resid(lmer_BWW)
resid(lmer_BBWW)


resid(aov_B)
resid(aov_BB)
resid(aov_W)
resid(aov_WW)
resid(aov_BW)
resid(aov_BBW)
resid(aov_BWW)
resid(aov_BBWW)

## aov VS ez

aov_BBWWaov = aov(values ~ btwn_X * btwn_Y * wthn_X * wthn_Y +
                   Error(subject / (wthn_X * wthn_Y)), data = dat_fk)

aov_BBWWaov$`subject:wthn_X:wthn_Y`$residuals

aov_BBWWez = ez::ezANOVA(
    data = dat_fk,
    dv = values,
    within = .(wthn_X, wthn_Y),
    between = .(btwn_X, btwn_Y),
    wid = subject, detailed = TRUE, return_aov = TRUE
)
aov_BBWWez$aov$`subject:wthn_X:wthn_Y`$residuals


# brief example ####

# preparing some invented data

dat_example = data.frame(
    subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    btwn_X = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    btwn_Y = c(1, 2, 1, 2, 2, 1, 1, 1, 2, 1),
    measure_x1_yA = c(36.2, 45.2, 41, 24.6, 30.5, 28.2, 40.9, 45.1, 31, 16.9),
    measure_x2_yA = c(-14.1, 58.5, -25.5, 42.2, -13, 4.4, 55.5, -28.5, 25.6, -37.1),
    measure_x1_yB = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85),
    measure_x2_yB = c(8.024, -14.162, 3.1, -2.1, -1.5, 0.91, 11.53, 18.37, 0.3, -0.59),
    measure_x1_yC = c(27.4,-17.6,-32.7, 0.4, 37.2, 1.7, 18.2, 8.9, 1.9, 0.4),
    measure_x2_yC = c(7.7, -0.8, 2.2, 14.1, 22.1, -47.7, -4.8, 8.6, 6.2, 18.2)
)
dat_example$subject = as.factor(as.character(dat_example$subject))
dat_example$btwn_X = as.factor(as.character(dat_example$btwn_X))
dat_example$btwn_Y = as.factor(as.character(dat_example$btwn_Y))

vars = c(
    'measure_x1_yA',
    'measure_x2_yA',
    'measure_x1_yB',
    'measure_x2_yB',
    'measure_x1_yC',
    'measure_x2_yC'
)
dat_l = stats::reshape(
    dat_example,
    direction = 'long',
    varying = vars,
    idvar = 'subject',
    timevar = "within_factor",
    v.names = "values",
    times = vars
)

dat_l$wthn_X = sapply(strsplit(dat_l$within_factor, split = '_', fixed =
                                   TRUE), `[`, 2)
dat_l$wthn_Y = sapply(strsplit(dat_l$within_factor, split = '_', fixed =
                                   TRUE), `[`, 3)
dat_l$wthn_X = as.factor(as.character(dat_l$wthn_X))
dat_l$wthn_Y = as.factor(as.character(dat_l$wthn_Y))

# performing ANOVA

aov_BBWW = aov(values ~ btwn_X * btwn_Y * wthn_X * wthn_Y +
                   Error(subject / (wthn_X * wthn_Y)), data = dat_l)

aov_BBWW$subject$residuals
aov_BBWW$`subject:wthn_X`$residuals
aov_BBWW$`subject:wthn_Y`$residuals
aov_BBWW$`subject:wthn_X:wthn_Y`$residuals
aov_BBWW$`(Intercept)`$residuals
