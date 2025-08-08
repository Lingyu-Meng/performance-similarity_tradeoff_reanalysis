# plot group-level posterior densities of model parameters
#### load data and pkgs####
required_packages <- c("tidyverse", "rstan","posterior","bayesplot")

# Check and install missing packages
install_and_Load <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE) # Load the required packages
  }
}

install_and_Load(required_packages)

f = readRDS('m7b.Rdata')

#### plot, mean (short tick) + 95%HDI (shaded fill) ####

## general plotting config
tksz = 24 #ticks
ttsz = 26 #titles
tm_config = theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size=2),
        axis.ticks = element_line(color='black', size=1.5),
        axis.ticks.length=unit(.3, "cm"),
        axis.text.x  = element_text(color='black', face='bold',size=tksz),
        axis.text.y  = element_text(color='black', face='bold',size=tksz),
        axis.title.x = element_text(color='black', face='bold',size=ttsz),
        axis.title.y = element_text(color='black', face='bold',size=ttsz) )
#### Diagnostics ####
t1 <- stan_trace(f, inc_warmup = T)
more_para <- c('beta_mu[9]','beta_mu[10]','beta_mu[11]')
t2 <- stan_trace(f, inc_warmup = T, pars = more_para)
print(t1)
print(t2)
# chain 4 has some issues, so we will not use it
# delete it from the fit object
draws <- as_draws_array(f)
draws_clean <- draws[, 1:3, ]  # keep only chains 1–3

#### Choice1-related parameters ####
clr1 = '#478fc2'
clr2 = '#375f97'
clr3 = '#2A3132'

# Parameters to plot
s1 <- c('lr_mu', 'beta_mu[1]')

# Plot intervals
g1 <- mcmc_areas(
  draws_clean,
  pars = s1,
  prob = 0.95,       # Inner interval
  prob_outer = 0.99, # Outer interval
  point_est = "mean"
) +
  scale_fill_manual(values = clr1) +     # slab fill
  scale_color_manual(values = clr3) +    # line/point color
  tm_config() +                      
  labs(title = "", y = "", x = "") +
  scale_y_discrete(labels = rev(c(expression(alpha), expression(beta[V])))) +
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.08), labels = c('0','0.5','1'))

print(g1)

#### Choice2-related parameters ####
s2 = c('beta_mu[2]','beta_mu[3]','beta_mu[4]')

g2 <- mcmc_areas(
  draws_clean,
  pars = s2,
  prob = 0.95,       # Inner interval
  prob_outer = 0.99, # Outer interval
  point_est = "mean"
) +
  scale_fill_manual(values = clr2) +     # slab fill
  scale_color_manual(values = clr3) +    # line/point color
  tm_config +                      
  labs(title = "", y = "", x = "") +
  scale_y_discrete(labels = rev(c(expression(paste(beta, '(bias)')),
                                  expression(paste(beta, '(', italic('V')['chosen'], '-',  italic('V')['unchosen'], ')')),
                                  expression(paste(beta, '(', italic('w'), '.' , 'N'['against'], ')'))
                                  ))) +
  scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2.5,2))
print(g2)


g2 = stan_plot(f$fit, pars = s2, ci_level=.95,outer_level=.99,point_est='mean', show_density=T, fill_color=clr2, est_color=clr3)
g2 = g2 + tm_config + labs(title="",y="",x="") + 
  scale_y_continuous(breaks=1:4,	labels=rev(c(
    expression(paste(beta, '(bias)')),
    #expression(paste(beta, '(', italic('V')^{'chn'}, '-',  italic('V')^{'unchn'}, ')'       )),
    expression(paste(beta, '(', italic('V')['chosen'], '-',  italic('V')['unchosen'], ')'       )),
    expression(paste(beta, '(Bet1)')),
    expression(paste(beta, '(', italic('w'), '.' , 'N'['against'], ')'   ))
  )), limits=c(.8,4.8))	+
  scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2.5,2))
print(g2)

#### omega ####
clr1 = '#86ac41'
clr2 = '#486b00'
clr3 = '#2A3132'
s3 = c('beta_mu[11]')

g3 = mcmc_areas(
  draws_clean,
  pars = s3,
  prob = 0.95,       # Inner interval
  prob_outer = 0.99, # Outer interval
  point_est = "mean"
) +
  scale_fill_manual(values = clr1) +     # slab fill
  scale_color_manual(values = clr3) +    # line/point color
  tm_config +                      
  labs(title = "", y = "", x = "") +
  scale_y_discrete(labels = rev(c(expression(omega)))) +
  scale_x_continuous(breaks=c(-2,-1,0,1,2,3), limits=c(-2.0,3))
print(g3)

# plot 4 chains
#### Choice1-related parameters ####
clr1 = '#478fc2'
clr2 = '#375f97'
clr3 = '#2A3132'

a1 = stan_plot(f, pars = s1, ci_level=.95,outer_level=.99,point_est='mean',show_density=T, fill_color=clr1, est_color=clr3)
a1 = a1 + tm_config + labs(title="",y="",x="") + 
  scale_y_continuous(breaks=1:2,labels = rev(c(expression(alpha), expression(beta[V])))) +
  scale_x_continuous(breaks=c(0, .5, 1), limits=c(0,1.08), labels=c('0','0.5','1'))
print(a1)	    

#### Choice2-related parameters ####
a2 = stan_plot(f, pars = s2, ci_level=.95,outer_level=.99,point_est='mean', show_density=T, fill_color=clr2, est_color=clr3)
a2 = a2 + tm_config + labs(title="",y="",x="") + 
  scale_y_continuous(breaks=1:3,labels = rev(c(expression(paste(beta, '(bias)')),
                                  expression(paste(beta, '(', italic('V')['chosen'], '-',  italic('V')['unchosen'], ')')),
                                  expression(paste(beta, '(', italic('w'), '.' , 'N'['against'], ')'))
  ))) +
  scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2.5,2))
print(a2)

#### omega ####
a3 = stan_plot(f, pars = s3, ci_level=.95,outer_level=.99,point_est='mean', show_density=T, fill_color=clr1, est_color=clr3)
a3 = a3 + tm_config + labs(title="",y="",x="") + 
  scale_y_continuous(breaks=1:1,labels = rev(c(expression(omega)))) +
  scale_x_continuous(breaks=c(-2,-1,0,1,2,3), limits=c(-2.0,3))
print(a3)

#### save plots ####
dir.create('fig', showWarnings = FALSE)
ggsave('fig/m7b_choice1.png', g1, width = 8, height = 6, dpi = 300)
ggsave('fig/m7b_choice2.png', g2, width = 8, height = 6, dpi = 300)
ggsave('fig/m7b_omega.png', g3, width = 8, height = 6, dpi = 300)
ggsave('fig/m7b_choice1_4chains.png', a1, width = 8, height = 6, dpi = 300)
ggsave('fig/m7b_choice2_4chains.png', a2, width = 8, height = 6, dpi = 300)
ggsave('fig/m7b_omega_4chains.png', a3, width = 8, height = 6, dpi = 300)
ggsave('fig/m7b_trace.png', t1, width = 8, height = 6, dpi = 300)
ggsave('fig/m7b_trace_2.png', t2, width = 8, height = 3, dpi = 300)
