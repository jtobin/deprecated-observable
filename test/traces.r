require(coda)
require(ggplot2)

handles = c(
    "mh-rosenbrock"
  , "mh-beale"
  , "mh-himmelblau"
  , "mh-bnn"

  , "mh-radial-rosenbrock"
  , "mh-radial-beale"
  , "mh-radial-himmelblau"
  , "mh-radial-bnn"

  , "hmc-rosenbrock"
  , "hmc-beale"
  , "hmc-himmelblau"
  , "hmc-bnn"

  , "nuts-rosenbrock"
  , "nuts-beale"
  , "nuts-himmelblau"
  , "nuts-bnn"

  , "custom-rosenbrock"
  , "custom-beale"
  , "custom-himmelblau"
  , "custom-bnn"

  , "random-rosenbrock"
  , "random-beale"
  , "random-himmelblau"
  , "random-bnn"

  , "annealed-rosenbrock"
  , "annealed-beale"
  , "annealed-himmelblau"
  , "annealed-bnn"

  )

worker = function(handle) {
  d = read.csv(paste(handle, "dat", sep = "."),
        header = F, colClasses = c('numeric', 'numeric'))
  d[,3] = 1:nrow(d)
  names(d) = c('x', 'y', 'epoch')
  pdf(paste(handle, "pdf", sep = "."))
    p = ggplot(d, aes(x, y)) + geom_point(aes(colour = epoch), alpha = 0.2)
    print(p)
    dev.off()
  m = mcmc(data = d[,c(1,2)])
  e = effectiveSize(m)
  print(paste(handle, e))
  }

for (handle in handles) worker(handle)



# consider ignoring 'jump' as it's similar to 'random'

# on some problems like the rosenbrock function the nuts transition performs
# quite well.  but comparable performance can be achieved by using less nuts
# transitions, such as in the 'custom' transition.
#
# however the effective sample size doesn't tell the whole story.  consider
# the beale density in which MH, HMC, and NUTS jump transitions report large
# effective sample sizes.  yet they fail to traverse to a nearby but disparate
# region of probability that is successfully located by the 'custom'
# transitions.  notably the 'random' transition - also failed to locate these
# regions.
#
# the custom and random transitions each do quite well on the BNN density.  most
# of the primitive transitions also perform well, but are less efficient.  mh
# does poorly, in particular.
#


