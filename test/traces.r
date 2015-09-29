require(coda)
require(ggplot2)
require(plyr)

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

rosenbrocks = c(
    'mh-rosenbrock'
  , 'mh-radial-rosenbrock'
  , 'hmc-rosenbrock'
  , 'nuts-rosenbrock'
  , 'custom-rosenbrock'
  , 'random-rosenbrock'
  )

beales = c(
    'mh-beale'
  , 'mh-radial-beale'
  , 'hmc-beale'
  , 'nuts-beale'
  , 'custom-beale'
  , 'random-beale'
  )

himmelblaus = c(
    'mh-himmelblau'
  , 'mh-radial-himmelblau'
  , 'hmc-himmelblau'
  , 'nuts-himmelblau'
  , 'custom-himmelblau'
  , 'random-himmelblau'
  )

bnns = c(
    'mh-bnn'
  , 'mh-radial-bnn'
  , 'hmc-bnn'
  , 'nuts-bnn'
  , 'custom-bnn'
  , 'random-bnn'
  )

collater = function() {
  l = list()
  for (j in 1:length(handles)) {
    d = read.csv(paste(handles[j], 'dat', sep = '.'), header = F,
          colClasses = c('numeric', 'numeric'))
    d[,3] = 1:nrow(d)
    d[,4] = rep(handles[j], nrow(d))
    names(d) = c('x', 'y', 'epoch', 'label')
    l[[j]] = d
    }

  collated = ldply(l, data.frame)
  collated[,4] = as.factor(collated[,4])
  return(collated)
  }

l = collater()

rosenbrock = l[l$label %in% rosenbrocks,]
beale      = l[l$label %in% beales,]
himmelblau = l[l$label %in% himmelblaus,]
bnn        = l[l$label %in% bnns,]

worker = function(target, d) {
  pdf(paste(target, 'pdf', sep = '.'))
    p = ggplot(d, aes(x, y)) + geom_point(aes(colour = epoch), alpha = 0.2)
    g = p + facet_wrap(~ label)
    print(g)
  dev.off()
  }

worker('rosenbrock', rosenbrock)
worker('beale', beale)
worker('himmelblau', himmelblau)
worker('bnn', bnn)






# worker = function(handle) {
#   d = read.csv(paste(handle, "dat", sep = "."),
#         header = F, colClasses = c('numeric', 'numeric'))
#   d[,3] = 1:nrow(d)
#   names(d) = c('x', 'y', 'epoch')
#   pdf(paste(handle, "pdf", sep = "."))
#     p = ggplot(d, aes(x, y)) + geom_point(aes(colour = epoch), alpha = 0.2)
#     print(p)
#     dev.off()
#   m = mcmc(data = d[,c(1,2)])
#   e = effectiveSize(m)
#   print(paste(handle, e))
#   }



# for (handle in handles) worker(handle)

# so: want to specify ranges of handles
# for each range, load all the (classified) data and



