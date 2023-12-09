# load compiled results
list_folder = dir('output/results/20230403/', pattern = 'results')
list_folder

results = lapply(1:length(list_folder), function(x){
  get(load(paste('output/results/20230403/', list_folder[x], sep = '')))
})

# load networks
list_folder = dir('output/results/20230403/', pattern = 'net')
list_folder

net = lapply(1:length(list_folder), function(x){
  get(load(paste('output/results/20230403/', list_folder[x], sep = '')))
})

# load nodelist
list_folder = dir('data/', pattern = 'nl')
list_folder

nl = lapply(1:length(list_folder), function(x){
  get(load(paste('data/', list_folder[x], sep = '')))
})



