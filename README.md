# Temporal contact patterns and the implications for predicting superspreaders and planning of targeted outbreak control 

This repository contains code and data to analyse the epidemiological features of 11 real-world temporal networks by estimating (i) the reliability to predict 'superspreaders', (ii) the type of contacts that tend to be retained over time, (iii) the correlation of contacts over the days. Preprint can be found [here](https://www.medrxiv.org/content/10.1101/2023.11.22.23298919v1)

## Quick start guide
### Data and code for time varying contact pattern
Edgelist and list of nodes for high resolution contact data from four [cruises](https://www.nature.com/articles/s41467-022-29522-y), a [community in the UK](https://doi.org/10.1101/479154), three [high schools](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0107878), a [hospital](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0073970), two [workplaces](https://www.cambridge.org/core/journals/network-science/article/abs/data-on-facetoface-contacts-in-an-office-building-suggest-a-lowcost-vaccination-strategy-based-on-community-linkers/18AB49AB4F2AEA33CE7501F06ADBC8E8) were processed and can be found in:

> data/el_network_name.RData<br/>
> data/nl_network_name.RData<br/>

We estimated the median duration of contact between individuals and the median delay between contacts. We split the study duration of each network study into regular intervals with length corresponding to the median delay between contacts. We defined a valid contact to occur when the contact duration last for at least the median duration of contact within the time interval. Furthermore, we track the contacts over consecutive time steps. 

The code to compute the degree distribution in a single time step, the degree distribution over consecutive time steps, the distribution of retained contacts can be found in 

> `code/temporal_functions`<br/>
> `code/temporal_step1_generate_param.R`<br/>
> `code/temporal_step2_generate_networks.R`<br/>

### Quantifying the extent of superspreader and superspreading events
For each network and in each time step, we identified the individuals who accounted for the top 80% of the contacts. We then estimated the proportion of individuals identified for the correponding proportion of the time steps in the network study. This allowed us to determine the reliability in predicting potential 'superspreaders'. Relevant codes to perform this function are:

> `code/temporal_functions`<br/>
> `code/temporal_step3_combine_outputs.r` <br/>

#### Computing the retention index
In order to compare temporal networks relative to fully static and fully dynamic networks, we developed a new metric – the retention index – that allows complex dynamic networks to be summarised and compared in an epidemiologically meaningful manner. Across each level of the retention index, we studied the type of contacts that tend to be retained over consecutive time steps. If contacts tend to be retained within the same subpopulation, outbreak control measures should ideally target at this subpopulation. However, if the retained contacts are spread over different subpopulations, then outbreak control measures should aim to minimise disease introduction across all subpopulations. Relevant codes to perform this function are:

> `code/temporal_functions`<br/>
> `code/temporal_step3_combine_outputs.r` <br/>

#### Estimating the frequency of occurrence of each contact pairs, total and unique number of contacts in each day
We estimated the extent of repeated contacts over the study period to determine potential biases when outbreak models assume independent contacts. Relevant codes to perform this function are:

> `code/temporal_step4_n_contacts.r` <br/>

### Plots
R scripts to perform the plots are:

> `code/temporal_step5_load_results.R`<br/>
> `code/temporal_step6_plot.R`<br/>


If you plan to build on or cite this preliminary analysis for an academic publication, please ensure that you credit the underlying data sources above appropriately.
