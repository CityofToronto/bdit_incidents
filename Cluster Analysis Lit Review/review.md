# Incident Cluster Analysis Literature Review

Articles:
1. Karolien Geurts et. al. Clustering and profiling traffic roads by means of accident data
2.

## 1. Clustering and profiling traffic roads by means of accident data 
*Karolien Geurts et. al., Limburg University, 2003*

+ First part of the paper uses *Latent Class Clustering* to classify roads into groups based on similar incident frequencies
+ Second part of the paper profiles each cluster of roads based on available incident data

### 1.1 Latent Class Clustering

+ Observed incident frequencies are assumed to originate from a number of probability density functions, whose parameters are unknown
+ The size and number of road segments is also unknown
+ The objective of latent class clustering is to find the optimal parameters of the underlying distributions along with the size and number of segments given the underlying data
+ Incident frequency is modelled as a poisson distributed random variable, $Y$. Assume an incident rate of $\lambda$
+ An issue of this model is that the mean and variance of the poisson distribution are, by definition, identical ( $\lambda t$), but this is often not the case in the case of incident data sets where the variance may exceed the mean, known as *overdispertion* and is a product of an inconsistent poisson rate parameter across the sample and can be dealt with using the *finite mixture specification*
+ Assuming $k$ roads, where $p_{j}$ is the length of a road segment divided by the length of all segments, the PMF for incident rate can be expressed as $P[Y_{i}=y_{i}] = \sum_{j=1}^{k}P[Y_{i}=y_{i}|segment_{j}] * p_{j}$

