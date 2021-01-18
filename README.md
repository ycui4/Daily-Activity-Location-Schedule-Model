# Daily-Activity-Location-Schedule-Model
This Git repository contains code for paper “Generating a Synthetic Probabilistic Daily Activity-Location Schedule using Large-Scale, Long-term and Low-Frequency Smartphone GPS data with Limited Activity Information”
This Git repository does not contain data. However, the attributes of our model are very common for most household travel surveys, including socio-demographics (household size, age, gender, and race) and trip/activity information (start/end location, start/end time, and trip purpose). And our method can handle missing trip/activity data.
To handle missing activity data, this research develops a new probabilistic approach, which measures the probability of visiting a place by three scores, global visit score (GVS), temporal visit score (TVS) and periodical visit score (PVS). Three different levels of activity-location schedule are modeled respectively. The first level handles only those data with known activities, while data with unknown activities are disregarded. The second takes unknown activities into account but combines all types of them as a single category. The third one models each location with unknown activities separately. These models are able to generate activity-location schedule in different levels of details for activity-based traffic simulator. After developing activity-location schedule models, both individual and aggregated validation processes are performed with simulation. The validation result shows that the simulated proportion of activity types and activity duration are close to the survey data, indicating the effectiveness of the proposed approaches.
This Git repository contains code of model and validation.
