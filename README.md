# Heart failures and linear mixed effect models: 
## Quality of the hospitals and support to doctors’ decision 


#### Our Goal
Heart failure (HF) is a pathophysiological state in which the heart fails to supply required amount of blood and oxygen to the body; it is a common, widespread all over the world, costly, chronic and potentially fatal condition. 

The aim of our project is to analyze the impact of different hospital on the treatment of the HF and to build statistically significant models that can help doctors in the decision-making process. 


#### Exploratory and Fragility Analysis
The dataset, from Regione Lombardia, collects hospitalizations and medical histories from 2006 to 2012 in
Lombardia. It includes patients’ and hospitals’ characteristics as well as information
related to hospitalizations, such as clinical interventions, duration and diagnosis.

 

At first we did a medical research, analyzing the different diagnosis a doctor could make for a patient and then we assigned a level of risk, from the least dangerous to the most, to each diagnosis. To verify the significance of this classification we performed a one-way Manova test. 

 
#### QUALITY OF HOSPITALS – LOGISTIC REGRESSION FOR SURVIVAL RATE
Subsequently, to find out the quality of the hospital we used a logistic linear mixed effect models in which we considered all the patient’s story, data refer to a time interval of 3 years from the first hospitalization. To build our model we selected the hospitals which had a large volume of patients and we considered, as statistical units, the patients who always visited the same hospital. 

We studied the patient survival probability assigning 1 if patient survive and 0 otherwise and we used a random intercept to highlight the hospital effect. 

 
#### SUPPORT TO DOCTORS – LOGISTIC REGRESSION FOR REHOSPITALIZATION
Then we moved our attention to the level of single hospitalization, this examination is focused on helping doctors to handle the different hospitalizations; we estimated the probability of a future re-hospitalization and we studied the single hospitalization duration. 

 

In the case of re-hospitalization probability, we used a logistic linear mixed effect models based only on the data of the patient’s first hospitalization to predict if a patient will be re-hospitalized within 500 days. 

Practically this model can be thought to help doctors understanding how to treat the patient in the future, let us make this concept clearer with an example: 

Imagine that a specific patient has a 14 days long hospitalization, after this period a doctor has to decide how to continue the treatment and, with the support of our model, he not only takes this decision using his medical knowledge but also running a prediction based on our model. An estimated response of 0, no re-hospitalization, suggest that it will be more likely to be safe for the patient to go home.  Otherwise, the patient will need further attention and the doctor must decide whether to send him home with an eventual prescription and a future check or to continue the current hospitalization. 

#### DURATION OF A HOSPITALIZATION - NESTED MIXED EFFECTS LINEAR MODEL
Lastly, we investigated the duration of a single hospitalization; this can be useful in a decision-making process, with previous model information, and on an organization level because the hospital must know how long it has to reserve a room for a specific patient. 

We introduced a mixed effects linear models taking into account all the hospitalization in the 3 years after the first one; we considered heteroskedasticity among the residuals because we observed a different variability over time. Under our point of view, it makes sense that the variability is increasing in time, in fact the evolution of different diseases in not equal for everyone. 


##### Authors:
- Christopher Volpi
- Francesco Songia
- Giuseppe Tancredi Morra
- Niccolò Donadini
- Samuele Marchioni

 
