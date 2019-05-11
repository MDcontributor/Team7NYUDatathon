%Author: Kelly Ruggles
%This script analyzes the general demographic difference beteween the live and dead
%patients 

load populationadm

%%subject_id 
%%hadm_id
%%age
%%marital_status
%%ethnicity
%%gender
%expire flag
%language
%religion
%died

[r,c]=size(populationadm); 
dead=cell2mat(populationadm(2:r,7)); 
age=cell2mat(populationadm(2:r,3)); 
gender=populationadm(2:r,6);
race=populationadm(2:r,5); 
married=populationadm(2:r,4); 

indx_dead=find(dead==1); 
indx_live=find(dead==0); 
m=max(numel(indx_dead), numel(indx_live)); 

%Age dependences
age(age>90)=90; 
age_mat=nan(m,2); 
age_mat(1:numel(indx_dead), 1)=age(indx_dead); 
age_mat(1:numel(indx_live), 2)=age(indx_live); 
[h,p_age]=ttest2(age_mat(:,1), age_mat(:,2)); 
boxplot(age_mat, {'dead', 'alive'}); 
print(gcf, '-dpdf', 'age-box'); 
close all

indx_F=find(strcmp(gender, 'F')==1); 
indx_M=find(strcmp(gender, 'M')==1); 
indx_dF=find(dead(indx_F)==1); 
indx_dM=find(dead(indx_M)==1); 

%Age differences within gender
age_F=nan(m,1); 
age_M=nan(m,1); 
age_F(1:numel(indx_dF))=age(indx_dF);
age_M(1:numel(indx_dM))=age(indx_dM);
age_mat=nan(m,2); 
age_mat(1:numel(indx_dF), 1)=age(indx_dF); 
age_mat(1:numel(indx_dM), 2)=age(indx_dM); 
[h, p_gender_age]=ttest2(age_F, age_M); 
boxplot(age_mat, {'female', 'male'}); 
print(gcf, '-dpdf', 'age-gender-box'); 
close all

%Male vs. female
[p_gender, chistat]=chi2(numel(indx_dF), numel(indx_F), numel(indx_dM), numel(indx_M)); 
per_Female=numel(indx_dF)/numel(indx_F); 
per_Male=numel(indx_dM)/numel(indx_M); 
mat(1)=per_Female; 
mat(2)=per_Male; 
bar(mat); 
print(gcf, '-dpdf', 'gender-bar'); 
close all

%Married vs. single
indx_Mar=find(strcmp(married, 'MARRIED')==1); 
indx_NoMar=find(strcmp(married, 'SINGLE')==1); 
indx_dMar=find(dead(indx_Mar)==1); 
indx_dNoMar=find(dead(indx_NoMar)==1); 
[p_married, chistat]=chi2(numel(indx_dMar), numel(indx_Mar), numel(indx_dNoMar), numel(indx_NoMar)); 
per_Married=numel(indx_dMar)/numel(indx_Mar); 
per_Single=numel(indx_dNoMar)/numel(indx_NoMar); 
mat(1)=per_Married; 
mat(2)=per_Single; 
bar(mat); 
print(gcf, '-dpdf', 'married-bar'); 
close all
