%Author: Kelly Ruggles
%This script using the cleaned table matrix with 157 features from the
%dataset and looked specificially to see if the clinical tests (BP,
%temperture etc) were changed between those that lived and died. 

load cleanedtable
load labels
load populationadm

%parse out the data to see who is alive and dead
[r1,c1]=size(populationadm); 
dead=cell2mat(populationadm(2:r1,10)); 
indx_dead=find(dead==1); 
indx_live=find(dead==0); 
m=max(numel(indx_dead), numel(indx_live)); 
names=cell2mat(populationadm(2:r1,1)); %sample names 
samples_dead=names(indx_dead); 
samples_live=names(indx_live);

%just extract the clinical data we are interested in
[r,c]=size(table); 
samples=cell2mat(table(2:r,1)); 
clinical=cell2mat(table(2:r,2:37)); %just get the clinical data
clinical_lab=labels(2:37); 

%identify dead patients in the new dataset
indx_dead2=double.empty; 
count=1; 
for i=1:numel(indx_dead)
    I=find(samples==samples_dead(i)); 
    if numel(I)>0
        indx_dead2(count)=I(1); 
        count=count+1; 
    end 
end 

%identify live patients in the new dataset
indx_live2=double.empty; 
count=1; 
for i=1:numel(indx_live)
    I=find(samples==samples_live(i)); 
    if numel(I)>0
        indx_live2(count)=I(1); 
        count=count+1; 
    end 
end 

%complete ttests for all of the comparisons
[r,c]=size(clinical); 
pmat=nan(c,1); 
up=zeros(c,1); 
m=max(numel(indx_live2), numel(indx_dead2)); 
mat_plot=double.empty; 
count=1; 
for i=1:c
    [h,p]=ttest2(clinical(indx_live2,i), clinical(indx_dead2,i)); 
    pmat(i)=p; 
    if nanmean(clinical(indx_live2,i)) > nanmean(clinical(indx_dead2,i))
        up(i)=1; 
    end 
    mat=nan(m,2); 

    mat(1:numel(indx_dead2),1)=clinical(indx_dead2,i); 
    mat(1:numel(indx_live2), 2)=clinical(indx_live2,i);
    mat_plot(count,1)=nanmean(clinical(indx_dead2,i));
    mat_plot(count,2)=nanmean(clinical(indx_live2,i));
    count=count+1; 
    %plot all of the data to look at once we do the statistical
    %corrections
    boxplot(mat_plot, {'dead', 'live'}); 
    print(gcf, '-dpdf', ['clinical-box-summary-' clinical_lab{i}]); 
end 

%Benjamini hochberg correction
[h, crit_p, adj_p]=fdr_bh(pmat);

% consider these to be significant! 
indx=find(adj_p<0.01); 
mat_new=zeros(r, numel(indx)); 
clin_lab_new=clinical_lab(indx); 

