%Author: Kelly Ruggles
%This script using the prescription table to understand if there were any
%drugs specifically given to those that lived versus those that did not

load prescriptions
load populationadm

[r,c]=size(populationadm); 
dead=cell2mat(populationadm(2:r,10)); 
indx_dead=find(dead==1); 
indx_live=find(dead==0); 
m=max(numel(indx_dead), numel(indx_live)); 

hadm=populationadm(2:r,2); %sample names 

sample_dead=hadm(indx_dead); 
sample_live=hadm(indx_live); 

[r2,c2]=size(prescriptions); 
prescriptions_=prescriptions(2:r2,:); 
prescrip=prescriptions_(:,11); 
u_prescrip=unique(prescrip);

p_mat=zeros(r,numel(u_prescrip)); 

for i=1:numel(hadm)
    indx=find(hadm{i}==cell2mat(prescriptions_(:,3))); %sample match
    if numel(indx)>0
        P=prescrip(indx,:); 
        for j=1:numel(P)
            indx2=find(strcmp(P{j}, u_prescrip)==1); 
            p_mat(i,indx2)=1; 
        end 
    end 
end 

s=sum(p_mat,1); 
indx=find(s>r*0.10); 
p_mat_=p_mat(:,indx); 
u_p=u_prescrip(indx); 

cmap2=cbrewer('div', 'RdBu', 100);  
cmap2=flipud(cmap2);
% 
% cg=clustergram(p_mat_,'Colormap', cmap2, 'DisplayRange', 1);
% plot(cg);
% print(gcf, '-dpdf', 'prescription-summary'); 
% close all

%do some stats on these to see if there is anything different: 
[r3,c3]=size(p_mat_); 
pvals=nan(c3,1); 
up=zeros(c3,1); 
for i=1:c3
    yes_dead = numel(find(p_mat_(indx_dead,i)==1)); 
    yes_live = numel(find(p_mat_(indx_live, i)==1)); 
    n1 = numel(indx_dead); 
    n2 = numel(indx_live); 
    [p, chistat]=chi2(yes_dead, n1, yes_live, n2); 
    pvals(i)=p; 
    if yes_dead/n1 > yes_live/n2
        up(i)=1; 
    end 
end 

%benjamini hochberg correction
[h, crit_p, adj_p]=fdr_bh(pvals);

%only look at those that are SUPER signifncant 
indx=find(adj_p<0.000000001); 
mat_new=zeros(r, numel(indx)); 
p_mat_new=p_mat_(:,indx); 
up=up(indx); 
u_p_new=u_p(indx); 

%reorder the samples so we are looking at the live/dead in order
mat_new(1:numel(indx_live),:)=p_mat_new(indx_live,:); 
mat_new(numel(indx_live)+1:numel(indx_dead)+numel(indx_live), :)=p_mat_new(indx_dead,:); 
mat_live=p_mat_new(indx_live,:); 
mat_dead=p_mat_new(indx_dead,:); 

%reorder drugs to be up and down in the live vs. dead
indx_up=find(up==1); 
indx_dn=find(up==0); 
mat_new_(:,1:numel(indx_up))=mat_new(:,indx_up); 
mat_new_(:,numel(indx_up)+1:numel(indx_up)+numel(indx_dn))=-1*mat_new(:,indx_dn); 
u_p_new2(1:numel(indx_up))=u_p_new(indx_up); 
u_p_new2(numel(indx_up)+1:numel(indx_up)+numel(indx_dn))=u_p_new(indx_dn); 

%get better values for drugs
labels_final=cell(numel(u_p_new2),1); 
for i=1:numel(u_p_new2)
    indx=find(strcmp(prescriptions(:,11),u_p_new2{i})==1); 
    labels_final{i,1}=prescriptions{indx(1),8};
end 

%plot the data 
cg=clustergram(mat_new_, 'Cluster', 2, 'Colormap', cmap2, 'DisplayRange', 3, 'ColumnLabels', labels_final);
plot(cg);
print(gcf, '-dpdf', ['precription-summary-sig-e-9_v2']); 
close all

labels_cg=cg.ColumnLabels'; 

