clear all
%loading files
load RCP3PD_MIDYR_CONC.txt
load RCP6_MIDYR_CONC.txt
load RCP45_MIDYR_CONC.txt
load RCP85_MIDYR_CONC.txt
load simple_model_data1880to2015.txt
load final_rcp26_2.txt;
load final_rcp45_2.txt;
load final_rcp60_2.txt;
load final_rcp85_2.txt;

%setting values for coefficients 
rco2 = 320;
tau = 4;
tau1 = 4;  
tau2 = 56;  
a = 1.21;   
b = 0.12;
c = -1.68;
d = 0.03;

%creating arrays of data from the files to use in historical period for
%simple model 
sat_obs1=simple_model_data1880to2015(:,2); %temp observations
co21=simple_model_data1880to2015(:,3); %co2 concentrations
nino1=simple_model_data1880to2015(:,4); %el nino
saod1=simple_model_data1880to2015(:,5); %volcanic eruptions 

sat0=sat_obs1(1); %starting value for observations in hist period
GMST(1)=sat0; %starting temp value for simple model in hist period at the earliest observation
GMSTd(1)=mean(sat_obs1(1:20)); %starting temp for the second temperature (deep ocean temperature)

%running the simple model in the historical period so that I can use the
%ending value to start the predictive period 
for i=2:1:126
    f=co21(i);
    g=nino1(i);
    h=saod1(i);
    GMST(i) = GMST(i-1) - GMST(i-1)/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd(i-1)-GMST(i-1))/tau1; %GMST is surface temperature 
    GMSTd(i) = GMSTd(i-1) + (GMST(i-1)-GMSTd(i-1))/tau2; %this is a two-temperature model, GMSTd represents deep ocean temp
end
GMSTdstart=GMSTd(1,1:126);
obs=sat_obs1; 

%for using the model predictively, el nino and volcanic aerosols must be
%zero, because we don't have values for those going into the future from
%the given pathways
nino1=zeros(700,1);
saod1=zeros(700,1);

%there are four pathways: 2.6, 4.5, 6.0 and 8.5 
%there are two kinds of co2 concentrations: atmospheric (actual
%concentration) and equivalent (taking into account other greenhouse
%gas/aerosol forcings in the form of one co2 concentration) 

%this is atmospheric co2 for the first pathway, using the model
%predictively
time1=RCP3PD_MIDYR_CONC(116:536,1); 
co2_26=RCP3PD_MIDYR_CONC(116:536,4); %taking co2 data from the pathway
GMST_26=zeros(1,126); %preallocating (this is maybe not necessary) 
GMST_26(1,1:126)=obs(1:126,1); %start the first (surface) temperature at the end of the observations
GMSTd_26=GMSTdstart; 
for i=127:1:421 %running the two-temperature model 
    f=co2_26(i);
    g=nino1(i);
    h=saod1(i);
    GMST_26(i) = GMST_26(i-1) - GMST_26(i-1)/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd_26(i-1)-GMST_26(i-1))/tau1;
    GMSTd_26(i) = GMSTd_26(i-1) + (GMST_26(i-1)-GMSTd_26(i-1))/tau2;
end
GMST_26=GMST_26.';

%pathway 6.0, atmospheric co2, same thing as above 
co2_6=RCP6_MIDYR_CONC(116:536,4);
GMST_6=zeros(1,126);
GMST_6(1,1:126)=obs(1:126,1);
GMSTd_6=GMSTdstart;
for i=127:1:421
    f=co2_6(i);
    g=nino1(i);
    h=saod1(i);
    GMST_6(i) = GMST_6(i-1) - GMST_6(i-1)/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd_6(i-1)-GMST_6(i-1))/tau1;
    GMSTd_6(i) = GMSTd_6(i-1) + (GMST_6(i-1)-GMSTd_6(i-1))/tau2;
end
GMST_6=GMST_6.';

%pathway 4.5, atmospheric co2
co2_45=RCP45_MIDYR_CONC(116:536,4);
GMST_45=zeros(1,126);
GMST_45(1,1:126)=obs(1:126,1);
GMSTd_45=GMSTdstart;
for i=127:1:421
    f=co2_45(i);
    g=nino1(i);
    h=saod1(i);
    GMST_45(i) = GMST_45(i-1) - GMST_45(i-1)/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd_45(i-1)-GMST_45(i-1))/tau1;
    GMSTd_45(i) = GMSTd_45(i-1) + (GMST_45(i-1)-GMSTd_45(i-1))/tau2;
end
GMST_45=GMST_45.';

%pathway 8.5, atmospheric co2 
co2_85=RCP85_MIDYR_CONC(116:536,4);
GMST_85=zeros(1,126);
GMST_85(1,1:126)=obs(1:126,1);
GMSTd_85=GMSTdstart;
for i=127:1:421
    f=co2_85(i);
    g=nino1(i);
    h=saod1(i);
    GMST_85(i) = GMST_85(i-1) - GMST_85(i-1)/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd_85(i-1)-GMST_85(i-1))/tau1;
    GMSTd_85(i) = GMSTd_85(i-1) + (GMST_85(i-1)-GMSTd_85(i-1))/tau2;
end
GMST_85=GMST_85.';

%pathway 2.6, equivalent co2
co2eq_26=RCP3PD_MIDYR_CONC(116:536,2);
GMST2_26=zeros(1,126);
GMST2_26(1,1:126)=obs(1:126,1);
GMSTd_eq_26=GMSTdstart;
for i=127:1:421
    f=co2eq_26(i);
    g=nino1(i);
    h=saod1(i);
    GMST2_26(i) = GMST2_26(i-1) - GMST2_26(i-1)/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd_eq_26(i-1)-GMST2_26(i-1))/tau1;
    GMSTd_eq_26(i) = GMSTd_eq_26(i-1) + (GMST2_26(i-1)-GMSTd_eq_26(i-1))/tau2;
end
GMST2_26=GMST2_26.';

%pathway 6.0, equivalent co2
co2eq_6=RCP6_MIDYR_CONC(116:536,2);
GMSTeq_6=zeros(1,126);
GMSTeq_6(1,1:126)=obs(1:126,1);
GMSTd_eq_6=GMSTdstart;
for i=127:1:421
    f=co2eq_6(i);
    g=nino1(i);
    h=saod1(i);
    GMSTeq_6(i) = GMSTeq_6(i-1) - GMSTeq_6(i-1)/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd_eq_6(i-1)-GMSTeq_6(i-1))/tau1;
    GMSTd_eq_6(i) = GMSTd_eq_6(i-1) + (GMSTeq_6(i-1)-GMSTd_eq_6(i-1))/tau2;
end
GMSTeq_6=GMSTeq_6.';

%pathway 4.5, equivalent co2
co2eq_45=RCP45_MIDYR_CONC(116:536,2);
GMSTeq_45=zeros(1,126);
GMSTeq_45(1,1:126)=obs(1:126,1);
GMSTd_eq_45=GMSTdstart;
for i=127:1:421
    f=co2eq_45(i);
    g=nino1(i);
    h=saod1(i);
    GMSTeq_45(i) = GMSTeq_45(i-1) - GMSTeq_45(i-1)/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd_eq_45(i-1)-GMSTeq_45(i-1))/tau1;
    GMSTd_eq_45(i) = GMSTd_eq_45(i-1) + (GMSTeq_45(i-1)-GMSTd_eq_45(i-1))/tau2;
end
GMSTeq_45=GMSTeq_45.';

%pathway 8.5, equivalent co2
co2eq_85=RCP85_MIDYR_CONC(116:536,2);
GMSTeq_85=zeros(1,126);
GMSTeq_85(1,1:126)=obs(1:126,1);
GMSTd_eq_85=GMSTdstart;
for i=127:1:421
    f=co2eq_85(i);
    g=nino1(i);
    h=saod1(i);
    GMSTeq_85(i) = GMSTeq_85(i-1) - GMSTeq_85(i-1)/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd_eq_85(i-1)-GMSTeq_85(i-1))/tau1;
    GMSTd_eq_85(i) = GMSTd_eq_85(i-1) + (GMSTeq_85(i-1)-GMSTd_eq_85(i-1))/tau2;
end
GMSTeq_85=GMSTeq_85.';
time2=1880:1:2015;
time3=2016:1:2100;

%loading data from the files containing multimodel means for all the
%General Circulation Models
%also, adjusting them by adding 2.9 degrees so that they are at the right
%point in the graph 
rcp85=final_rcp85_2(31:451,2)-2.869e+02;
rcp60=final_rcp60_2(31:451,2)-2.869e+02;
rcp26=final_rcp26_2(31:451,2)-2.869e+02;
rcp45=final_rcp45_2(31:451,2)-2.869e+02;

%plotting multimodel means, and simple model with equivalent/atmospheric
%co2, for each pathway 
plot(time1,GMST_26,'b',time1,GMST_6,'m',time1,GMST_45,'g',time1,GMST_85,'r',time1,GMST2_26,'--b',time1,GMSTeq_6,'--m',time1,GMSTeq_45,'--g',time1,GMSTeq_85,'--r',time1,rcp26,'-.b',time1,rcp60,'-.m',time1,rcp45,'-.g',time1,rcp85,'-.r',time2,sat_obs1,'k')

