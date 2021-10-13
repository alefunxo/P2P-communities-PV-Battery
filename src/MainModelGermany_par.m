%% ----------------------------------------------------------------------
% This is the main model file for the German version of the network model.
% It contains data loading, processing and saving. None of the data files
% are included in the model package but can be found either on Eurostat,
% the associated publications or purchased from VDE.
% A description of the model can be found in E. Hartvigsson, et al., "Generating low-voltage grid 
% proxies in order to estimate grid capacity for residential end-use 
% technologies: The case of reisdential solar PV", MethodsX.
% Data can be found in E. Hartvigsson, et al., "Dataset for generating synthetic residential low-voltage
% grids in Sweden, Germany and the UK", Data in Brief
% The model represents parameters and regulations as of 2020.
%% ----------------------------------------------------------------------

clear all
clc
close all

%No need for Pena-Bello
% GEOSTAT shapefile can be downloaded from Eurostat.
%filename = 'GEOSTAT_grid_POP_1K_2011_V2_0_1_CNTR_CODE_DE.shp';
%GISData = shaperead(filename,'Selector',{@(v1) (v1<55000) && (v1>1), 'GEOSTAT_gr'});
%%
g=1;
% DE load profiles, can be purchased from VDE.
%Charge load profiles DE in 15 min res
LoadHouse=load('LoadHouse.mat');
LoadHouse=LoadHouse.MatlabD;

% Load solar production dataset, from Z. Norwood, E. Nyholm, T. Otanicar, 
% and F. Johnsson, â€œA Geospatial Comparison of Distributed Solar Heat and 
% Power in Europe and the US,â€? PLoS One,  NOT INCLUDED
%kk=load('PV_factor_XYcoordinatesDE.mat');
%PV_factor = kk.PV_factor;
%PV_factor = PV_factor(:,:,1:8760);
%PV_X = kk.X;
%PV_Y = kk.Y;
%Load PV generation
PV=load('PV.mat');
PV=PV.PV;
pp = 2;     % Voltage, refers to base case
mm = 3;     % Transformer margin, refers to base case
nn = 2;     % Load, refers to base case

%%%%% Control parameters
voltageLimitMatrix = [1.1 0.94; 1.05 0.94; 1.03 0.94];
alphavec = [1 1.2 1.5 1.8 2.1 2.4];
LoadProfileHH = mean(LoadHouse,2)*4;
LoadProfileAP = LoadHouse(:,nn).*0;
voltageLimit = voltageLimitMatrix(pp,:);

Sol_factor=PV*4;

%CapacityPerCell = zeros(1,g);
MaxFeeder = zeros(1,g);
Transformers = zeros(1,g);
CapacityPerCustomer = zeros(1,g);
TransformersCap = zeros(1,g);
CustomersPerFeeder = zeros(1,g);
CustomersPerTr = zeros(1,g);
ShareAPT = zeros(1,g);
Limit = zeros(1,g);
PeakLoad = zeros(1,g);
Energy = zeros(1,g);
PopDensity=200;



parfor k=1:6       % different margins
    alpha = alphavec(k);
[MinCost,customersperfeeder,MaxLoad,CustomersPerTransformer,...
    TrCap,LV,ll,Trans,CapPerCust,EnergyPerKM2,Limiter]...
    = NetworkModelGermany(PopDensity,LoadProfileHH,LoadProfileAP, Sol_factor, ...
        alpha, voltageLimit)
    % Save data from each alpha
    CapacityPerCell(k) = ll; 
    MaxFeeder(k) = LV;
    Transformers(k) = Trans;
    CapacityPerCustomer(k) = CapPerCust;
    TransformersCap(k) = TrCap;
    CustomersPerFeeder(k) = customersperfeeder;
    CustomersPerTr(k) = CustomersPerTransformer;
    MinCosts(k) = MinCost;
    Limit(k) = Limiter;
    PeakLoad(k) = MaxLoad;
    Energy(k) = EnergyPerKM2;

end

%LV is max Feeder

for h = 1:length(CapacityPerCell)
    ModelOutput(h).Cap = CapacityPerCell(h);
    ModelOutput(h).CapPerCust = CapacityPerCustomer(h);
    ModelOutput(h).Energy = Energy(h);
    ModelOutput(h).Feeder = MaxFeeder(h);
    ModelOutput(h).TrCap = TransformersCap(h);
    ModelOutput(h).TrNumbber = Transformers(h);
    ModelOutput(h).CustPerFeeder = CustomersPerFeeder(h);
    ModelOutput(h).CustPerTr = CustomersPerTr(h);
    ModelOutput(h).Demand = PeakLoad(h);
    ModelOutput(h).MinCosts = MinCosts(h);
    ModelOutput(h).Limiter = Limit(h);

end

filename = sprintf('DE_DataInBrief_fuse_10');
writetable(table(ModelOutput),filename);


