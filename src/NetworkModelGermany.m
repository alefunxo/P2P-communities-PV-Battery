%% ------------------------------------------------------------------------
% This is the network generator, the inputs are population density, load
% profile for the customers, solar production profiles, and external model
% parameters found in the MainModelGermany file.
%% ------------------------------------------------------------------------


function  [MinCost,CustomersPerFeeder,PeakLoad,CustomersPerTransformer,TransformerType...
    ,LLVMax,solcap,NumberOfTransformers,CapPerCustomer,EnergyPerKM2,Limiter]...
    = NetworkModelGermany(PopDensity, HouseLoadProfile, AptLoadProfile1, SolProfile, Alpha, VoltageLimit)



%%  Set model parameters  %%
% Design voltage limits
Design_voltage = [1.1 0.94];

% Design loop impedance from IEC
Design_Z = 0.51*1000;

% Nominal voltage
Vn = 400;    

% Demographic parameters
PeoplePerHouse = 2;
PeoplePerApt = 2;


% Fuse ratings
AptFuseRating = 10;
HouseFuseRating = 10;

% Transformer parameters
TransformerCap = [1250 1000 800 500 315 200 100 50];
TransformerCost = [195272 159520.98 134751 101565 70501 5350900 3844600 3214000];%134
Transformer_Impedance = [6.5 8 10 13 20 32 65 130]/1000;%10
Z_TransformerR_list = [0.000848114120254821 0.0010420326194848022 0.00148841681507050 0.00251028652976876 0.00492770793724613 0.00776114000116266 0.0202385770250776 0.0404771540501553];
Z_TransformerX_list = [0.00763302708229339 0.008027342124469827 0.0119073345205640 0.0125514326488438 0.0197108317489845 0.0310445600046506 0.0607157310752329 0.121431462150466]; % http://www.schneider-electric.com/resources/sites/SCHNEIDER_ELECTRIC/content/live/FAQS/225000/FA225849/en_US/Calculation%20exaple%20for%20X%20to%20R%20ratio.docx


% Cable parameters
CableCapacity = [52 67 80 94 138 178 230 356 460];
fuses_tripping_size = [6 10 16 20 25 32 40 50 63 80 100 125 160 200 250 315;...
    18 32 65 85 110 150 190 250 320 425 580 715 950 1250 1650 2200];
targetFuseRatings = [6 10 16 20 25 32 40 50 63 80 100 125 160 200 250 315];
Z_lineR_list = [1.83 1.15 0.76 0.641 0.32 0.206 0.125 0.103 0.0625];
Z_lineX_list = [0.0861 0.0817 0.0783 0.0779 0.0762 0.0745 0.0752 0.0752 0.0752];
Z_Line = sqrt(Z_lineR_list.^2+Z_lineX_list.^2);
targetLineImpedance = [4.18 2.63 1.91 1.47 0.746 0.495 0.324 0.324 0.324]; % Last two values are assumed

% Tripping criteria parameters
c = 1; 
Ufn = 230;

% Set feeder length multiplier (gamma)
if PopDensity<=200% rural
    LLAF_LV = 1;                                                            
elseif PopDensity>200 && PopDensity<=1000% semiurban
    LLAF_LV = 1.1;
else% urban
    LLAF_LV = 1.2;
end


% Demand parameters
PowerFactor = 0.9;
PowerFactor = sind(acosd(PowerFactor));
inf_coincidence = 0.3;
margin = Alpha;     
AptBuilding_share = 0;%1.25;     % https://www.sciencedirect.com/science/article/pii/S0378778811005019#fig0010


% Cable costs per km (SEK)
if PopDensity>1000
    CostPerKmLineLV = 827000;
    CostPerKmLineMV = 1003802;
elseif PopDensity>200 
    CostPerKmLineLV = 540000;
    CostPerKmLineMV = 690984;
else   
    CostPerKmLineLV = 177000; 
    CostPerKmLineMV = 358154;
end


% Calculate the share of single family and multifamily households.

frac_AP = 0;
frac_HH = 1;


% Calcualte average fuse rating
fuse = AptFuseRating*frac_AP+HouseFuseRating*frac_HH;


% Calculate the average number of people per household
PeoplePerHousehold = PeoplePerHouse*frac_HH + PeoplePerApt*frac_AP;


% Special cases of low population density
if PopDensity<4
    NumberOfCustomers = PopDensity;
    frac_AP = 0;
    frac_HH = 1;
else
    NumberOfCustomers = PopDensity/PeoplePerHousehold;
end


% Create average load profile
AVG_LoadProfile = (frac_HH.*HouseLoadProfile+frac_AP.*AptBuilding_share*AptLoadProfile1);


% Set initial values for cost-minimization 
maxP = fuse*Vn*sqrt(3)/1000
CustomersPerTransformer = NumberOfCustomers;
coincidenceTR = inf_coincidence + (1-inf_coincidence)/(CustomersPerTransformer^0.75);
PeakLoadTR = margin*CustomersPerTransformer*coincidenceTR*maxP;
n0 = ceil(PeakLoadTR/TransformerCap(1));%min # of trafos?
ToTCost = zeros((200),length(TransformerCap));
PeakLoad = PeakLoadTR/margin;
%%
% Finding lest cost option of transformer capacity and number of
% transformers to supply the load.
for gg = n0:100%Number of trafos
    
    CustomersPerTransformer = round(NumberOfCustomers/gg);
    coincidenceTR = inf_coincidence + (1-inf_coincidence)/(CustomersPerTransformer^0.75);
    PeakLoadTR = margin*CustomersPerTransformer*coincidenceTR*maxP;
    indexTR = find((PeakLoadTR./(TransformerCap)<1)~=0,1, 'last');
    TransformerCostArea = TransformerCost(indexTR)*gg;

    A_TS = 0.01/gg; % in km^2
    NubmerOfConnectionsTransformer = NumberOfCustomers/gg;
    d = sqrt(A_TS)/(sqrt(NubmerOfConnectionsTransformer)+1)+0.02;
    dMV = 1/(sqrt(gg)+1);
    
    if mod(round(sqrt(NubmerOfConnectionsTransformer)),2)
        n = NubmerOfConnectionsTransformer-1;
    else
        n = NubmerOfConnectionsTransformer + sqrt(NubmerOfConnectionsTransformer) -2;
    end
    
    if round(sqrt(gg)) == 1
        nMV = 0.5;
    elseif mod(round(sqrt(gg)),2)
        nMV = gg-1;
    else
        nMV = gg + sqrt(gg) - 2;
    end

    LengthLVPerTransformer = n*d;
    LengthMV = nMV*dMV;

    MV_cost = LengthMV*CostPerKmLineMV;
    LV_cost = gg*LengthLVPerTransformer*CostPerKmLineLV;
    LineCost = LV_cost + MV_cost;
    
    ToTCost((gg-n0+1),indexTR) = TransformerCostArea + LineCost;
end

% Extract lowest cost option and transformer capcaity
ToTCost(ToTCost==0)=NaN;
MinCost = min(min(ToTCost));
size(ToTCost);
[xt,TrSize]=find(ToTCost==MinCost);
xt=xt(1);
TrSize=TrSize(1);
NumberOfTransformers = n0+xt-1;

% Update transformer and area parameters
TransformerType = TransformerCap(TrSize);
A_TS = 0.01/NumberOfTransformers; % in km^2
CustomersPerTransformer = round(NumberOfCustomers/NumberOfTransformers);
coincidenceTR = inf_coincidence + (1-inf_coincidence)/(CustomersPerTransformer^0.75);


% Calcualte transformer resistance and reactance
Transformer_R = Z_TransformerR_list(TrSize);
Transformer_X = Z_TransformerX_list(TrSize);
Z_transformer = Transformer_Impedance(TrSize);


% Handle special cases of the uniform distribution of customers
if CustomersPerTransformer == 2
    LLVMax = 0.1;
    d = LLVMax;
    n = 2;
    nc = 2;
elseif CustomersPerTransformer == 1
    LLVMax =  0.1;
    d = LLVMax;
    n = 1;
    nc = 1;
else
    n = round(sqrt(CustomersPerTransformer));
    nc = round(CustomersPerTransformer/4);
    if n == 2
        d = sqrt(A_TS)/(4)+0.02;
        LLVMax = d;
        nc = 2;
    else
        LLVMax = LLAF_LV*sqrt(A_TS)*(n-1)/(n)+0.02;
        d = LLVMax/n;

    end
end


% Set branches of feeders based on number of supplied customers (n^2)
if n > 10
    m = zeros(1,5);
    mc = zeros(1,5);
    m(1)= round((n-1)/5);
    m(2) = m(1);
    m(3) = m(1);
    m(4) = m(1);
    m(5) = n-m(1)-m(2)-m(3)-m(4);
    mc(1)= round((nc-1)/5);
    mc(2) = mc(1);
    mc(3) = mc(1);
    mc(4) = mc(1);
    mc(5) = nc-mc(1)-mc(2)-mc(3)-mc(4);
    p = 5;
elseif n > 8
    m = zeros(1,4);
    mc = zeros(1,4);
    m(1)= round((n-1)/4);
    m(2) = m(1);
    m(3) = m(1);
    m(4) = n-m(1)-m(2)-m(3);
    mc(1)= round((nc-1)/4);
    mc(2) = mc(1);
    mc(3) = mc(1);
    mc(4) = nc-mc(1)-mc(2)-mc(3);
    p = 4;
elseif n == 1
    m = zeros(1,1);
    mc = zeros(1,1);
    m(1) = n;
    mc(1) = nc;
    p = 1;
elseif n>6
    m = zeros(1,3);
    mc = zeros(1,3);
    m(1)= round((n-1)/3);
    m(2) = m(1);
    m(3) = n-m(1)-m(2);
    mc(1)= round((nc-1)/3);
    mc(2) = mc(1);
    mc(3) = nc-mc(1)-mc(2);
    p = 3;
else
    m = zeros(1,2);
    mc = zeros(1,2);
    m(1)= round((n-1)/2);
    m(2) = n-m(1);
    mc(1)= round((nc-1)/2);
    mc(2) = nc-mc(1);
    p = 2;
end


% Calculate length of each feeder section, and aggregate for a total feeder
% length.
mc = flip(mc);
d_long = zeros(1,p);
for uu = 1 :p
    if n == 1
        d_long(uu) = (m(uu))*d*1000;%in km?
    else
        d_long(uu) = (m(uu)-1/p)*d*1000;%in km?
    end
end
d_long = flip(d_long);
CustomersPerFeeder = sum(mc);


% Set variables used in cable sizing
Cable = zeros(1,p);
R = zeros(1,p);
X = zeros(1,p);
ZmaxThick = zeros(9,p);
coincidenceLine = zeros(1,p);
P_demand = zeros(1,p);
mp = zeros(1,p);
L_max = zeros(9,p);
I_line_fuse = zeros(1,p);
RX_multiplier = ones(1,p);
z_supply = zeros(1,p+1);
Z_loop = [Z_transformer*1000];


% Size feeders based on fuse ratings and tripping criteria
for rt=1:p
    
    rr = sum(mc(rt:end));
    mp(rt) = rr;
    coincidenceLine(rt) = inf_coincidence + (1-inf_coincidence)/(rr^0.75);
    P_demand(rt) =  sqrt(3)*fuse*400*rr*coincidenceLine(rt);
    I_line = P_demand(rt)./(sqrt(3)*400);
    
    % Check fuse ratings based om demand and update coincidence along
    % feeder
    while I_line>max(targetFuseRatings)
        rr = round(rr/2);
        RX_multiplier(rt) =  RX_multiplier(rt)*0.5;
        coincidenceLine(rt) = inf_coincidence + (1-inf_coincidence)/(rr^0.75);
        P_demand(rt) =  sqrt(3)*fuse*400*rr*coincidenceLine(rt);
        I_line = P_demand(rt)./(sqrt(3)*400);
    end

    I_line_fuse(rt) = targetFuseRatings(find((ceil(I_line)./targetFuseRatings<=1)~=0,1, 'first'));
    index = find((I_line_fuse(rt)./targetFuseRatings<=1)~=0,1, 'first') ;
    Iu = fuses_tripping_size(2,(index));
    ZmaxThick(rt) = 1000*c*Ufn/Iu;
    
    % Set maximum length of feeders based on available capacity and fuse
    % ratings
    L_max(:,rt) = (ZmaxThick(rt)-sum(Z_loop))./targetLineImpedance';
    while sum(L_max(:,rt)>d_long(rt)) == 0
        rr = round(rr/2);
        RX_multiplier(rt) =  RX_multiplier(rt)*0.5;
        coincidenceLine(rt) = inf_coincidence + (1-inf_coincidence)/(rr^0.75);
        P_demand(rt) =  sqrt(3)*fuse*400*rr*coincidenceLine(rt);
        I_line = P_demand(rt)./(sqrt(3)*400);
        I_line_fuse(rt) = targetFuseRatings(find((ceil(I_line)./targetFuseRatings<=1)~=0,1, 'first'));
        index = find((I_line_fuse(rt)./targetFuseRatings<=1)~=0,1, 'first') ;
        Iu = fuses_tripping_size(2,(index));
        ZmaxThick(rt) = 1000*c*Ufn/Iu;
        L_max(:,rt) = (ZmaxThick(rt)-sum(Z_loop))./targetLineImpedance';
        
    end
     
    L_max(L_max<d_long(rt))=NaN;
    L_max(CableCapacity<I_line_fuse(rt),rt) = NaN;
    [void ixZloop] = min(L_max(:,rt));
    Z_loopNew = targetLineImpedance(ixZloop)*d_long(rt);
    Z_loop = [Z_loop Z_loopNew];
end


% Check cable thermal capacity is within limits
L_max(isnan(L_max))=0;
z_supply(1) = Z_transformer*1000;
for gg=1:p
    ixCable(gg) = find(L_max(:,gg),1);
    Cable(gg) = CableCapacity(ixCable(gg));
    if CableCapacity(ixCable(gg))<max(I_line_fuse)
        ixCable(gg) = find((I_line_fuse(gg)./CableCapacity<=1)~=0,1, 'first');
        Cable(gg) = CableCapacity(ixCable(gg));
    end
    R(gg) = Z_lineR_list(ixCable(gg))*d_long(gg);
    X(gg) = Z_lineX_list(ixCable(gg))*d_long(gg);
    z_supply(gg+1) = Z_Line(ixCable(gg))*d_long(gg)*RX_multiplier(gg);
end


id = find(ixCable==max(ixCable),1);

for hh=1:id
    Cable(hh) = CableCapacity(ixCable(id));
    R(hh) = Z_lineR_list(ixCable(id))*d_long(id);
    X(hh) = Z_lineX_list(ixCable(id))*d_long(id);
    z_supply(hh+1) = Z_Line(ixCable(hh))*d_long(hh)*RX_multiplier(hh);
end

% Make sure updated resistance and reactance have correct quantity
R = R./1000;
X = X./1000;


% Check voltage drop along feeder is within regulations and update if
% voltage is outside limits
V = zeros(1,p+1);
V(1) = 400 -coincidenceTR*CustomersPerTransformer*(Transformer_R.*(fuse*230*3)+Transformer_X.*(fuse*230*3)*PowerFactor)./(Vn);

for kk=1:p
    V(kk+1) = V(kk)-(R(kk).*(P_demand(kk))+(X(kk)).*P_demand(kk)*PowerFactor)./(Vn);
end


% Checking that voltage drop is within limits, otherwise increse cable
% capacity
while  min(V./Vn)<Design_voltage(2)
    [val pos] = min(ixCable);
    if min(ixCable) == 8
        [val2 pos] = max(RX_multiplier);
        RX_multiplier(pos) = RX_multiplier(pos)*0.5;
        R(pos) = Z_lineR_list(val)*d_long(pos)*RX_multiplier(pos)/1000;
        X(pos) = Z_lineX_list(val)*d_long(pos)*RX_multiplier(pos)/1000;
        ixCable(pos) = val;
        Cable(pos) = CableCapacity(val);
        z_supply(pos+1) = z_supply(pos) + targetLineImpedance((val))*d_long(pos)*RX_multiplier(pos);
    else
        R(pos) = Z_lineR_list(val+1)*d_long(pos)*RX_multiplier(pos)/1000;
        X(pos) = Z_lineX_list(val+1)*d_long(pos)*RX_multiplier(pos)/1000;
        ixCable(pos) = val +1;
        Cable(pos) = CableCapacity(val+1);
        z_supply(pos+1) = z_supply(pos) + targetLineImpedance((val+1))*d_long(pos)*RX_multiplier(pos);
    end
    V = zeros(1,p+1);
    V(1) = 400 -min((coincidenceTR*CustomersPerTransformer*(Transformer_R.*(fuse*230*3)+Transformer_X.*(fuse*230*3).*PowerFactor)./Vn));
    for kk=1:p
        V(kk+1) = V(kk)-(R(kk).*(P_demand(kk))+(X(kk)).*P_demand(kk)*PowerFactor)./(Vn);
    end
 
end


% Checking that supply impedance is below IEC values
while  sum(z_supply)>Design_Z
    
    [val pos] = min(ixCable);
    if min(ixCable) == 8
        [val2 pos] = max(RX_multiplier);
        RX_multiplier(pos) = RX_multiplier(pos)*0.5;
        z_supply(pos+1) =  Z_Line(val)*d_long(pos)*RX_multiplier(pos);
        ixCable(pos) = val;
        Cable(pos) = CableCapacity(val)*(1/RX_multiplier(pos));
    else
        z_supply(pos+1) =  Z_Line(val+1)*d_long(pos)*RX_multiplier(pos);
        ixCable(pos) = val+1;
        Cable(pos) = CableCapacity(val+1);
    end
    
end


% Add solar PV systems to each connection point on the feeder in increments
% of 0.5 kW
for ll=0.5:0.5:50
    % Calcualte maximum voltage along feeder
    V0 = 400 - min((CustomersPerTransformer*(Transformer_R.*(coincidenceTR*fuse*230*3-(ll).*SolProfile.*1000)+Transformer_X.*(coincidenceTR*fuse*230*3*PowerFactor))./Vn)) ;
    if length(mp) == 1
        voltage = V0- min((mp.*(R.*(coincidenceLine.*AVG_LoadProfile-(ll).*SolProfile.*1000)+X.*(coincidenceLine.*AVG_LoadProfile*PowerFactor))./Vn)');
    else
        voltage = V0- min(sum((mp.*(R.*(coincidenceLine.*AVG_LoadProfile-(ll).*SolProfile.*1000)+X.*(coincidenceLine.*AVG_LoadProfile*PowerFactor))./Vn)'));
    end
    
    
    % Calculate maximum demand along feeder
    deltaPowerPerCustomerDouble = mc.*max(abs(coincidenceLine(1).*AVG_LoadProfile-ll.*SolProfile.*1000))/400/sqrt(3);
    deltaPowerPerCustomer = max(abs(coincidenceTR*AVG_LoadProfile-ll.*SolProfile.*1000));
    deltaPower = deltaPowerPerCustomer.*CustomersPerTransformer;
    
    if (max(max(voltage))/400)>VoltageLimit(1) 
        solcap = (ll-0.5)*NumberOfCustomers;
        Limiter = 10;
        break
       
    elseif deltaPowerPerCustomerDouble>Cable 
        solcap = (ll-0.5)*NumberOfCustomers;
        Limiter = 25;
        break
      
    elseif deltaPower>(TransformerType*1000)
        solcap = (ll-0.5)*NumberOfCustomers;
        Limiter = 20;
        break
    end
end
deltaPower>(TransformerType*1000)

EnergyPerKM2 = (NumberOfCustomers)*(ll-0.5)*sum(SolProfile)/4;       % i kW
CapPerCustomer = (ll-0.5);

end










