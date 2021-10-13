'''
File modified to use Production to Consumption ratio.

It is working, but need to be improved


'''

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sys  
import os
from itertools import cycle,product
import datetime as dt
import warnings
sys.path.insert(0, '../../Demand')
sys.path.insert(0, '../../PV_model/src')
from get_demand import get_demand as dem
import get_pv
import P2P_functions as p2p
import copy
from pandas.plotting import register_matplotlib_converters
register_matplotlib_converters()

def Gini_rsv(y,w=None):
    '''takes a np array and the weights if it is the case and return the GINI index for negative income
    Based on https://rdrr.io/cran/GiniWegNeg/src/R/Gini_RSV.R
    '''    
    arg_sorted=np.argsort(y)
    y=y[arg_sorted]
    if w is None:
        w=np.ones(len(y))
    else:
        w=w[arg_sorted]
        
    N=sum(w)
    yw=y*w
    C_i=np.cumsum(w)
    num_1=np.sum(yw*C_i)
    num_2=np.sum(yw)
    num_3=np.sum(yw*w)
    G_num=(2/N**2)*num_1-(1/N)*num_2-(1/N**2)*num_3
    
    T_neg=np.sum(yw[yw<0])
    T_pos=np.sum(yw)+np.abs(T_neg)
    n_RSV=(2*(T_pos+(np.abs(T_neg)))/N)
    mean_RSV=(n_RSV/2)
    G_RSV=(1/mean_RSV)*G_num
    return G_RSV.round(2)
    

class Community():
    """
    Main Community class
    Contains common methods and functions such as the Data initialization, plots and 
    ----
    Outputs:
     Input data
     param_tech
     param_tech_no_batt
     param_tech_comm
     meta
     pv_dist
     selection     
     
     retail_price_sc
     export_price_sc
     
     nested_out
     sum_bill
     PV_size_comm
     result_out
     out_comm_final
     out
     
    """
    
    
    def __init__(self, Data = None):
        """
        Community initialization using Data (Dict)
        -----
        Data: Dict with community_size,timestep,seed,country_code,pv_penetration,
        pv_penetration,batt_penetration,retail_price,export_price,predet_bhv,ILR,
        BatteryCapacity,BatteryEfficiency,InverterEfficiency,MaxPower
        
        ----
        TODO
        Assign battery size per household
        """
        print('######################')
        print('Getting demand and PV profiles')
        if(Data is None):
            self.community_size=100
            self.timestep=0.25 # in hours 0.25 is 15 minute
            self.seed=1
            self.country_code='DE'
            self.pv_penetration=50
            self.batt_penetration=50
            self.retail_price=0.28
            self.export_price=0.04
            self.predet_bhv=False#'high','low',False
            self.ILR=1
            self.test_sc=False #generate P2P community prices without exchanges
            self.cut='q0' # get the deciles of the trading groups in ['q0', 'q10', 'q20', 'q30', 'q40', 'q50', 'q60', 'q70', 'q80', 'q90']
            self.PtC=False
            self.Comm='SC'#default
            self.parallel=True #default if parallel delete the non relevant results
            self.PWI='None'
        else:
            #If a dataframe has been provided, proceed to set up the optimisation problem:
            print('inside')
            self.community_size=Data['community_size']
            self.timestep=Data['timestep'] # in hours 0.25 is 15 minute
            self.seed=Data['seed']
            self.country_code=Data['country_code']
            self.pv_penetration=Data['pv_penetration']
            self.batt_penetration=Data['batt_penetration']
            self.retail_price=Data['retail_price']
            self.export_price=Data['export_price']
            self.predet_bhv=Data['predet_bhv']
            self.ILR=Data['ILR']
            self.test_sc=Data['test_sc'] #generate P2P community prices without exchanges
            self.cut=Data['cut']
            self.PtC=Data['PtC']# Production to consumption ratio
            self.Comm='SC'#Default
            self.parallel=Data['parallel'] #default
            self.PWI='None'
    def createParams(self):
        self.param_tech={'BatteryCapacity': 10,
                          'BatteryEfficiency': .91,
                          'InverterEfficiency': .94,
                          'MaxPower': 4,
                          'timestep': self.timestep,                         
                          'pv_penetration':self.pv_penetration,
                          'batt_penetration':self.batt_penetration,
                          'seed':self.seed,
                          'community_size':self.community_size}
        self.param_tech_no_batt = {'BatteryCapacity': 0,
                          'BatteryEfficiency': .91,
                          'InverterEfficiency': .94,
                          'MaxPower': 0,
                          'timestep': self.timestep,                         
                          'pv_penetration':self.pv_penetration,
                          'batt_penetration':self.batt_penetration,
                          'seed':self.seed,
                          'community_size':self.community_size,
                          'test_sc': self.test_sc} #true if SC False if P2P}
        self.param_tech_comm = {'BatteryCapacity': 0,
                          'BatteryEfficiency': 1,
                          'InverterEfficiency': 1,
                          'MaxPower': 0,
                          'timestep': self.timestep,                         
                          'pv_penetration':self.pv_penetration,
                          'batt_penetration':self.batt_penetration,
                          'seed':self.seed,
                          'community_size':self.community_size}
        
        
    def selectData(self):
        """
        Community initialization using Data (Dict)
        -----
        Data: Dict
        """
        df,self.meta=dem(self.country_code)
        pv=get_pv.get_pv(self.country_code,self.timestep)
        if (self.timestep>0.25) & (self.country_code in ['DE','CH','US','UK']):
            if self.timestep==0.5:
                df=df.resample('30T').sum()*2 # from kWh to kW
                pv=pv*2
            elif self.timestep==1:
                df=df.resample('1H').sum() # from kWh to kW

        elif (self.timestep>0.5) & (self.country_code in ['IE']):
            if self.timestep==1:
                df=df.resample('1H').sum() # from kWh to kW
        elif self.timestep==0.25:
            df=df*4 # from kWh to kW
            pv=pv*4
        else:
            print('country or timestep not supported')
        print('######################')
        print('Getting PV distribution')
        #We use PV distribution with an average of 4 kW for DE
        self.pv_dist=get_pv.get_pv_ditribution(self.country_code,mean=4,seed=self.seed)
        
        self.df_out=pd.concat([df,pv],axis=1)# demand and PV profiles
        self.df_com=pd.DataFrame(self.df_out.loc[:,(self.df_out.sum()<(10000/
                   self.timestep))].iloc[:,:-1].sample(n=self.community_size,
                   replace=True,random_state=self.seed,axis=1).columns)# names of the hh selected
        if self.PtC==False:
            self.selection=pd.concat([self.pv_dist.round(1).sample(n=self.community_size,
                                      random_state=self.seed).reset_index(drop=True),
                                      self.df_com],axis=1) # select sizes of PV
            self.selection.columns=['PV_size','name']
            list_pv_penetration=[100,75,50,25]# %
            list_batt_penetration=[100,75,50,25,0]# %
            list_product=list(product(list_pv_penetration,list_batt_penetration))
            # Here I match hh profile with different PV sizes and the selection of hh with PV
            # and PV and battery inside the community depending on PV and battery penetration
            # Seed makes the simulations reproducible
            self.selection=p2p.flag_selection(self.selection,list_product,
                                              list_pv_penetration,list_batt_penetration,
                                              self.community_size,self.seed) 
                #self.selection.to_csv('/data/home/alejandropena/Psychology/Input/selection_'+country_code+'.csv',index=False)
        else:
            PV_total=self.df_out.loc[:,(self.df_com.loc[:,0])].sum().sum()*self.PtC/pv.sum()
            sel_pv_size=self.pv_dist.round(1).sample(n=self.community_size,
                                  random_state=self.seed)
            new_sel=sel_pv_size[sel_pv_size.cumsum()<=PV_total].fillna(0)
            sum_pv=new_sel.sum()
            new_sel.loc[new_sel[new_sel.PV==0].sample(n=1,random_state=self.seed).index]=(PV_total-sum_pv).values[0]
            self.selection=pd.concat([new_sel.reset_index(drop=True),self.df_com],axis=1)
            self.selection.columns=['PV_size','name']
            self.selection['battery']=False
            aux_batt=min(len(self.selection[self.selection.PV_size>0]),int(self.batt_penetration/100*self.community_size/2))
            aux=self.selection[self.selection.PV_size>0].sample(random_state=self.seed,
                                                                n= int(aux_batt)).index
            self.selection.loc[aux,'battery']=True
        self.retail_price_sc=np.ones(self.df_out.shape[0])*self.retail_price # could be an input from the user #TODO
        self.export_price_sc=np.ones(self.df_out.shape[0])*self.export_price # could be an input from the user #TODO
        
    def getDataSingleDict(self):
        #get all the data in a single dict
        pv2inv=pd.DataFrame()
        res_pv=pd.DataFrame()
        pv2store=pd.DataFrame()
        inv2load=pd.DataFrame()
        grid2load=pd.DataFrame()
        store2inv=pd.DataFrame()
        inv2curt=pd.DataFrame()
        LevelOfCharge=pd.DataFrame()
        inv2grid=pd.DataFrame()
        batt_losses=pd.DataFrame()
        inv_losses=pd.DataFrame()
        store2load=pd.DataFrame()
        store2grid=pd.DataFrame()
        flagsell=pd.DataFrame()
        for i in self.nested_out.keys():
            pv2inv['pv2inv'+str(i)]=self.nested_out[i]['pv2inv']
            inv2curt['inv2curt'+str(i)]=self.nested_out[i]['inv2curt']
            pv2store['pv2store'+str(i)]=self.nested_out[i]['pv2store']
            inv2load['inv2load'+str(i)]=self.nested_out[i]['inv2load']
            batt_losses['batt_losses'+str(i)]=self.nested_out[i]['batt_losses']
            inv_losses['inv_losses'+str(i)]=self.nested_out[i]['inv_losses']
            store2inv['store2inv'+str(i)]=self.nested_out[i]['store2inv']
            LevelOfCharge['LevelOfCharge'+str(i)]=self.nested_out[i]['LevelOfCharge']
            res_pv['res_pv'+str(i)]=self.nested_out[i]['res_pv']#not important, recalculated later
            inv2grid['inv2grid'+str(i)]=self.nested_out[i]['inv2grid']#not important, recalculated later
            grid2load['grid2load'+str(i)]=self.nested_out[i]['grid2load']#not important, recalculated later
            store2load['store2load'+str(i)]=self.nested_out[i]['store2load']
            store2grid['store2grid'+str(i)]=self.nested_out[i]['store2grid']
            flagsell['flag_sell'+str(i)]=self.nested_out[i]['flag_sell']
        nested_dict={'pv2inv':pv2inv.sum(axis=1),'pv2store':pv2store.sum(axis=1),'inv2load':inv2load.sum(axis=1),
                     'grid2load':grid2load.sum(axis=1),'store2inv':store2inv.sum(axis=1),'LevelOfCharge':LevelOfCharge.sum(axis=1),
                     'inv2grid':inv2grid.sum(axis=1),'res_pv':res_pv.sum(axis=1),'store2grid':store2grid.sum(axis=1),
                     'store2load':store2load.sum(axis=1),'inv_losses':inv_losses.sum(axis=1),
                     'batt_losses':batt_losses.sum(axis=1),'inv2curt':inv2curt.sum(axis=1),
                     'flagsell':flagsell.sum(axis=1)}
        out_comm_res=p2p.dispatch_max_sc(nested_dict['inv2grid'],nested_dict['grid2load'],1e10,self.param_tech_comm)
        self.out_comm_final={}
        self.out_comm_final['pv2inv']=nested_dict['pv2inv'] # DC from hh with PV
        self.out_comm_final['pv2store']=nested_dict['pv2store'] # DC PV 2 batt from hh with PV
        self.out_comm_final['inv2load']=(nested_dict['inv2load']+out_comm_res['inv2load']) # AC from all hh
        self.out_comm_final['store2inv']=nested_dict['store2inv'] # DC  from hh with PV and batt
        self.out_comm_final['LevelOfCharge']=nested_dict['LevelOfCharge'] # kWh  from hh with PV and batt
        self.out_comm_final['res_pv']=out_comm_res['res_pv'] # DC  from hh with PV
        self.out_comm_final['inv2grid']=out_comm_res['inv2grid'] # AC from hh with PV
        self.out_comm_final['grid2load']=out_comm_res['grid2load'] # AC  from all hh
        self.out_comm_final['inv2curt']=nested_dict['inv2curt'] # DC  from hh with PV
        self.out_comm_final['batt_losses']=nested_dict['batt_losses']
        self.out_comm_final['inv_losses']=nested_dict['inv_losses']
        self.out_comm_final['store2load']=nested_dict['store2load']
        self.out_comm_final['store2grid']=nested_dict['store2grid']
        self.out_comm_final['LevelOfCharge']=nested_dict['LevelOfCharge']
        self.out_comm_final['PV_size_comm']=self.PV_size_comm
        self.out_comm_final['df']=self.df_out
        self.out_comm_final['param_tech']=self.param_tech
        self.out_comm_final['nested_dict']=nested_dict
        self.out_comm_final['selection']=self.selection
        self.out_comm_final['flagsell']=nested_dict['flagsell']
        
    def updateOut(self):
        self.out['bill']=self.sum_bill
        self.out['cut']=self.cut
        self.out['Comm']=self.Comm
        if self.test_sc:
            self.out['Comm']=self.out['Comm']+'_P2P'
        
        if (self.parallel) & (self.Comm=='P2P'): #only relevant output
            del(self.df_out)
            del(self.df_com)
            del(self.sum_bill)
            del(self.result_out)
            del(self.prices_binned)
            del(self.df_bhv_paired)
            del(self.df_prices)
            del(self.out_comm_final)
            del(self.param_tech)
            del(self.param_tech_no_batt)
            del(self.param_tech_comm)
            del(self.meta)
            del(self.pv_dist)
            del(self.selection)
            del(self.retail_price_sc)
            del(self.export_price_sc)
            del(self.PV_size_comm)
            del(self.community_size)
            del(self.timestep)
            del(self.seed)
            del(self.country_code)
            del(self.pv_penetration)
            del(self.batt_penetration)
            del(self.retail_price)
            del(self.export_price)
            del(self.predet_bhv)
            del(self.ILR)
            del(self.test_sc)
            del(self.cut)
            del(self.PtC)
            del(self.Comm)
            del(self.parallel)
            del(self.nested_out)
     
    def returnHousehold(self):

        self.out_hh=pd.DataFrame()
        self.out_hh=self.out_hh.append([pd.DataFrame(self.result_out[i],
                           index=[i]) for i in range(len(self.result_out))])
        self.out_hh["SCR_comm"]=self.out["SCR"]
        self.out_hh["SSR_comm"]=self.out["SSR"]
        self.out_hh["Bill_comm"]=self.sum_bill
        self.out_hh['cut']=self.cut
        self.out_hh['Comm']=self.Comm
        self.out_hh['predet_bhv']=self.predet_bhv
        self.out_hh['Benefit_index']=self.PWI
        
        
    
    def plotCommunity(self,week=20,flag=0,prices=None,save=True):
        if prices is None:
            p2p.plot_SCCommunitydispatch_comm(self.df_out.PV*self.PV_size_comm, 
                           self.df_out.loc[:,self.df_com.iloc[:,0]].sum(axis=1),
                           self.out_comm_final, week=week,flag=flag,save=save)
        else:
            p2p.plot_dispatch_comm(self.df_out.PV*self.PV_size_comm, 
                               self.df_out.loc[:,self.df_com.iloc[:,0]].sum(axis=1),
                               self.out_comm_final,prices=prices, week=week,flag=flag,save=save)
            
    def plotIndividual(self,hhNumber=1,week=10):
        # Single household test
        inv_size=max(self.selection.PV_size[hhNumber],self.param_tech['MaxPower'])
        out_aux=p2p.dispatch_max_sc(self.df_out.PV*self.selection.PV_size[hhNumber],
                                    self.df_out.loc[:,str(self.selection.name[hhNumber])],
                                inv_size,self.param_tech)

        aux=p2p.print_analysis_prices(self.df_out.PV*self.selection.PV_size[hhNumber],
                  self.df_out.loc[:,str(self.selection.name[hhNumber])],self.retail_price_sc,
                  self.export_price_sc,self.param_tech,out_aux,hhNumber)

        p2p.plot_dispatch(self.df_out.PV*self.selection.PV_size[hhNumber],
                          self.df_out.loc[:,str(self.selection.name[hhNumber])],out_aux,week=week)
        return out_aux,aux        


class SCCommunity(Community):
    
    def getSelfConsumptionCommunity(self):
        self.nested_out={}
        j=0
        k=0
        self.sum_bill=0
        self.PV_size_comm=0
        self.result_out={}
        for i in self.selection.index:
            print(i, end='')
            if self.PtC!=False:
                if self.selection.loc[i,'PV_size']>0:#all with PV
                    inv_size=max(self.param_tech['MaxPower'],
                                      self.selection.PV_size[i])#selection.PV_size[i]/ILR
                    self.PV_size_comm+=self.selection.PV_size[i]
                    if self.selection.loc[i,'battery']==True: #if battery
                        self.nested_out[i]=p2p.dispatch_max_sc(self.df_out.PV*self.selection.PV_size[i],
                                                          self.df_out.loc[:,str(self.selection.name[i])],
                                                          inv_size,self.param_tech)
                        j+=1
                        self.result_out[i]=p2p.print_analysis_prices(self.df_out.PV*self.selection.PV_size[i],
                                             self.df_out.loc[:,str(self.selection.name[i])],
                                             self.retail_price_sc,self.export_price_sc,
                                             self.param_tech, self.nested_out[i],isCommunity=False,hh=i,print_all=False)
                        self.result_out[i]['type']='PV_batt'
                    else: #if only PV battery=0 kWh
                        self.nested_out[i]=p2p.dispatch_max_sc(self.df_out.PV*self.selection.PV_size[i],
                                           self.df_out.loc[:,str(self.selection.name[i])],
                                           inv_size,self.param_tech_no_batt)
                        k+=1
                        self.result_out[i]=p2p.print_analysis_prices(self.df_out.PV*self.selection.PV_size[i],
                                            self.df_out.loc[:,str(self.selection.name[i])],
                                            self.retail_price_sc,self.export_price_sc,
                                            self.param_tech_no_batt,self. nested_out[i],
                                            isCommunity=False,hh=i,print_all=False)
                        self.result_out[i]['type']='PV'
                else: #No PV
                    self.nested_out[i]=p2p.dispatch_max_sc(self.df_out.PV*0,self.df_out.loc[:,str(self.selection.name[i])],
                                       0,self.param_tech_no_batt)
                    self.result_out[i]=p2p.print_analysis_prices(self.df_out.PV*0,
                                         self.df_out.loc[:,str(self.selection.name[i])],
                                         self.retail_price_sc,self.export_price_sc,
                                         self.param_tech_no_batt, self.nested_out[i],
                                         isCommunity=False,hh=i,print_all=False)
                    self.result_out[i]['type']='No'

                self.sum_bill+=self.result_out[i]['bill']
            else:
                if self.selection.loc[i,'sub_'+str(self.pv_penetration)+'_100']:#all with PV
                    inv_size=max(self.param_tech['MaxPower'],
                                      self.selection.PV_size[i])#selection.PV_size[i]/ILR
                    self.PV_size_comm+=self.selection.PV_size[i]
                    if self.selection.loc[i,'sub_'+str(self.pv_penetration)+'_'+
                                          str(self.batt_penetration)]: #if battery
                        self.nested_out[i]=p2p.dispatch_max_sc(self.df_out.PV*self.selection.PV_size[i],
                                                          self.df_out.loc[:,str(self.selection.name[i])],
                                                          inv_size,self.param_tech)
                        j+=1
                        self.result_out[i]=p2p.print_analysis_prices(self.df_out.PV*self.selection.PV_size[i],
                                             self.df_out.loc[:,str(self.selection.name[i])],
                                             self.retail_price_sc,self.export_price_sc,
                                             self.param_tech, self.nested_out[i],isCommunity=False,hh=i,print_all=False)
                        self.result_out[i]['type']='PV_batt'
                    else: #if only PV battery=0 kWh
                        self.nested_out[i]=p2p.dispatch_max_sc(self.df_out.PV*self.selection.PV_size[i],
                                           self.df_out.loc[:,str(self.selection.name[i])],
                                           inv_size,self.param_tech_no_batt)
                        k+=1
                        self.result_out[i]=p2p.print_analysis_prices(self.df_out.PV*self.selection.PV_size[i],
                                            self.df_out.loc[:,str(self.selection.name[i])],
                                            self.retail_price_sc,self.export_price_sc,
                                            self.param_tech_no_batt,self. nested_out[i],
                                            isCommunity=False,hh=i,print_all=False)
                        self.result_out[i]['type']='PV'
                else: #No PV
                    self.nested_out[i]=p2p.dispatch_max_sc(self.df_out.PV*0,self.df_out.loc[:,str(self.selection.name[i])],
                                       0,self.param_tech_no_batt)
                    self.result_out[i]=p2p.print_analysis_prices(self.df_out.PV*0,
                                         self.df_out.loc[:,str(self.selection.name[i])],
                                         self.retail_price_sc,self.export_price_sc,
                                         self.param_tech_no_batt, self.nested_out[i],
                                         isCommunity=False,hh=i,print_all=False)
                    self.result_out[i]['type']='No'

                self.sum_bill+=self.result_out[i]['bill']
    
    def runAll(self):
        self.createParams()
        self.selectData()
        self.getSelfConsumptionCommunity()
        self.getDataSingleDict()
        self.out=p2p.print_analysis_prices(self.df_out.PV*self.PV_size_comm, 
                 self.df_out.loc[:,self.df_com.iloc[:,0]].sum(axis=1),self.retail_price_sc,
                 self.export_price_sc, self.param_tech_comm,self.out_comm_final,
                 isCommunity=True)
        self.returnHousehold()
        
        self.updateOut()
    def runPartial(self):
        
        self.getSelfConsumptionCommunity()
        self.getDataSingleDict()
        self.out=p2p.print_analysis_prices(self.df_out.PV*self.PV_size_comm, 
                 self.df_out.loc[:,self.df_com.iloc[:,0]].sum(axis=1),self.retail_price_sc,
                 self.export_price_sc, self.param_tech_comm,self.out_comm_final,
                 isCommunity=True)
        self.returnHousehold()
        self.updateOut()
        

class P2PCommunity(Community):
    def getPrices(self,SCCommunity):
        self.Comm='P2P'
        self.createParams()
        self.selectData()
        df_bhv=pd.read_csv('../Input/table_bhv.csv',index_col=[0])
        
        if self.predet_bhv==False:
            tmp_pair=pd.DataFrame(map(lambda X: dict({'id':X[0],'hh':X[1]}), 
                                      list(zip(list(self.selection.name.unique()),
                                      list(df_bhv[df_bhv.will==1].hh.unique())))))
        elif self.predet_bhv=='high':
            df_sel=df_bhv[df_bhv.will]
            upper=df_sel.groupby('hh').sum().sell.median()+df_sel.groupby('hh').sum().sell.std()
            sell_group=df_sel.groupby('hh').sum().sell
            high_sell_group=sell_group[sell_group>upper]
            tmp_pair=pd.DataFrame(map(lambda X: dict({'id':X[0],'hh':X[1]}), list(zip(list(self.selection.name.unique()),cycle(list(df_bhv[df_bhv.hh.isin(high_sell_group.index)].hh.unique()))))))
        elif self.predet_bhv=='low':
            df_sel=df_bhv[df_bhv.will]
            lower=df_sel.groupby('hh').sum().sell.median()-df_sel.groupby('hh').sum().sell.std()
            sell_group=df_sel.groupby('hh').sum().sell
            low_sell_group=sell_group[sell_group<lower]
            tmp_pair=pd.DataFrame(map(lambda X: dict({'id':X[0],'hh':X[1]}), 
                  list(zip(list(self.selection.name.unique()),
                   cycle(list(df_bhv[df_bhv.hh.isin(low_sell_group.index)].hh.unique()))))))
        elif self.predet_bhv=='medium':
            df_sel=df_bhv[df_bhv.will]
            lower=df_sel.groupby('hh').sum().sell.median()-df_sel.groupby('hh').sum().sell.std()
            upper=df_sel.groupby('hh').sum().sell.median()+df_sel.groupby('hh').sum().sell.std()
            sell_group=df_sel.groupby('hh').sum().sell
            low_sell_group=sell_group[(sell_group>lower)&(sell_group<upper)]
            tmp_pair=pd.DataFrame(map(lambda X: dict({'id':X[0],'hh':X[1]}), 
                  list(zip(list(self.selection.name.unique()),
                   cycle(list(df_bhv[df_bhv.hh.isin(low_sell_group.index)].hh.unique()))))))
        elif self.predet_bhv=='cuts':
            df_sel=df_bhv[df_bhv.will]
            labels=['q0', 'q10', 'q20', 'q30', 'q40', 'q50', 'q60', 'q70', 'q80', 'q90']#quartiles
            sell_group=df_sel.groupby('hh').sum().sell
            cuts=pd.qcut(sell_group,10,labels=labels)
            tmp_pair=pd.DataFrame(map(lambda X: dict({'id':X[0],'hh':X[1]}), 
                  list(zip(list(self.selection.name.unique()),
                   cycle(list(df_bhv[df_bhv.hh.isin(sell_group[cuts==self.cut].index)].hh.unique()))))))
            
        self.df_bhv_paired=pd.merge(left=tmp_pair, right=df_bhv, left_on='hh', right_on='hh')
        
        self.df_bhv_paired.Price/=100
        self.df_bhv_paired['Price_binned']=(np.digitize(self.df_bhv_paired.Price,
                                                        [0.04,0.12,0.20,0.28]))
        self.df_bhv_paired['SOC_binned']=(np.digitize(self.df_bhv_paired.SOC,[30,60,90]))
        
        self.param_tech.update({'Price':[0.04,0.12,0.20,0.28]})
        self.param_tech_comm.update({'Price':[0.04,0.12,0.20,0.28]})
        self.param_tech_no_batt.update({'Price':[0.04,0.12,0.20,0.28]})
        self.df_prices=p2p.price_generation(SCCommunity.out_comm_final, self.param_tech_no_batt)
        self.prices_binned=np.digitize(self.df_prices,bins=[0.04,0.12,0.20,0.28])
        self.prices_binned[self.prices_binned==0]=1
        if self.test_sc:
            self.df_bhv_paired.loc[:,'sell']=False#this creates a P2P market where nobody want to sell
    def getP2PCommunity(self):

        self.nested_out={}
        #print_analysis_prices(pv, demand,retail,export, param, E,isCommunity=False,hh=None,print_all=False):

        j=0
        k=0
        self.PV_size_comm=0
        for i in self.selection.index:
            print(i, end='')
            if self.PtC!=False:
                if self.f.loc[i,'PV_size']>0:#all with PV
                    inv_size=max(self.param_tech['MaxPower'],self.selection.PV_size[i])#selection.PV_size[i]/ILR
                    self.PV_size_comm+=self.selection.PV_size[i]

                    if self.selection.loc[i,'battery']: #if battery
                        self.nested_out[i]=p2p.dispatch_max_sc_bhv(self.df_out.PV*self.selection.PV_size[i],
                                               self.df_out.loc[:,str(self.selection.name[i])],
                                              self.df_bhv_paired[self.df_bhv_paired.id==self.selection.name[i]],
                                               self.prices_binned,inv_size,self.param_tech)
                        j+=1
                    else: #if only PV battery=0 kWh, thus no bhv needed
                        self.nested_out[i]=p2p.dispatch_max_sc(self.df_out.PV*self.selection.PV_size[i],
                                                   self.df_out.loc[:,str(self.selection.name[i])],
                                                   inv_size,self.param_tech_no_batt)

                        k+=1
                else: #No PV, thus no bhv needed
                    self.nested_out[i]=p2p.dispatch_max_sc(self.df_out.PV*0,
                                               self.df_out.loc[:,str(self.selection.name[i])],
                                               0,self.param_tech_no_batt)
            else:
                if self.selection.loc[i,'sub_'+str(self.pv_penetration)+'_100']:#all with PV
                    inv_size=max(self.param_tech['MaxPower'],self.selection.PV_size[i])#selection.PV_size[i]/ILR
                    self.PV_size_comm+=self.selection.PV_size[i]

                    if self.selection.loc[i,'sub_'+str(self.pv_penetration)+'_'+str(self.batt_penetration)]: #if battery
                        self.nested_out[i]=p2p.dispatch_max_sc_bhv(self.df_out.PV*self.selection.PV_size[i],
                                               self.df_out.loc[:,str(self.selection.name[i])],
                                              self.df_bhv_paired[self.df_bhv_paired.id==self.selection.name[i]],
                                               self.prices_binned,inv_size,self.param_tech)
                        j+=1
                    else: #if only PV battery=0 kWh, thus no bhv needed
                        self.nested_out[i]=p2p.dispatch_max_sc(self.df_out.PV*self.selection.PV_size[i],
                                                   self.df_out.loc[:,str(self.selection.name[i])],
                                                   inv_size,self.param_tech_no_batt)

                        k+=1
                else: #No PV, thus no bhv needed
                    self.nested_out[i]=p2p.dispatch_max_sc(self.df_out.PV*0,
                                               self.df_out.loc[:,str(self.selection.name[i])],
                                               0,self.param_tech_no_batt)
        self.getDataSingleDict()
        self.result_out={}
        #print_analysis_prices(pv, demand,retail,export, param, E,isCommunity=False,hh=None,print_all=False):
        self.sum_bill=0

        j=0
        k=0
        for i in self.selection.index:
            print(i, end='')
            if self.PtC!=False:
                if self.selection.loc[i,'PV_size']>0:#all with PV
                    inv_size=max(self.param_tech['MaxPower'],self.selection.PV_size[i])#selection.PV_size[i]/ILR

                    if self.selection.loc[i,'battery']: #if battery
                        self.result_out[i]=p2p.print_analysis_prices(self.df_out.PV*self.selection.PV_size[i],
                                                 self.df_out.loc[:,str(self.selection.name[i])],self.df_prices,
                                                 self.df_prices,self.param_tech,self.nested_out[i],
                                                 isCommunity=False,hh=i,print_all=False)
                        self.result_out[i]['bill']=p2p.bill_hh_p2p(self.df_out.PV*self.selection.PV_size[i],
                                                 self.df_out.loc[:,str(self.selection.name[i])],self.df_prices,
                                                 self.export_price_sc,self.out_comm_final['inv2grid'], 
                                                 self.nested_out[i],self.param_tech)
                        self.result_out[i]['type']='PV_batt'
                        j+=1
                    else: #if only PV battery=0 kWh, thus no bhv needed
                        self.result_out[i]=p2p.print_analysis_prices(self.df_out.PV*self.selection.PV_size[i],
                                             self.df_out.loc[:,str(self.selection.name[i])],self.df_prices,
                                             self.df_prices,self.param_tech_no_batt, 
                                             self.nested_out[i],isCommunity=False,hh=i,print_all=False)
                        self.result_out[i]['bill']=p2p.bill_hh_p2p(self.df_out.PV*self.selection.PV_size[i],
                                                   self.df_out.loc[:,str(self.selection.name[i])],
                                                   self.df_prices,self.export_price_sc,
                                                   self.out_comm_final['inv2grid'], self.nested_out[i],
                                                   self.param_tech_no_batt)
                        self.result_out[i]['type']='PV'
                        k+=1
                else: #No PV, thus no bhv needed

                    self.result_out[i]=p2p.print_analysis_prices(self.df_out.PV*0,
                                         self.df_out.loc[:,str(self.selection.name[i])],self.df_prices,
                                         self.df_prices,self.param_tech_no_batt, self.nested_out[i],
                                         isCommunity=False,hh=i,print_all=False)
                    self.result_out[i]['bill']=p2p.bill_hh_p2p(self.df_out.PV*0,
                                        self.df_out.loc[:,str(self.selection.name[i])],self.df_prices,
                                        self.export_price_sc,self.out_comm_final['inv2grid'],
                                        self.nested_out[i],self.param_tech_no_batt)
                    self.result_out[i]['type']='No'
            else:
                if self.selection.loc[i,'sub_'+str(self.pv_penetration)+'_100']:#all with PV
                    inv_size=max(self.param_tech['MaxPower'],self.selection.PV_size[i])#selection.PV_size[i]/ILR

                    if self.selection.loc[i,'sub_'+str(self.pv_penetration)+'_'+str(self.batt_penetration)]: #if battery
                        self.result_out[i]=p2p.print_analysis_prices(self.df_out.PV*self.selection.PV_size[i],
                                                 self.df_out.loc[:,str(self.selection.name[i])],self.df_prices,
                                                 self.df_prices,self.param_tech,self.nested_out[i],
                                                 isCommunity=False,hh=i,print_all=False)
                        self.result_out[i]['bill']=p2p.bill_hh_p2p(self.df_out.PV*self.selection.PV_size[i],
                                                 self.df_out.loc[:,str(self.selection.name[i])],self.df_prices,
                                                 self.export_price_sc,self.out_comm_final['inv2grid'], 
                                                 self.nested_out[i],self.param_tech)
                        self.result_out[i]['type']='PV_batt'
                        j+=1
                    else: #if only PV battery=0 kWh, thus no bhv needed
                        self.result_out[i]=p2p.print_analysis_prices(self.df_out.PV*self.selection.PV_size[i],
                                             self.df_out.loc[:,str(self.selection.name[i])],self.df_prices,
                                             self.df_prices,self.param_tech_no_batt, 
                                             self.nested_out[i],isCommunity=False,hh=i,print_all=False)
                        self.result_out[i]['bill']=p2p.bill_hh_p2p(self.df_out.PV*self.selection.PV_size[i],
                                                   self.df_out.loc[:,str(self.selection.name[i])],
                                                   self.df_prices,self.export_price_sc,
                                                   self.out_comm_final['inv2grid'], self.nested_out[i],
                                                   self.param_tech_no_batt)
                        self.result_out[i]['type']='PV'
                        k+=1
                else: #No PV, thus no bhv needed

                    self.result_out[i]=p2p.print_analysis_prices(self.df_out.PV*0,
                                         self.df_out.loc[:,str(self.selection.name[i])],self.df_prices,
                                         self.df_prices,self.param_tech_no_batt, self.nested_out[i],
                                         isCommunity=False,hh=i,print_all=False)
                    self.result_out[i]['bill']=p2p.bill_hh_p2p(self.df_out.PV*0,
                                        self.df_out.loc[:,str(self.selection.name[i])],self.df_prices,
                                        self.export_price_sc,self.out_comm_final['inv2grid'],
                                        self.nested_out[i],self.param_tech_no_batt)
                    self.result_out[i]['type']='No'

            self.sum_bill+=self.result_out[i]['bill']

    
    def getKPI(self,SCCommunity):
        print(self.out['SSR']>SCCommunity.out['SSR'])
        print(self.out['SSR']-SCCommunity.out['SSR'])

        print(self.out['SCR']>SCCommunity.out['SCR'])
        print(self.out['SCR']-SCCommunity.out['SCR'])


        a=pd.DataFrame(self.out_comm_final['grid2load']-self.out_comm_final['inv2grid'])#create the grid exchange column
        b=pd.DataFrame(SCCommunity.out_comm_final['grid2load']-SCCommunity.out_comm_final['inv2grid'])#create the grid exchange column


        sc_peak=(b.groupby([b.index.month,b.index.day]).max()-b.groupby([b.index.month,b.index.day]).min()).reset_index()#peak-to-peak difference inside sc per day
        p2p_peak=(a.groupby([a.index.month,a.index.day]).max()-a.groupby([a.index.month,a.index.day]).min()).reset_index()#peak-to-peak difference inside p2p per day
        out_peak=pd.concat([sc_peak,p2p_peak],axis=1,ignore_index=True)
        out_peak.columns=['Month','Day','SC','Month2','Day2','P2P']

        p2p_week=(self.out_comm_final['grid2load']-self.out_comm_final['inv2grid']).groupby([self.out_comm_final['grid2load'].index.dayofweek,
                                                     self.out_comm_final['grid2load'].index.hour]).mean()
        sc_week=(SCCommunity.out_comm_final['grid2load']-SCCommunity.out_comm_final['inv2grid']).groupby([SCCommunity.out_comm_final['grid2load'].index.dayofweek,
                                                 SCCommunity.out_comm_final['grid2load'].index.hour]).mean()


        p2p_day=(self.out_comm_final['grid2load']-self.out_comm_final['inv2grid']).groupby([self.out_comm_final['grid2load'].index.hour]).mean()
        sc_day=(SCCommunity.out_comm_final['grid2load']-SCCommunity.out_comm_final['inv2grid']).groupby([SCCommunity.out_comm_final['grid2load'].index.hour]).mean()

        if self.parallel==False:
            week_power=pd.concat([p2p_week.rename('p2p'),sc_week.rename('sc')],axis=1).to_csv('../Output/week_power.csv')
        else:
            week_power=pd.concat([p2p_week.rename('p2p'),sc_week.rename('sc')],axis=1)
        
        day_power=pd.concat([p2p_day.rename('p2p'),sc_day.rename('sc')],axis=1)

        #The P2P peak-to-peak measure is lower than in the SC one

        (p2p_peak<sc_peak).sum()[0]/365
        

        print(self.out['ADME']>SCCommunity.out['ADME'])# False is good
        print(self.out['ADME']-SCCommunity.out['ADME'])

        print(self.out['ADMD']>SCCommunity.out['ADMD'])
        print(self.out['ADMD']-SCCommunity.out['ADMD'])

        print(self.out['EPARI']>SCCommunity.out['EPARI'])
        print(self.out['EPARI']-SCCommunity.out['EPARI'])

        print(self.out['EPARI'])
        print(SCCommunity.out['EPARI'])
        
        self.PWI=(((pd.DataFrame([SCCommunity.result_out[i]['bill'] for i in range(SCCommunity.community_size)])-
            pd.DataFrame([self.result_out[i]['bill'] for i in range(self.community_size)]))>0).sum()/
        self.community_size).values[0]
        
        aux=pd.DataFrame(np.array([[SCCommunity.result_out[i]['bill'] for i in range(SCCommunity.community_size)],
                           [self.result_out[i]['bill'] for i in range(self.community_size)],
                           [SCCommunity.result_out[i]['type'] for i in range(SCCommunity.community_size)]]).T,
             columns=['SC','P2P','type'])

        aux.loc[aux.type=='PV_batt','weight']=4
        aux.loc[aux.type=='PV','weight']=2
        aux.loc[aux.type=='No','weight']=1

        Gini_rsv(np.array(aux.SC.astype(float)),aux.weight)

        Gini_rsv(np.array(aux.P2P.astype(float)),aux.weight)
        
        print(sum([SCCommunity.result_out[i]['bill'] for i in range(SCCommunity.community_size)])**2/(SCCommunity.community_size*sum([SCCommunity.result_out[i]['bill']**2 for i in range(SCCommunity.community_size)])).round(2))# benefit index
        print(sum([self.result_out[i]['bill'] for i in range(self.community_size)])**2/(self.community_size*sum([self.result_out[i]['bill']**2 for i in range(self.community_size)])).round(2))# benefit index
        
    def endP2P(self):
        self.out=p2p.print_analysis_prices(self.df_out.PV*self.PV_size_comm, 
                 self.df_out.loc[:,self.df_com.iloc[:,0]].sum(axis=1),self.retail_price_sc,
                 self.export_price_sc, self.param_tech_comm,self.out_comm_final,
                 isCommunity=True)
        self.returnHousehold()
        self.updateOut()

    def runAll(self,SCCommunity):
        self.getPrices(SCCommunity)
        self.getP2PCommunity()
        self.returnHousehold()
        self.endP2P()
        self.getKPI(SCCommunity)
        self.updateOut()
        
        if self.parallel:
            del(self.out)        
    