@echo off
matlab.exe -batch "addpath mfiles\; addpath mfiles\latlonutm\; addpath PreProcess\; TrawlData5mmbin(%1, %2); exit;"
matlab.exe -batch "addpath mfiles\; addpath mfiles\latlonutm\; addpath PreProcess\; PullOutRecruitData; exit;"
matlab.exe -batch "addpath mfiles\; addpath mfiles\latlonutm\; addpath PreProcess\; ProcessRecruitData(%1, %2); exit;"
set dirGB=KrigingEstimates\SimGB
set dirMA=KrigingEstimates\SimMA
for /l %%y in (%1, 1, %2) do (
    if not exist %dirGB%%%y\ (
    mkdir %dirGB%%%y\
    )
    if not exist %dirMA%%%y\ (
    mkdir %dirMA%%%y\
    )
)
matlab.exe -batch "addpath mfiles\; addpath mfiles\latlonutm\; addpath PreProcess\; NearestNeighborRecInterp(%1, %2); exit;"
