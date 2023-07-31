clear;clc
data=csvread('Aug18.csv');
%data=data(:,1:29);
X=[350:2500];
X=[X' interp1(data(:,1),data(:,2:end),X','linear')];
csvwrite('SLSC_Aug18_SpectInt.csv',X)
