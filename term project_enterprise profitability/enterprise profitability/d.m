% Program to simulate an AR(1) process and plot impulse responses. 
% Diego Vilan - Spring 2011

%Housekeeping
clc;
clear all;


% Inital set-up values:

rho = 0.95;
T = 150;
x = zeros(T,1);
ex = randn(T,1);
ee = zeros(T-50,1);
y = zeros(T,1);

 
ey = [ex(1:50);ex(51)+1;ex(52:T)];
 
 
% Simulate AR(1) process: y_{t} = rho * y_{t-1} + e_{t}
% where e_{t} follows a standard normal distribution.

for t = 1:(T-1)
  x(t+1) = rho*x(t) + ex(t+1);
  y(t+1) = rho*y(t) + ey(t+1);
end


% Compute difference with original series:

 w=y-x;



% Plot graphs:

figure(1)

subplot(2,1,1)
plot((1:T),x,'b',(1:T),y,'r');
xlabel('Period');
%ylabel('Simulated series');
title('AR(1) Simulation');


subplot(2,1,2)
plot((50:T),w(50:T));
xlabel('Period');
%ylabel('Impulse Response');
title('Impulse Response Analysis');
