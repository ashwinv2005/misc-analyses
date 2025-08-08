% Model of Kéfi et al. TPB 2007
% Asynchronous CA
% Program associated to the functions Neighbors and Nextstep
% (these functions should be in the same folder as this main program)
% Sonia Kéfi, March 2010
%--------------------------------------------------------------------------

clear all 
close all

global n m f c b d r del z deltat

tic

% Parameter values
% -------------------------------------------------------------------------
z = 4 ;        % Size of the neighborhood
c = 0.3 ;      % Competition
d = 0.2 ;      % Degradation
r = 0.0001 ;   % Regeneration
m = 0.15 ;      % Mortality
f = 0.9 ;      % Facilitation
b = 0.9 ;      % Establishment without competition
del = 0.1 ;    % Seed dispersal
deltat = 0.1 ; % Time step of the CA (numerical time step)
n = 500 ;      % Size of the lattice n*n
tfin = 100 ;  % Length of the simulation - Total number of time steps run = tfin/deltat

% vegetation = zeros(3000,1);
% degraded = zeros(3000,1);
den = zeros(25,1);
count = 0;

for b = linspace(1,0.1,25)
count = count + 1;
count
% Matrix initialisation
% -------------------------------------------------------------------------
M = rand(n,n) ;% Creates a matrix of size n*n with random number in each cell (between 0 and 1)
po = 0.6 ;     % Proportion of vegetation in the lattice initially
pd = 0.2 ;     % 1-pd-po = proportion of empty sites in the lattice initially
NbOcc = 0 ;    % Total number of occupied cells in the lattice
NbDe = 0 ;     % Total number of degraded cells in the lattice
% dens = zeros(1000,1);

% Fills in the matrix with vegetated, empty and degraded cells
for i=1:n
    for k = 1:n
        if M(i,k)<po
            M(i,k) = 1 ; % cell occupied by vegetation
            NbOcc = NbOcc + 1 ;
        elseif M(i,k)>pd + po
            M(i,k) = 2 ; % empty cell
        else M(i,k) = 3 ; % degraded cell
            NbDe = NbDe + 1 ;
        end
    end
end

% vegetation(1) = NbOcc/(n*n) ; % density of vegetation in the lattice
% degraded(1) = NbDe/(n*n) ; % density of degraded cells in the lattice

% Initialisation of the number of neighbors
V = Neighbors(M) ;

% Evolution of the system
%--------------------------------------------------------------------------
tmax = tfin/deltat ; 
% tfin is the number of "biological step", e.g. 5 years, 
% whereas tmax is the number of numerical iteration.
% If deltat = 0.1, 5 years is reached in 50 numerical time steps

for t = 0 : 1448
    
    t
    % dens(t+1) = (1000000-length(M(M==1)))/1000000;
    % The evolution of the system is done by calling the function NextStep
    [M] = NextStep(M,V,NbOcc,NbDe) ;
    
    % Plot the CA at each time step
    % comment this part for the code to run faster
%     black = [0 0 0] ;
%     gray = [0.7 0.7 0.7] ;
%     white = [1 1 1] ;
%     Satecolormap = [black; gray; white] ; % 1=back 2=grey 3=white
%     colormap(Satecolormap) ;
%     image(M) ; 
%     axis off ;
%     drawnow
    
    % Update of the densities and neighborhood
    NbOcc = sum(sum(M==1)) ;
    NbDe = sum(sum(M==3)) ;
    % vegetation(t+2) = NbOcc/(n*n) ;
    % degraded(t+2) = NbDe/(n*n) ;
    V = Neighbors(M) ;
    
end %Fin boucle sur t
den(count) = NbOcc/(n*n);
end
% Plots
% -------------------------------------------------------------------------
%%%%%%%%% to delete

den1 = zeros(25,1);
count = 0;
m = 0.2;

for b = linspace(1,0.1,25)
count = count + 1;
count
% Matrix initialisation
% -------------------------------------------------------------------------
M = rand(n,n) ;% Creates a matrix of size n*n with random number in each cell (between 0 and 1)
po = 0.6 ;     % Proportion of vegetation in the lattice initially
pd = 0.2 ;     % 1-pd-po = proportion of empty sites in the lattice initially
NbOcc = 0 ;    % Total number of occupied cells in the lattice
NbDe = 0 ;     % Total number of degraded cells in the lattice
% dens = zeros(1000,1);

% Fills in the matrix with vegetated, empty and degraded cells
for i=1:n
    for k = 1:n
        if M(i,k)<po
            M(i,k) = 1 ; % cell occupied by vegetation
            NbOcc = NbOcc + 1 ;
        elseif M(i,k)>pd + po
            M(i,k) = 2 ; % empty cell
        else M(i,k) = 3 ; % degraded cell
            NbDe = NbDe + 1 ;
        end
    end
end

% vegetation(1) = NbOcc/(n*n) ; % density of vegetation in the lattice
% degraded(1) = NbDe/(n*n) ; % density of degraded cells in the lattice

% Initialisation of the number of neighbors
V = Neighbors(M) ;

% Evolution of the system
%--------------------------------------------------------------------------
tmax = tfin/deltat ; 
% tfin is the number of "biological step", e.g. 5 years, 
% whereas tmax is the number of numerical iteration.
% If deltat = 0.1, 5 years is reached in 50 numerical time steps

for t = 0 : 1448
    
    t
    % dens(t+1) = (1000000-length(M(M==1)))/1000000;
    % The evolution of the system is done by calling the function NextStep
    [M] = NextStep(M,V,NbOcc,NbDe) ;
    
    % Plot the CA at each time step
    % comment this part for the code to run faster
%     black = [0 0 0] ;
%     gray = [0.7 0.7 0.7] ;
%     white = [1 1 1] ;
%     Satecolormap = [black; gray; white] ; % 1=back 2=grey 3=white
%     colormap(Satecolormap) ;
%     image(M) ; 
%     axis off ;
%     drawnow
    
    % Update of the densities and neighborhood
    NbOcc = sum(sum(M==1)) ;
    NbDe = sum(sum(M==3)) ;
    % vegetation(t+2) = NbOcc/(n*n) ;
    % degraded(t+2) = NbDe/(n*n) ;
    V = Neighbors(M) ;
    
end %Fin boucle sur t
den1(count) = NbOcc/(n*n);
end

%%%%%%%%%%%%% to delete

%%%%%%%%% to delete

den2 = zeros(25,1);
count = 0;
m = 0.25;

for b = linspace(1,0.1,25)
count = count + 1;
count
% Matrix initialisation
% -------------------------------------------------------------------------
M = rand(n,n) ;% Creates a matrix of size n*n with random number in each cell (between 0 and 1)
po = 0.6 ;     % Proportion of vegetation in the lattice initially
pd = 0.2 ;     % 1-pd-po = proportion of empty sites in the lattice initially
NbOcc = 0 ;    % Total number of occupied cells in the lattice
NbDe = 0 ;     % Total number of degraded cells in the lattice
% dens = zeros(1000,1);

% Fills in the matrix with vegetated, empty and degraded cells
for i=1:n
    for k = 1:n
        if M(i,k)<po
            M(i,k) = 1 ; % cell occupied by vegetation
            NbOcc = NbOcc + 1 ;
        elseif M(i,k)>pd + po
            M(i,k) = 2 ; % empty cell
        else M(i,k) = 3 ; % degraded cell
            NbDe = NbDe + 1 ;
        end
    end
end

% vegetation(1) = NbOcc/(n*n) ; % density of vegetation in the lattice
% degraded(1) = NbDe/(n*n) ; % density of degraded cells in the lattice

% Initialisation of the number of neighbors
V = Neighbors(M) ;

% Evolution of the system
%--------------------------------------------------------------------------
tmax = tfin/deltat ; 
% tfin is the number of "biological step", e.g. 5 years, 
% whereas tmax is the number of numerical iteration.
% If deltat = 0.1, 5 years is reached in 50 numerical time steps

for t = 0 : 1448
    
    t
    % dens(t+1) = (1000000-length(M(M==1)))/1000000;
    % The evolution of the system is done by calling the function NextStep
    [M] = NextStep(M,V,NbOcc,NbDe) ;
    
    % Plot the CA at each time step
    % comment this part for the code to run faster
%     black = [0 0 0] ;
%     gray = [0.7 0.7 0.7] ;
%     white = [1 1 1] ;
%     Satecolormap = [black; gray; white] ; % 1=back 2=grey 3=white
%     colormap(Satecolormap) ;
%     image(M) ; 
%     axis off ;
%     drawnow
    
    % Update of the densities and neighborhood
    NbOcc = sum(sum(M==1)) ;
    NbDe = sum(sum(M==3)) ;
    % vegetation(t+2) = NbOcc/(n*n) ;
    % degraded(t+2) = NbDe/(n*n) ;
    V = Neighbors(M) ;
    
end %Fin boucle sur t
den2(count) = NbOcc/(n*n);
end

%%%%%%%%%%%%% to delete

black = [0 0 0] ;
gray = [0.7 0.7 0.7] ;
white = [1 1 1] ;
Satecolormap = [black; gray; white] ; % 1=noir 2=gris et 3=white
colormap(Satecolormap) ;
image(M) ; axis off ;
title('Final state of the system')

figure ;
plot(vegetation,'k')
hold on
plot(degraded,'b')
legend('Vegetation density','Degraded site density','Location','NorthWest')
legend boxoff
xlabel('Time')
ylabel('Densities')
title('Evolution of the global densities')

b = 0.4 ;

for t = tmax+1 : 3*tmax
    
    % The evolution of the system is done by calling the function NextStep
    [M] = NextStep(M,V,NbOcc,NbDe) ;
    
    % Plot the CA at each time step
    % comment this part for the code to run faster
%     black = [0 0 0] ;
%     gray = [0.7 0.7 0.7] ;
%     white = [1 1 1] ;
%     Satecolormap = [black; gray; white] ; % 1=back 2=grey 3=white
%     colormap(Satecolormap) ;
%     image(M) ; 
%     axis off ;
%     drawnow
    
    % Update of the densities and neighborhood
    NbOcc = sum(sum(M==1)) ;
    NbDe = sum(sum(M==3)) ;
    vegetation(t+2) = NbOcc/(n*n) ;
    degraded(t+2) = NbDe/(n*n) ;
    V = Neighbors(M) ;
    
end %Fin boucle sur t

figure 
plot(vegetation,'k')
hold on
plot(degraded,'b')
legend('Vegetation density','Degraded site density','Location','NorthWest')
legend boxoff
xlabel('Time')
ylabel('Densities')
title('Evolution of the global densities')

toc

plot(den2);
arid = transpose(arid);