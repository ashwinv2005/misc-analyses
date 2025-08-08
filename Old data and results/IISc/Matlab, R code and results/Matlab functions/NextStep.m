% Function associated with the main program AC_TPBmodel
% Calculates the state t+1 of the lattice knowing t

function [N] = NextStep(M,V,NbOcc,NbDe)

N = M ;
W = V ;
NbDe2 = NbDe ;
NbOcc2 = NbOcc ;

global n m f c b d r del z deltat

for i = 1 : n*n
    % identification of a cell randomly picked in the lattice
    S = floor(rand*(n*n-1)) + 1; % Picks a cell randomly between 1 and n*n
    Stest1 = floor(S/n) ;
    Stest2 = S/n ;
    if Stest1 == Stest2
        Si = floor(S/n) ;        % Corresponding row in the lattice
    else
        Si = floor(S/n) + 1 ;
    end
    Sj = mod(S,n) ;              % Corresponding col in the lattice
    if Si == 0
        Si = n ;
    end
    if Sj == 0
        Sj = n ;
    end
    
    % Change state
    test = rand ;
    if N(Si,Sj) == 1      % Vegetation
        if test < m*deltat
            N(Si,Sj) = 2 ;% Mortality
            z = mod(Sj-1+n,n) ;
            if z == 0
                z = n ;
            end
            W(Si,z) = W(Si,z) - 1 ;
            z = mod(Si-1+n,n) ;
            if z == 0
                z = n ;
            end
            W(z,Sj) = W(z,Sj) - 1 ;
            z = mod(Sj+1,n) ;
            if z == 0
                z = n ;
            end
            W(Si,z) = W(Si,z) - 1 ;
            z = mod(Si+1,n) ;
            if z == 0
                z = n ;
            end
            W(z,Sj) = W(z,Sj) - 1 ;
            NbOcc2 = NbOcc2 - 1 ;
        end
        
    elseif N(Si,Sj) == 3  % Degraded
        if (r + f*W(Si,Sj)/4)*deltat > 1
            disp('Transition - --> o certain. Decrease deltat')
        end
        if test < (r + f*W(Si,Sj)/4)*deltat
            N(Si,Sj) = 2 ;% Regeneration
            NbDe2 = NbDe2 - 1 ;
        end
        
    elseif test < ( (del*NbOcc2/(n*n)+(1-del)*W(Si,Sj)/4)*(b-c*NbOcc2/(n*n)) )*deltat
        if ( (del*NbOcc2/(n*n)+(1-del)*W(Si,Sj)/4)*(b-c*NbOcc2/(n*n)) + d )*deltat > 1
            disp('Transition o --> o impossible. Decrease deltat')
        end
        N(Si,Sj) = 1 ;   % Recolonisation
        z = mod(Sj-1+n,n) ;
        if z == 0
            z = n ;
        end
        W(Si,z) = W(Si,z) + 1 ;
        z = mod(Si-1+n,n) ;
        if z == 0
            z = n ;
        end
        W(z,Sj) = W(z,Sj) + 1 ;
        z = mod(Sj+1,n) ;
        if z == 0
            z = n ;
        end
        W(Si,z) = W(Si,z) + 1 ;
        z = mod(Si+1,n) ;
        if z == 0
            z = n ;
        end
        W(z,Sj) = W(z,Sj) + 1 ;
        NbOcc2 = NbOcc2 + 1 ;
        
    elseif  test > 1 - d*deltat
        N(Si,Sj) = 3 ; % Degradation
        NbDe2 = NbDe2 + 1 ;
        
    end % End of the loop on cell state (1, 2 or 3)
end % End of the loop picking n*n cells

