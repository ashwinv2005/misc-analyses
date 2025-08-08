function [py,count2] = boundpixel(bw,cc,ps,obno)
py = zeros(ps(obno)*4,2);
bwtemp = zeros(size(bw));
bwtemp(cc.PixelIdxList{obno}) = 1;
count2 = 0;
for i=1:ps(obno)
    temp = cc.PixelIdxList{obno}(i);
    [tempi tempj] = convertij(bw,temp);
    if tempi == 1 && tempj == 1        
        if bw(tempi+1,tempj) == 0 || bw(tempi,tempj+1) == 0
            if bwtemp(tempi+1,tempj) == 0 
                count2=count2+1;
                py(count2,1) = tempi+1;
                py(count2,2) = tempj;
                bwtemp(tempi+1,tempj) = 1;
            end
            if bwtemp(tempi,tempj+1) == 0
                count2=count2+1;
                py(count2,1) = tempi;
                py(count2,2) = tempj+1;
                bwtemp(tempi,tempj+1) = 1;
            end
        end
    end
    if tempi == 1 && tempj == size(bw,2)
        if bw(tempi+1,tempj) == 0 || bw(tempi,tempj-1) == 0
            if bwtemp(tempi+1,tempj) == 0 
                count2=count2+1;
                py(count2,1) = tempi+1;
                py(count2,2) = tempj;
                bwtemp(tempi+1,tempj) = 1;
            end
            if bwtemp(tempi,tempj-1) == 0
                count2=count2+1;
                py(count2,1) = tempi;
                py(count2,2) = tempj-1;
                bwtemp(tempi,tempj-1) = 1;
            end
        end
    end
    if tempi == 1 && tempj ~= size(bw,2) && tempj ~= 1
        if bw(tempi+1,tempj) == 0 || bw(tempi,tempj-1) == 0 || bw(tempi,tempj+1) == 0
            if bwtemp(tempi+1,tempj) == 0 
                count2=count2+1;
                py(count2,1) = tempi+1;
                py(count2,2) = tempj;
                bwtemp(tempi+1,tempj) = 1;
            end
            if bwtemp(tempi,tempj-1) == 0
                count2=count2+1;
                py(count2,1) = tempi;
                py(count2,2) = tempj-1;
                bwtemp(tempi,tempj-1) = 1;
            end
            if bwtemp(tempi,tempj+1) == 0
                count2=count2+1;
                py(count2,1) = tempi;
                py(count2,2) = tempj+1;
                bwtemp(tempi,tempj+1) = 1;
            end
        end
    end    
    if tempi == size(bw,1) && tempj == 1
        if bw(tempi-1,tempj) == 0 || bw(tempi,tempj+1) == 0
            if bwtemp(tempi-1,tempj) == 0 
                count2=count2+1;
                py(count2,1) = tempi-1;
                py(count2,2) = tempj;
                bwtemp(tempi-1,tempj) = 1;
            end
            if bwtemp(tempi,tempj+1) == 0
                count2=count2+1;
                py(count2,1) = tempi;
                py(count2,2) = tempj+1;
                bwtemp(tempi,tempj+1) = 1;
            end
        end
    end
    if tempi == size(bw,1) && tempj == size(bw,2)
        if bw(tempi-1,tempj) == 0 || bw(tempi,tempj-1) == 0
            if bwtemp(tempi-1,tempj) == 0 
                count2=count2+1;
                py(count2,1) = tempi-1;
                py(count2,2) = tempj;
                bwtemp(tempi-1,tempj) = 1;
            end
            if bwtemp(tempi,tempj-1) == 0
                count2=count2+1;
                py(count2,1) = tempi;
                py(count2,2) = tempj-1;
                bwtemp(tempi,tempj-1) = 1;
            end
        end
    end
    if tempi == size(bw,1) && tempj ~= size(bw,2) && tempj ~= 1
        if bw(tempi-1,tempj) == 0 || bw(tempi,tempj-1) == 0 || bw(tempi,tempj+1) == 0
            if bwtemp(tempi-1,tempj) == 0 
                count2=count2+1;
                py(count2,1) = tempi-1;
                py(count2,2) = tempj;
                bwtemp(tempi-1,tempj) = 1;
            end
            if bwtemp(tempi,tempj-1) == 0
                count2=count2+1;
                py(count2,1) = tempi;
                py(count2,2) = tempj-1;
                bwtemp(tempi,tempj-1) = 1;
            end
            if bwtemp(tempi,tempj+1) == 0
                count2=count2+1;
                py(count2,1) = tempi;
                py(count2,2) = tempj+1;
                bwtemp(tempi,tempj+1) = 1;
            end
        end
    end            
    if tempi ~= 1 && tempi ~= size(bw,1) && tempj ~= size(bw,2) && tempj ~= 1
        if bw(tempi-1,tempj) == 0 || bw(tempi+1,tempj) == 0 || bw(tempi,tempj-1) == 0 || bw(tempi,tempj+1) == 0
            if bwtemp(tempi-1,tempj) == 0 
                count2=count2+1;
                py(count2,1) = tempi-1;
                py(count2,2) = tempj;
                bwtemp(tempi-1,tempj) = 1;
            end
            if bwtemp(tempi+1,tempj) == 0
                count2=count2+1;
                py(count2,1) = tempi+1;
                py(count2,2) = tempj;
                bwtemp(tempi+1,tempj) = 1;
            end
            if bwtemp(tempi,tempj-1) == 0
                count2=count2+1;
                py(count2,1) = tempi;
                py(count2,2) = tempj-1;
                bwtemp(tempi,tempj-1) = 1;
            end
            if bwtemp(tempi,tempj+1) == 0
                count2=count2+1;
                py(count2,1) = tempi;
                py(count2,2) = tempj+1;
                bwtemp(tempi,tempj+1) = 1;
            end
        end
    end            
    if tempi ~= 1 && tempi ~= size(bw,1) && tempj == 1
        if bw(tempi-1,tempj) == 0 || bw(tempi+1,tempj) == 0 || bw(tempi,tempj+1) == 0
            if bwtemp(tempi-1,tempj) == 0 
                count2=count2+1;
                py(count2,1) = tempi-1;
                py(count2,2) = tempj;
                bwtemp(tempi-1,tempj) = 1;
            end
            if bwtemp(tempi+1,tempj) == 0
                count2=count2+1;
                py(count2,1) = tempi+1;
                py(count2,2) = tempj;
                bwtemp(tempi+1,tempj) = 1;
            end
            if bwtemp(tempi,tempj+1) == 0
                count2=count2+1;
                py(count2,1) = tempi;
                py(count2,2) = tempj+1;
                bwtemp(tempi,tempj+1) = 1;
            end
        end
    end      
    if tempi ~= 1 && tempi ~= size(bw,1) && tempj == size(bw,2)
        if bw(tempi-1,tempj) == 0 || bw(tempi+1,tempj) == 0 || bw(tempi,tempj-1) == 0
            if bwtemp(tempi-1,tempj) == 0 
                count2=count2+1;
                py(count2,1) = tempi-1;
                py(count2,2) = tempj;
                bwtemp(tempi-1,tempj) = 1;
            end
            if bwtemp(tempi+1,tempj) == 0
                count2=count2+1;
                py(count2,1) = tempi+1;
                py(count2,2) = tempj;
                bwtemp(tempi+1,tempj) = 1;
            end
            if bwtemp(tempi,tempj-1) == 0
                count2=count2+1;
                py(count2,1) = tempi;
                py(count2,2) = tempj-1;
                bwtemp(tempi,tempj-1) = 1;
            end
        end
    end
end