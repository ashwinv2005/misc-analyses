function [px,count] = brpixel(bw,cc,ps,obno)
px = zeros(ps(obno),2);
count = 0;
for i=1:ps(obno)
    temp = cc.PixelIdxList{obno}(i);
    [tempi tempj] = convertij(bw,temp);
    if tempi == 1 && tempj == 1
        if bw(tempi+1,tempj) == 0 || bw(tempi,tempj+1) == 0
            count=count+1;
            px(count,1) = tempi;
            px(count,2) = tempj;
        end
    end
    if tempi == 1 && tempj == size(bw,2)
        if bw(tempi+1,tempj) == 0 || bw(tempi,tempj-1) == 0
            count=count+1;
            px(count,1) = tempi;
            px(count,2) = tempj;
        end
    end
    if tempi == 1 && tempj ~= size(bw,2) && tempj ~= 1
        if bw(tempi+1,tempj) == 0 || bw(tempi,tempj-1) == 0 || bw(tempi,tempj+1) == 0
            count=count+1;
            px(count,1) = tempi;
            px(count,2) = tempj;
        end
    end    
    if tempi == size(bw,1) && tempj == 1
        if bw(tempi-1,tempj) == 0 || bw(tempi,tempj+1) == 0
            count=count+1;
            px(count,1) = tempi;
            px(count,2) = tempj;
        end
    end
    if tempi == size(bw,1) && tempj == size(bw,2)
        if bw(tempi-1,tempj) == 0 || bw(tempi,tempj-1) == 0
            count=count+1;
            px(count,1) = tempi;
            px(count,2) = tempj;
        end
    end
    if tempi == size(bw,1) && tempj ~= size(bw,2) && tempj ~= 1
        if bw(tempi-1,tempj) == 0 || bw(tempi,tempj-1) == 0 || bw(tempi,tempj+1) == 0
            count=count+1;
            px(count,1) = tempi;
            px(count,2) = tempj;
        end
    end            
    if tempi ~= 1 && tempi ~= size(bw,1) && tempj ~= size(bw,2) && tempj ~= 1
        if bw(tempi-1,tempj) == 0 || bw(tempi+1,tempj) == 0 || bw(tempi,tempj-1) == 0 || bw(tempi,tempj+1) == 0
            count=count+1;
            px(count,1) = tempi;
            px(count,2) = tempj;
        end
    end            
    if tempi ~= 1 && tempi ~= size(bw,1) && tempj == 1
        if bw(tempi-1,tempj) == 0 || bw(tempi+1,tempj) == 0 || bw(tempi,tempj+1) == 0
            count=count+1;
            px(count,1) = tempi;
            px(count,2) = tempj;
        end
    end      
    if tempi ~= 1 && tempi ~= size(bw,1) && tempj == size(bw,2)
        if bw(tempi-1,tempj) == 0 || bw(tempi+1,tempj) == 0 || bw(tempi,tempj-1) == 0
            count=count+1;
            px(count,1) = tempi;
            px(count,2) = tempj;
        end
    end
end