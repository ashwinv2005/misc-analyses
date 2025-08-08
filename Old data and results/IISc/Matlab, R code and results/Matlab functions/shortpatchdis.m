function [dis] = shortpatchdis(bw)
cc = bwconncomp(bw,4);
l = cc.NumObjects;
dis = zeros(l,1);
maxdis = sqrt(size(bw,1)^2 + size(bw,2)^2);
parfor i = 1:l
    i
    bwtemp = bw;
    for j = 1:maxdis
        cc1 = bwconncomp(bwtemp,4); 
        ps1 = cellfun(@numel,cc1.PixelIdxList);
        l1 = cc1.NumObjects;
        if l1 ~= l
            dis(i) = j-1;
            break
        end
        if l1 == l
            [cod count1] = boundpixel(bwtemp,cc1,ps1,i);
            for k = 1:count1
                bwtemp(cod(k,1),cod(k,2)) = 1;
            end
        end
    end
end
        
        
        