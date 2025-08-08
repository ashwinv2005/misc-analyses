function [fin xfin] = perdfill(bw,n)
cc = bwconncomp(bw,4);
l = cc.NumObjects;
fin = zeros(l,1);
xfin = zeros(l,1);
parfor i=1:l
    i
    countfin = zeros(10000,2);
    bwtemp = zeros(size(bw));
    bwtemp(cc.PixelIdxList{i}) = 1;
    x=0;
    for j=1:n        
        cc1 = bwconncomp(bwtemp,4);
        ps1 = cellfun(@numel,cc1.PixelIdxList);
        [cod count] = boundpixel(bwtemp,cc1,ps1,1);
        countfin(x+1:x+count,:) = cod(1:count,:);
        for k = 1:count
            bwtemp(cod(k,1),cod(k,2)) = 1;
        end
        if (j~=1)
            x=x+count;
        end
    end
    ct = 0;
    for k = 1:x
        ct = ct + bw(countfin(k,1),countfin(k,2));
    end
    fin(i) = ct/x;
    xfin(i) = x;
end
        
