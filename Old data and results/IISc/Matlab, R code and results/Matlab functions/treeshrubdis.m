function [dis] = treeshrubdis(bw1,bw2)
cct = bwconncomp(bw1,4);
lt = cct.NumObjects;
dis = zeros(lt,1);
ccb = bwconncomp(bw2,4);
lb = ccb.NumObjects;
maxdis = sqrt(size(bw1,1)^2 + size(bw2,2)^2);
parfor i = 1:lt
    i
    bwtemp = bw2;
    bwtemp1 = zeros(size(bw1,1),size(bw1,2));
    bwtemp1(cct.PixelIdxList{i}) = 1;
    for j = 1:maxdis
        ccx = bwconncomp(bwtemp1,4);
        psx = cellfun(@numel,ccx.PixelIdxList);
        bwtemp(ccx.PixelIdxList{1}) = 1;
        cc1 = bwconncomp(bwtemp,4); 
        l1 = cc1.NumObjects;
        if l1 ~= lb + 1
            dis(i) = j-1;
            break
        end
        if l1 == lb + 1
            [cod count1] = boundpixel(bwtemp1,ccx,psx,1);
            for k = 1:count1
                bwtemp1(cod(k,1),cod(k,2)) = 1;
            end
        end
    end
end