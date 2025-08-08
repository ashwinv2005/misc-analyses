function [matpat] = patchsim(bw)
matpat = zeros(size(bw,1),size(bw,2));
cc = bwconncomp(bw,4);
l = cc.NumObjects;
ps = cellfun(@numel,cc.PixelIdxList);
ind = randperm(l);
for i = ind
    i
    temp = cc.PixelIdxList{i};
    m = zeros(ps(i),1);
    n = m;
    for k = 1:ps(i)
        [m(k) n(k)] = convertij(bw,temp(k));
    end
    x1 = min(m);
    x2 = max(m);
    y1 = min(n);
    y2 = max(n);
    h = 0;
    while h==0
        tempx = datasample([1:(x1-1), (x2+1):size(bw,1)],1);
        tempy = datasample([1:(y1-1), (y2+1):size(bw,2)],1);
        if tempx<x1 && tempy<y1
            m1 = m - tempx;
            n1 = n - tempy;
            temp = size(bw,1)*(n1-1)+m1;
        end
        if tempx<x1 && tempy>y2
            m1 = m - tempx;
            n1 = n + tempy - y2;
            temp = size(bw,1)*(n1-1)+m1;
        end
        if tempx>x2 && tempy<y1
            m1 = m + tempx - x2;
            n1 = n - tempy;
            temp = size(bw,1)*(n1-1)+m1;
        end
        if tempx>x2 && tempy>y2
            m1 = m + tempx - x2;
            n1 = n + tempy - y2;
            temp = size(bw,1)*(n1-1)+m1;
        end
        tempbr = patchenlarge(size(bw,1),size(bw,2),m1,n1);
        if (matpat(tempbr) == 0)
            h = 1;
        end
    end
    matpat(temp) = 1;
end