function [bw] = connpatch(bw,z)
for n=1:z
    for i=1:size(bw,1)
        for j=1:size(bw,2)
            if bw(i,j) == 1
                bw = fillloop(bw,n,i,j);
            end
        end
    end
end                   
                        