function[cont freq ubin] = listtohist(psize)
ubin = sort(unique(psize));
lpar = length(psize);

len = length(min(ubin):max(ubin));
cont = zeros(len,2);
cont(:,2) = min(ubin):max(ubin);

for i=1:lpar
  for j=1:len  
    if psize(i) == cont(j,2)
      cont(j,1) = cont(j,1) + 1;
    end
  end
end
freq = cont(cont(:,1) ~= 0,1);
