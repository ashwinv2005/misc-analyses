function [tempbr] = patchenlarge(sizex,sizey,m1,n1)
m2 = [m1,m1+1,m1-1,m1,m1];
n2 = [n1-1,n1,n1,n1+1,n1];
m2(m2 == 0) = 1;
m2(m2 == sizex+1) = sizex;
n2(n2 == 0) = 1;
n2(n2 == sizey+1) = sizey;
temp = sizex*(n2-1)+m2;
tempbr = unique(temp);
end