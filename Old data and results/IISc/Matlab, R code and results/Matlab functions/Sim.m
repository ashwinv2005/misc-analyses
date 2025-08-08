%Power law dispersal
%Distance dependent surival (inverse power law)

pat = randmat(100,100,0.2);
pam = randmat(100,100,0.2);
sp1 = zeros(100);
sp2 = sp1;
imagesc(pat); 
pat(pam == 1) = 2;

%parameters

%power law arrival curve
a = 0.0000001;
c = -0.2;
b = 0.0000001;
d = -0.2;

%heterospecific power law
e = 0.000001;
f = -.1;

%distance dependent predation (exponential)
g = 0.95;
h = -0.05;


for j=1:100
    for k=1:100
        [x y] = find(pat == 1);
        [u v] = find(pat == 2);
        count1 = 0;
        count2 = 0;
        if pat(j,k)~=2
            for l=1:length(x)
                count1 = count1 + a*(1+sqrt((x(l)-j)^2 + (y(l)-k)^2))^c;
            end
        end
        if pat(j,k)~=1
            for l=1:length(u)
                count2 = count2 + b*(1+sqrt((u(l)-j)^2 + (v(l)-k)^2))^d;
            end
        end
        if pat(j,k)==2
            for l=1:length(x)
                count1 = count1 + e*(1+sqrt((x(l)-j)^2 + (y(l)-k)^2))^f;
            end
        end
        if pat(j,k)==1
            for l=1:length(u)
                count2 = count2 + e*(1+sqrt((u(l)-j)^2 + (v(l)-k)^2))^f;
            end
        end
        sur1 = 1 - g*exp(sqrt(min((x-j).^2 + (y-k).^2))*h);
        sur2 = 1 - g*exp(sqrt(min((u-j).^2 + (v-k).^2))*h);
        sp1(j,k) = sur1*count1;
        sp2(j,k) = sur2*count2;
    end
end
sum(sum(sp1))