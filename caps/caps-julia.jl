# run with julia caps-julia.jl
f = open("/tmp/file","r")
s = chars(readall(f))
s[1] = uppercase(s[1])
for i = 1:length(s)-1
  if int(s[i])==10 || int(s[i])==32
    s[i+1] = uppercase(s[i+1])
  end
end
print(CharString(s))
