s = textread('/tmp/file','%s','delimiter','\n');
cs = regexprep(s,'(\<\w)', '${upper($1)}');
f = fopen('/tmp/file_caps','w')
fprintf(f,'%s\n',cs{:});

