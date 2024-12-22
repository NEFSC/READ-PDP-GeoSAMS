% Write a string to file. ischar is used as isstring always returns false in octave
function fputc(fid, line)
  if ischar(line)
    fprintf(fid, '%s', line);
  end
end