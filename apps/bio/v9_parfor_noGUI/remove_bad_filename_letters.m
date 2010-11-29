function filename = remove_bad_filename_letters(filename)
filename = filename((filename>=48 & filename<=57) | (filename>=65 & filename<=90) | (filename>=97 & filename<=122) | (filename=='_'));
return
