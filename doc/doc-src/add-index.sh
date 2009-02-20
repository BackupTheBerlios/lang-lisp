
rm ../$1

#Header
echo "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html><head>
   <meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">" >> ../$1
echo $2 "
</head>" >> ../$1


echo "<frameset rows=\"10%,90%\">
  <frame src=\"top.htm\"  noresize=\"noresize\">
  <frameset cols=\"20%,80%\">
    <frame src=\"contents.htm\"  noresize=\"noresize\">
    <frame src=\"" pages/$1 "\" noresize=\"noresize\">
  </frameset>
</frameset>
" >> ../$1

echo "
</html>" >> ../$1


rm ../pages/$1

echo "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html><head>
   <meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">
" >> ../pages/$1
echo $2 "</head><body>
" >> ../pages/$1

cat $1 >> ../pages/$1
cat berlios-banner.htm >> ../pages/$1

echo "
</body></html>" >> ../pages/$1
