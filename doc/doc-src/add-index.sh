
rm ../$1

#Header
echo "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html><head>
   <meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">
" >> ../$1
echo $2 "</head><body>" >> ../$1

#Table with banner, menu and the document
echo "<table><tr height=10%>" >> ../$1

cat top.htm >> ../$1

echo "</tr><tr height=90%><table><td width=20% valign=\"top\">" >> ../$1

cat contents.htm >> ../$1

echo "</td><td width=80%>" >> ../$1

cat $1 >> ../$1
cat berlios-banner.htm >> ../$1

echo "</td></table></tr></table></body></html>" >> ../$1
