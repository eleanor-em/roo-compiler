file=$1
out=$(echo $file | sed -e s/\.roo/\.oz/g)
./Roo $file > $out && oz/oz $out
