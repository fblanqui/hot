BEGIN{FS=","}
{if($2=="YES"){hot_yes++}else if($2=="NO"){hot_no++}else hot_maybe++;
if($3=="YES"){wanda_yes++}else if($3=="NO"){wanda_no++}else wanda_maybe++;
if($4=="YES"){thor_yes++}else if($4=="NO"){thor_no++}else thor_maybe++}
END{printf"prover\tALL\tYES\tNO\tMAYBE\tYES+NO\n";
    printf"hot\t%d\t%d\t%d\t%d\t%d\n",hot_yes+hot_no+hot_maybe,
	hot_yes,hot_no,hot_maybe,hot_yes+hot_no;
    printf"wanda\t%d\t%d\t%d\t%d\t%d\n",wanda_yes+wanda_no+wanda_maybe,
	wanda_yes,wanda_no,wanda_maybe,wanda_yes+wanda_no;
    printf"thor\t%d\t%d\t%d\t%d\t%d\n",thor_yes+thor_no+thor_maybe,
	thor_yes,thor_no,thor_maybe,thor_yes+thor_no}
