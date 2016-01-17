BEGIN {
    FS = ": |, "
}

NR == 1 {
    for (i = 1; i < NF-1; i+=2) {
	if (substr($(i+1), 1, 1) == "(") {
	    printf("%s-min,", $i)
	    printf("%s-max,", $i)
	} else {
	    printf("%s,", $i)
	}
    }
    if (substr($(i+1), 1, 1) == "(") {
	printf("%s-min\n", $i)
	printf("%s-max\n", $i)
    } else {
	printf("%s\n", $i)
    }

    for (i = 2; i < NF; i+=2) {
	if (substr($i, 1, 1) == "(") {
	    len = length($i)
	    $i = substr($i, 2, len - 2) # get text within ( ... )
	}
        printf("%s,", $i)
    }
    printf("%s\n", $NF)
}

NR > 1 {
    for (i = 2; i < NF; i+=2) {
	if (substr($i, 1, 1) == "(") {
	    len = length($i)
	    $i = substr($i, 2, len - 2) # get text within ( ... )
	}
        printf("%s,", $i)
    }
    printf("%s\n", $NF)
}
