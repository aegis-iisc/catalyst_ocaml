type  binarytree  = 
        E of int  
        | T of  binarytree * binarytree 

type color = 
        R | G | B

type coloredtree = 
        CT of color
