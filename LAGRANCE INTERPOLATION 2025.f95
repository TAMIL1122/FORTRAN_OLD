!12345 PROGRAM FOR LAGRANGE'S INTERPOLATION   
    DIMENSION X(20),P(20),F(20)
    WRITE (*,*)'ENTER N'
    READ (*,10)N
10  FORMAT (I5)
    WRITE (*,*) 'ENTER Y'
    READ (*,15)Y
15  FORMAT (I15)
    WRITE (*,*) 'ENTER Xi,Fi'
    DO 20 I=1,N                                                                                                                                                                                                                                                              
    READ (*,25)X(I),F(I)
25  FORMAT(2I15)
20  CONTINUE
    DO 30 K=1,N
    P(K)=1
    DO 35 I=1,N
    IF(I.EQ.K)GOTO 35
    P(K)=P(K)*(Y-X(I))/(X(K)-X(I))
35  CONTINUE 
30  CONTINUE
    FY=0.0
    DO 40 I=1,N
    FY=FY+P(I)*F(I)
40  CONTINUE 
    WRITE (*,45)FY
45  FORMAT(5X,'THE EXPECTED RESULT IS:' F15.5)
    END
  